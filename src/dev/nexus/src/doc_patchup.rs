use std::fmt::Write;
use std::fs;
use std::future::poll_fn;
use std::io;
use std::net::Ipv4Addr;
use std::net::SocketAddr;
use std::net::SocketAddrV4;
use std::path::Path;
use std::pin::pin;
use std::task::Poll;

use doc_extract::Markdown;
use doc_extract::Session;
use miette::IntoDiagnostic;
use tokio::io as tokio_io;
use tokio::io::AsyncReadExt;
use tokio::io::AsyncWriteExt;
use tokio::net;
use tokio::net::TcpListener;
use tokio::process::Child;
use tokio::process::ChildStdin;
use tokio::process::ChildStdout;
use tokio::signal;

use crate::NexusR;

pub struct Code {
    pub before: String,
    pub after: String,
    pub write_doctests: bool,
    pub all_code_for_doctests: String,
}

pub enum SessionWrapper {
    Child(Session<ChildStdout, ChildStdin>),
    Net(Session<net::tcp::OwnedReadHalf, net::tcp::OwnedWriteHalf>),
}

impl SessionWrapper {
    pub async fn query_symbol(&self, symbol: &str) -> Result<Markdown, String> {
        match self {
            SessionWrapper::Child(session) => {
                // Query the child process
                session.query_symbol(symbol).await
            }
            SessionWrapper::Net(session) => {
                // Query the network session
                session.query_symbol(symbol).await
            }
        }
    }

    pub async fn attempt_to_connect_daemon_or_spawn_child(
        root: impl AsRef<Path>,
    ) -> NexusR<(Option<Child>, SessionWrapper)> {
        match Self::make_net(&root).await {
            Ok(session) => Ok((None, session)),
            Err(e) => {
                eprintln!("Failed to connect to daemon: {e}.\nSpawning child process.");
                let (child, session) = Self::make_child(root).await;
                Ok((Some(child), session))
            }
        }
    }

    pub async fn make_child(root: impl AsRef<Path>) -> (Child, SessionWrapper) {
        let (child, session) = Session::spawn_child(root).await;
        (child, SessionWrapper::Child(session))
    }

    const LOCAL_ADDR: SocketAddr =
        SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::new(127, 0, 0, 1), 15192));

    pub async fn make_net(root: impl AsRef<Path>) -> NexusR<SessionWrapper> {
        eprintln!("Attempting to connect to doc-extract daemon...");
        let tcp_stream = net::TcpSocket::connect(
            net::TcpSocket::new_v4().into_diagnostic()?,
            Self::LOCAL_ADDR,
        )
        .await
        .into_diagnostic()?;
        eprintln!("Connected to doc-extract daemon.");
        let (mut read_half, write_half) = tcp_stream.into_split();

        let first_byte = read_half.read_u8().await.into_diagnostic()?;
        let initialize = first_byte == b'1';

        let session = Session::new(write_half, read_half).await;
        if initialize {
            session.initialize(root).await;
        }
        Ok(SessionWrapper::Net(session))
    }

    pub async fn run_server() -> NexusR {
        let mut child = doc_extract::prepare_command();
        let mut child_stdin = child.stdin.take().expect("Failed to take stdin");
        let mut child_stdout = child.stdout.take().expect("Failed to take stdout");

        let listener = TcpListener::bind(Self::LOCAL_ADDR)
            .await
            .into_diagnostic()?;

        let mut first = true;
        let mut ctrlc = pin!(signal::ctrl_c());

        loop {
            enum CtrlCOrResult<R> {
                CtrlC(io::Result<()>),
                Result(R),
            }

            let mut list_accept = pin!(listener.accept());

            let r = poll_fn(|cx| {
                if let Poll::Ready(r) = ctrlc.as_mut().poll(cx) {
                    return Poll::Ready(CtrlCOrResult::CtrlC(r));
                }

                list_accept.as_mut().poll(cx).map(CtrlCOrResult::Result)
            })
            .await;

            let mut socket = match r {
                CtrlCOrResult::CtrlC(Ok(())) => {
                    // Gracefully exit
                    break;
                }
                CtrlCOrResult::CtrlC(Err(e)) => {
                    eprintln!("Error occurred while waiting for Ctrl-C: {}", e);
                    break;
                }
                CtrlCOrResult::Result(Ok((socket, _))) => socket,
                CtrlCOrResult::Result(Err(e)) => {
                    eprintln!("Error occurred while accepting connection: {}", e);
                    continue;
                }
            };

            socket
                .write_all(if first { b"1" } else { b"0" })
                .await
                .into_diagnostic()?;
            first = false;

            let (mut rh, mut wh) = socket.split();

            let mut copy_rh_to_stdin = pin!(tokio_io::copy(&mut rh, &mut child_stdin));
            let mut copy_stdout_to_wh = pin!(tokio_io::copy(&mut child_stdout, &mut wh));

            let res = poll_fn(|cx| {
                if let Poll::Ready(a) = ctrlc.as_mut().poll(cx) {
                    return Poll::Ready(CtrlCOrResult::CtrlC(a));
                }

                if let Poll::Ready(r) = copy_stdout_to_wh.as_mut().poll(cx) {
                    return Poll::Ready(CtrlCOrResult::Result(Err(io::Error::other(
                        "Unexpected early EOF from rust-analyzer stdout",
                    ))));
                }

                copy_rh_to_stdin
                    .as_mut()
                    .poll(cx)
                    .map(CtrlCOrResult::Result)
            })
            .await;

            match res {
                CtrlCOrResult::CtrlC(Ok(())) => {
                    // Gracefully exit
                    break;
                }
                CtrlCOrResult::CtrlC(Err(e)) => {
                    eprintln!("Error occurred while waiting for Ctrl-C: {}", e);
                    break;
                }
                CtrlCOrResult::Result(Err(e)) => {
                    eprintln!("Error occurred while copying data: {}", e);
                }
                CtrlCOrResult::Result(Ok(_)) => {
                    // Successfully copied data in both directions
                }
            }
        }

        if child.try_wait().is_ok() {
            return Ok(());
        }

        let session = Session::new(child_stdin, child_stdout).await;
        session.shutdown().await;

        Ok(())
    }
}

pub async fn patchup_doc(session: &SessionWrapper, before: String) -> Code {
    let mut after = String::new();
    let mut all_code_for_doctests = String::new();

    let mut current_doctest = String::new();
    let mut current_doctest_req_features = Vec::new();

    let mut previous_r = false;
    let mut in_doc_tooltip = false;
    let mut write_doctests = false;
    let mut in_code_block = false;

    for line in before.lines() {
        if let Some(l) = line.strip_prefix("## !!doctooltips ") {
            // Process the line
            let l = l.trim();
            if l.ends_with("-R") {
                previous_r = true;
                writeln!(&mut after, "{line}").unwrap();
                continue;
            }
            let docs = session
                .query_symbol(l)
                .await
                .expect("Failed to extract docs");

            writeln!(&mut after, "{line}").unwrap();
            writeln!(&mut after).unwrap();
            writeln!(&mut after, "<DocTooltip>").unwrap();
            writeln!(&mut after, "{}", patch_rust_lines(docs.contents)).unwrap();
            writeln!(&mut after, "</DocTooltip>").unwrap();
        } else if previous_r {
            let l = line
                .trim()
                .strip_prefix("<TooltipTarget>`")
                .expect(r#"Expected the "<TooltipTarget>`" prefix"#)
                .strip_suffix("`</TooltipTarget>")
                .expect(r#"Expected the "`</TooltipTarget>" suffix"#);

            let docs = session
                .query_symbol(l)
                .await
                .expect("Failed to extract docs");

            writeln!(&mut after, "{line}").unwrap();
            writeln!(&mut after).unwrap();
            writeln!(&mut after, "<DocTooltip>").unwrap();
            writeln!(&mut after, "{}", patch_rust_lines(docs.contents)).unwrap();
            writeln!(&mut after, "</DocTooltip>").unwrap();

            previous_r = false;
        } else if line == "<DocTooltip>" {
            if after.ends_with("\n\n") {
                after.pop();
            }
            in_doc_tooltip = true;
        } else if line == "</DocTooltip>" {
            in_doc_tooltip = false;
        } else if in_doc_tooltip {
            // Skip lines inside the doctooltips block
            continue;
        } else {
            writeln!(&mut after, "{line}").unwrap();
            if line == "```rust" {
                current_doctest.clear();
                current_doctest_req_features.clear();
                writeln!(&mut current_doctest, "```rust").unwrap();
                in_code_block = true;
            } else if line.starts_with("```rust !") {
                current_doctest.clear();
                current_doctest_req_features.clear();
                in_code_block = true;
                let tag = if line.ends_with("no_run") {
                    write_doctests = true;
                    "no_run"
                } else if line.ends_with("ignore") {
                    write_doctests = true;
                    "ignore"
                } else if line.ends_with("should_panic") {
                    write_doctests = true;
                    "should_panic"
                } else if line.ends_with("compile_fail") {
                    write_doctests = true;
                    "compile_fail"
                } else {
                    writeln!(&mut current_doctest, "```rust").unwrap();
                    continue;
                };

                writeln!(&mut current_doctest, "```rust,{tag}").unwrap();
            } else if line == "```" {
                in_code_block = !in_code_block;
                if in_code_block {
                    current_doctest.clear();
                    current_doctest_req_features.clear();
                    writeln!(&mut current_doctest, "{line}").unwrap();
                } else {
                    writeln!(&mut current_doctest, "{line}").unwrap();
                    if current_doctest_req_features.is_empty() {
                        for line in current_doctest.lines() {
                            writeln!(&mut all_code_for_doctests, "/// {line}").unwrap();
                        }
                    } else {
                        write!(&mut all_code_for_doctests, "#[cfg_attr(all(").unwrap();
                        for feature in &current_doctest_req_features {
                            write!(&mut all_code_for_doctests, "feature = {feature:?}, ").unwrap();
                        }
                        writeln!(
                            &mut all_code_for_doctests,
                            "), doc = r##########\"{current_doctest}\"##########)]"
                        )
                        .unwrap();
                    }
                }
            } else if in_code_block {
                if let Some(tail) = line.strip_prefix("// !tail ") {
                    write_doctests = true;
                    writeln!(&mut current_doctest, "Ok::<_, {tail}>(())").unwrap();
                } else if let Some(hidden) = line.strip_prefix("// !hidden ") {
                    write_doctests = true;
                    writeln!(&mut current_doctest, "{hidden}").unwrap();
                } else if let Some(feature) = line.strip_prefix("// !req-feature ") {
                    write_doctests = true;
                    current_doctest_req_features.push(feature.trim().to_owned());
                } else if line == "// !lints" {
                    writeln!(
                        &mut current_doctest,
                        "{}",
                        r#"
#![deny(unused_imports)]
"#
                        .trim_start()
                    )
                    .unwrap();
                } else {
                    writeln!(&mut current_doctest, "{line}").unwrap();
                }
            } else {
                writeln!(&mut all_code_for_doctests, "/// {line}").unwrap();
            }
        }
    }

    writeln!(&mut all_code_for_doctests, "struct Guide;").unwrap();

    Code {
        before,
        after,
        write_doctests,
        all_code_for_doctests,
    }
}

fn patch_rust_lines(contents: String) -> String {
    contents
        .lines()
        .map(|line| {
            if line.starts_with("```rust") {
                "```rust ,ignore".to_owned()
            } else if line.starts_with("```") {
                "```".to_owned()
            } else {
                line.to_owned()
            }
        })
        .collect::<Vec<_>>()
        .join("\n")
}

#[derive(Debug, thiserror::Error)]
pub enum PerformEndError {
    #[error("IO error: {0}")]
    IO(#[from] io::Error),
    #[error("Changes detected")]
    ChangesDetected,
}

pub fn perform_end(code: &Code, p: &Path, check_mode: bool) -> Result<(), PerformEndError> {
    if code.write_doctests {
        let name = p.file_name().unwrap().to_str().unwrap();
        let new_name = format!(".{name}.doctests");
        fs::write(p.with_file_name(new_name), &code.all_code_for_doctests)?;
    }

    let before = match fs::read_to_string(p) {
        Ok(b) => Some(b),
        Err(e) if e.kind() == io::ErrorKind::NotFound => None,
        Err(e) => {
            eprintln!("Error reading file: {}", e);
            return Err(PerformEndError::IO(e));
        }
    };

    if check_mode {
        if before.as_ref() == Some(&code.after) {
            return Ok(());
        } else {
            return Err(PerformEndError::ChangesDetected);
        }
    } else if before.as_ref() != Some(&code.after) {
        let new_path = p.with_extension("before.mdx");
        match fs::rename(p, &new_path) {
            Ok(_) => {}
            Err(e) if e.kind() == io::ErrorKind::NotFound => {
                // If the file doesn't exist, we will just write the new one
            }
            Err(e) => {
                return Err(PerformEndError::IO(e));
            }
        };
        fs::write(p, &code.after)?;
    } else {
        return Ok(());
    }

    Ok(())
}
