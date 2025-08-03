use std::path::Path;
use std::process;
use std::process::Stdio;
use std::str::FromStr;
use std::time::Duration;

use client::start_language_server;
use lsp_client::lsp::client;
use lsp_types::ClientCapabilities;
use lsp_types::Hover;
use lsp_types::HoverContents;
use lsp_types::HoverParams;
use lsp_types::InitializeParams;
use lsp_types::MarkupContent;
use lsp_types::TextDocumentClientCapabilities;
use lsp_types::Uri;
use lsp_types::WorkDoneProgressParams;
use lsp_types::WorkspaceSymbolParams;
use lsp_types::WorkspaceSymbolResponse;
use lsp_types::notification::Exit;
use lsp_types::notification::Initialized;
use lsp_types::notification::Notification;
use lsp_types::request::HoverRequest;
use lsp_types::request::Initialize;
use lsp_types::request::Request;
use lsp_types::request::Shutdown;
use lsp_types::request::WorkspaceSymbolRequest;
use serde_json::json;
use tokio::process::Child;
use tokio::process::ChildStdin;
use tokio::process::Command;
use tokio::sync::oneshot;
use tokio::time;

pub struct Markdown {
    pub contents: String,
}

pub struct Session {
    #[expect(unused)]
    child: Child,
    lang_server: client::LanguageServerRef<ChildStdin>,
}

impl Session {
    pub async fn new(root: impl AsRef<Path>) -> Self {
        let (child, lang_server) = start_language_server(prepare_command()).await;
        let root = root.as_ref();
        let root_url = format!("file://{}", root.display());
        let root_uri = Uri::from_str(&root_url).unwrap_or_else(|err| {
            panic!(
                "Failed converting directory name {} into a Url: {err}",
                root.display()
            )
        });

        lang_server
            .s_request::<Initialize, _>(
                &InitializeParams {
                    workspace_folders: Some(vec![lsp_types::WorkspaceFolder {
                        uri: root_uri,
                        name: root
                            .file_name()
                            .and_then(|name| name.to_str())
                            .unwrap_or("unknown")
                            .to_string(),
                    }]),
                    capabilities: ClientCapabilities {
                        text_document: Some(TextDocumentClientCapabilities {
                            hover: Some(lsp_types::HoverClientCapabilities {
                                dynamic_registration: Some(false),
                                content_format: Some(vec![lsp_types::MarkupKind::Markdown]),
                            }),
                            ..Default::default()
                        }),
                        ..Default::default()
                    },
                    ..Default::default()
                },
                |_| {},
            )
            .await;

        lang_server
            .send_notification(Initialized::METHOD, &json!({}))
            .await;

        Session { child, lang_server }
    }

    pub async fn query_symbol(&self, symbol: &str) -> Result<Markdown, String> {
        eprintln!("Querying symbol: {symbol}");
        let symb = loop {
            let (tx, rx) = oneshot::channel();

            // need # at the end of the target name to search for functions, by default rust-analyzer
            // searches for types, so we append # to the end of the target name.

            // if there are multiple symbols with the same name, we will try to find the one
            // that matches the container name, if it exists.
            // if no container name is provided, we will panic if we get multiple symbols
            let (term_name, ends_with_hash) = symbol
                .strip_suffix('#')
                .map_or((symbol, false), |f| (f, true));
            let (root_name, term_name) = match term_name.split_once("::") {
                Some((root, term)) => (Some(root), term),
                None => (None, term_name),
            };

            self.lang_server
                .s_request::<WorkspaceSymbolRequest, _>(
                    &WorkspaceSymbolParams {
                        query: format!("{term_name}{}", if ends_with_hash { "#" } else { "" }),
                        ..Default::default()
                    },
                    |result| {
                        tx.send(result).expect("unable to send to receiver");
                    },
                )
                .await;
            let Some(WorkspaceSymbolResponse::Flat(symbols)) = rx.await.unwrap().unwrap() else {
                time::sleep(Duration::from_secs(1)).await;
                continue;
            };

            if symbols.is_empty() {
                // eprintln!("No symbols found");
                time::sleep(Duration::from_secs(1)).await;
                continue;
            }

            let symbset = symbols
                .iter()
                .filter(|s| s.name == term_name)
                .collect::<Vec<_>>();

            if symbset.is_empty() {
                eprintln!("No symbol found with name: {term_name}");
                eprintln!("Available symbols:");
                for symbol in &symbols {
                    eprintln!(" - {} @ {:?}", symbol.name, symbol.location);
                }
                process::exit(1);
            };

            let symb =
                symbset
                    .iter()
                    .find(|s| s.container_name.as_deref() == root_name)
                    .cloned()
                    .ok_or_else(|| {
                        eprintln!(
                            "No symbol found with name: {term_name} in container: {root_name:?}",
                        );
                        eprintln!("Available symbols:");
                        for symbol in &symbset {
                            eprintln!(
                                " - {} @ {:?} {}",
                                symbol.name,
                                symbol.location,
                                if let Some(c) = &symbol.container_name {
                                    format!("(in container {c})")
                                } else {
                                    String::new()
                                }
                            );
                        }
                        format!(
                            "No symbol found with name: {term_name} in container: {root_name:?}"
                        )
                    })?;

            // eprintln!(
            //     "Found symbol: {} @ {:?}:{:?}",
            //     symb.name, symb.location.uri, symb.location.range.start
            // );

            break symb.clone();
        };

        let (tx, rx) = oneshot::channel();
        self.lang_server
            .s_request::<HoverRequest, _>(
                &HoverParams {
                    text_document_position_params: lsp_types::TextDocumentPositionParams {
                        text_document: lsp_types::TextDocumentIdentifier {
                            uri: symb.location.uri.clone(),
                        },
                        position: symb.location.range.start,
                    },
                    work_done_progress_params: WorkDoneProgressParams::default(),
                },
                |result| {
                    tx.send(result).expect("unable to send to receiver");
                },
            )
            .await;

        let hover = rx.await.unwrap().unwrap();
        let Some(Hover {
            contents:
                HoverContents::Markup(MarkupContent {
                    kind: lsp_types::MarkupKind::Markdown,
                    value: contents,
                }),
            ..
        }) = hover
        else {
            panic!("Expected hover contents to be Markdown, got: {hover:?}");
        };

        Ok(Markdown { contents })
    }

    pub async fn shutdown(&self) {
        self.lang_server
            .send_notification(Exit::METHOD, &json!({}))
            .await;
        self.lang_server.s_request::<Shutdown, _>(&(), |_| {}).await;
    }
}

trait DoSendRequest {
    async fn s_request<RT: Request, CB>(&self, params: &RT::Params, completion: CB)
    where
        CB: 'static + Send + FnOnce(Result<RT::Result, serde_json::Value>);

    // async fn s_request_with_extra_params<RT: Request, CB>(
    //     &self,
    //     params: &RT::Params,
    //     extra_params: serde_json::Value,
    //     completion: CB,
    // ) where
    //     CB: 'static + Send + FnOnce(Result<RT::Result, serde_json::Value>);
}

impl DoSendRequest for client::LanguageServerRef<ChildStdin> {
    async fn s_request<RT: Request, CB>(&self, params: &RT::Params, completion: CB)
    where
        CB: 'static + Send + FnOnce(Result<RT::Result, serde_json::Value>),
    {
        self.send_request(RT::METHOD, &json!(params), |r| {
            completion(r.and_then(|v| {
                serde_json::from_value(v).map_err(|e| {
                    serde_json::Value::String(format!(
                        "Failed to deserialize response for {}: {e}",
                        RT::METHOD
                    ))
                })
            }))
        })
        .await;
    }

    // async fn s_request_with_extra_params<RT: Request, CB>(
    //     &self,
    //     params: &RT::Params,
    //     extra_params: serde_json::Value,
    //     completion: CB,
    // ) where
    //     CB: 'static + Send + FnOnce(Result<RT::Result, serde_json::Value>),
    // {
    //     self.send_request(
    //         RT::METHOD,
    //         &(match (json!(params), extra_params) {
    //             (serde_json::Value::Object(mut map), serde_json::Value::Object(extra)) => {
    //                 map.extend(extra.clone());
    //                 serde_json::Value::Object(map)
    //             }
    //             (a, b) => panic!("Expected params / extra to be an object, got {a:?} and {b:?}"),
    //         }),
    //         |r| {
    //             completion(r.and_then(|v| {
    //                 serde_json::from_value(v).map_err(|e| {
    //                     serde_json::Value::String(format!(
    //                         "Failed to deserialize response for {}: {e}",
    //                         RT::METHOD
    //                     ))
    //                 })
    //             }))
    //         },
    //     )
    //     .await;
    // }
}

fn prepare_command() -> Child {
    Command::new("rust-analyzer")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("Failed to start rust-analyzer process")
}
