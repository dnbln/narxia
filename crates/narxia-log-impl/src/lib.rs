use std::fmt::{Debug, Display};
use std::{fmt, io};

use colored::{ColoredString, Colorize};
use tracing::field::{Field, Visit};
use tracing::span::Attributes;
use tracing::{Event, Id, Level, Metadata, Subscriber};
use tracing_subscriber::fmt::MakeWriter;
use tracing_subscriber::layer::{Context, SubscriberExt};
use tracing_subscriber::registry::{LookupSpan, SpanRef};
use tracing_subscriber::util::SubscriberInitExt;
use tracing_subscriber::Layer;

struct NarxiaLayerConfig {
    min_level: Level,
    max_level: Level,
    ignore_outside_logging: bool,
}

impl Default for NarxiaLayerConfig {
    fn default() -> Self {
        Self {
            max_level: if cfg!(debug_assertions) {
                Level::TRACE
            } else {
                Level::INFO
            },
            min_level: Level::ERROR,
            ignore_outside_logging: true,
        }
    }
}

struct NarxiaLayer<'a, W>
where
    W: MakeWriter<'a>,
    W::Writer: 'static,
{
    make_writer: W,
    cfg: NarxiaLayerConfig,
    _pd: std::marker::PhantomData<&'a ()>,
}

enum MetadataKind {
    Span,
    Event,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct PrintExtra(bool);

impl<W> NarxiaLayer<'static, W>
where
    W: for<'a> MakeWriter<'a> + 'static,
    for<'a> <W as MakeWriter<'a>>::Writer: 'static,
{
    fn new(make_writer: W, cfg: NarxiaLayerConfig) -> Self {
        Self {
            make_writer,
            cfg,
            _pd: std::marker::PhantomData,
        }
    }

    fn should_pass_event(&self, event: &Event<'_>) -> bool {
        // this comparison should be inverted
        // ----
        // the more verbose levels are supposed to compare greater than the less verbose ones
        // error is supposed to be the lowest
        //
        // <https://github.com/tokio-rs/tracing/blob/6b272c6c4ed02bef9c8409b8fbe0db6bc87abc12/tracing-core/src/metadata.rs#L623-L650>
        let l = event.metadata().level();

        &self.cfg.min_level <= l && l <= &self.cfg.max_level
    }

    fn style_field<Wr>(
        &self,
        w: &mut Wr,
        name: &str,
        f: impl FnOnce(&mut Wr) -> io::Result<()>,
    ) -> io::Result<()>
    where
        Wr: io::Write + ?Sized,
    {
        write!(w, "    {} ", name.dimmed().yellow())?;
        f(w)?;
        writeln!(w)
    }

    fn format_metadata(
        &self,
        w: &mut impl io::Write,
        metadata_kind: MetadataKind,
        md: &Metadata,
    ) -> io::Result<()> {
        match metadata_kind {
            MetadataKind::Span => {
                self.style_field(w, "in", |w| {
                    write!(
                        w,
                        "{}::{}",
                        md.module_path().unwrap().blue(),
                        md.name().white().bold().italic(),
                    )
                })?;
            }
            MetadataKind::Event => {
                if let Some(mod_path) = md.module_path() {
                    self.style_field(w, "in", |w| write!(w, "{}", mod_path.blue()))?;
                }
            }
        }

        if let Some(file_path) = md.file() {
            self.style_field(w, "at", |w| {
                write!(w, "{}", file_path.dimmed().bright_white())?;
                if let Some(file_line) = md.line() {
                    write!(w, ":{}", file_line.to_string().bright_white())?;
                }
                Ok(())
            })?;
        }

        Ok(())
    }

    fn format_event_data(
        &self,
        w: &mut impl io::Write,
        event: &Event<'_>,
    ) -> io::Result<PrintExtra> {
        if self.cfg.ignore_outside_logging
            && event.metadata().file().is_none()
            && event.metadata().module_path().is_none()
        {
            return Ok(PrintExtra(false));
        }

        struct Visitor {
            message: Option<String>,
            short: bool,
        }
        let mut vis = Visitor {
            message: None,
            short: false,
        };

        impl Visit for Visitor {
            fn record_bool(&mut self, field: &Field, value: bool) {
                match field.name() {
                    "short" => {
                        self.short = value;
                    }
                    _ => {}
                }
            }
            fn record_debug(&mut self, field: &Field, value: &dyn Debug) {
                match field.name() {
                    "message" => {
                        self.message = Some(format!("{value:?}"));
                    }
                    _ => {}
                }
            }
        }

        event.record(&mut vis);
        let Visitor { message, short } = vis;

        let level = event.metadata().level();

        let (name, style_color): (&str, fn(String) -> ColoredString) = match level {
            &Level::TRACE => ("TRACE", |s| s.bright_white()),
            &Level::DEBUG => ("DEBUG", |s| s.bright_blue()),
            &Level::INFO => ("INFO", |s| s.green()),
            &Level::WARN => ("WARN", |s| s.yellow()),
            &Level::ERROR => ("ERROR", |s| s.red()),
        };

        write!(w, ">>> {}:", style_color(name.to_owned()).bold())?;

        if let Some(message) = message {
            if message.contains('\n') {
                writeln!(w)?; // put it on a new line
            } else {
                write!(w, " ")?;
            }
            write!(w, "{}", style_color(message))?;
        }

        writeln!(w)?;

        if !short {
            self.format_metadata(w, MetadataKind::Event, event.metadata())?;
        }

        Ok(PrintExtra(!short))
    }

    fn format_span_line<'scope, S>(
        &self,
        w: &mut impl io::Write,
        span: SpanRef<'scope, S>,
    ) -> io::Result<()>
    where
        S: Subscriber + for<'span> LookupSpan<'span> + fmt::Debug,
    {
        let md = span.metadata();

        self.format_metadata(w, MetadataKind::Span, md)?;

        Ok(())
    }

    fn format_event_and_write<S>(&self, event: &Event<'_>, ctx: Context<'_, S>) -> io::Result<()>
    where
        S: Subscriber + for<'span> LookupSpan<'span> + fmt::Debug,
    {
        let w = self.make_writer.make_writer();
        let mut writer = io::BufWriter::with_capacity(4096, w);

        let print_extra = self.format_event_data(&mut writer, event)?;
        if print_extra.0 {
            let scope = ctx.event_scope(event).unwrap();
            for span in scope {
                self.format_span_line(&mut writer, span)?;
            }
        }

        Ok(())
    }
}

impl<S, W> Layer<S> for NarxiaLayer<'static, W>
where
    S: Subscriber + for<'span> LookupSpan<'span> + fmt::Debug,
    W: for<'a> MakeWriter<'a> + 'static,
    for<'a> <W as MakeWriter<'a>>::Writer: 'static,
{
    fn on_new_span(&self, attrs: &Attributes, id: &Id, ctx: Context<S>) {}

    fn on_event(&self, event: &Event<'_>, ctx: Context<'_, S>) {
        if !self.should_pass_event(event) {
            return;
        }

        self.format_event_and_write(event, ctx).unwrap();
    }

    fn on_enter(&self, id: &Id, ctx: Context<S>) {}
}

pub fn init() {
    tracing_subscriber::Registry::default()
        .with(NarxiaLayer::new(io::stderr, NarxiaLayerConfig::default()))
        // .with(
        //     tracing_subscriber::fmt::layer()
        //         .pretty()
        //         .with_file(true)
        //         .with_line_number(true)
        //         .with_ansi(true),
        // )
        .init();
}
