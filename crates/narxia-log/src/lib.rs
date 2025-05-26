pub use tracing::Level;
pub use tracing::debug;
pub use tracing::debug_span;
pub use tracing::error;
pub use tracing::error_span;
pub use tracing::info;
pub use tracing::info_span;
pub use tracing::span;
pub use tracing::trace;
pub use tracing::trace_span;
pub use tracing::warn;
pub use tracing::warn_span;

#[macro_export]
macro_rules! d {
    ($($args:tt)*) => {
        $crate::debug!(short = true, $($args)*)
    };
}

#[macro_export]
macro_rules! e {
    ($($args:tt)*) => {
        $crate::error!(short = true, $($args)*)
    };
}

#[macro_export]
macro_rules! i {
    ($($args:tt)*) => {
        $crate::info!(short = true, $($args)*)
    };
}

#[macro_export]
macro_rules! t {
    ($($args:tt)*) => {
        $crate::trace!(short = true, $($args)*)
    };
}

#[macro_export]
macro_rules! w {
    ($($args:tt)*) => {
        $crate::warn!(short = true, $($args)*)
    };
}

#[macro_export]
macro_rules! etrace_span {
    ($($args:tt)*) => {
        $crate::trace_span!($($args)*).entered()
    };
}

#[macro_export]
macro_rules! edebug_span {
    ($($args:tt)*) => {
        $crate::debug_span!($($args)*).entered()
    };
}

#[macro_export]
macro_rules! einfo_span {
    ($($args:tt)*) => {
        $crate::info_span!($($args)*).entered()
    };
}

#[macro_export]
macro_rules! ewarn_span {
    ($($args:tt)*) => {
        $crate::warn_span!($($args)*).entered()
    };
}

#[macro_export]
macro_rules! eerror_span {
    ($($args:tt)*) => {
        $crate::error_span!($($args)*).entered()
    };
}
