pub use tracing::Level;
pub use tracing::debug;
pub use tracing::error;
pub use tracing::info;
pub use tracing::span;
pub use tracing::trace;
pub use tracing::warn;

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
