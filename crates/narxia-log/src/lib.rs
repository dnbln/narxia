pub use tracing::{debug, error, info, trace, warn, span, Level};

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
