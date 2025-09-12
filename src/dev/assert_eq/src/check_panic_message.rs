use std::panic::{catch_unwind, resume_unwind, UnwindSafe};

pub fn check_panic_message(func: impl FnOnce() + UnwindSafe, msg: &'static str) {
    let chk = |panic_msg: &'_ str| panic_msg.contains(msg);
    if let Err(err) = catch_unwind(func) {
        let resume_panicking = if let Some(s) = err.downcast_ref::<String>() {
            chk(&*s)
        } else if let Some(s) = err.downcast_ref::<&'static str>() {
            chk(*s)
        } else {
            false
        };

        if resume_panicking {
            resume_unwind(err)
        }
    }
}
