use std::fmt;
use std::time;

pub struct NexusDuration {
    duration: time::Duration,
}

impl NexusDuration {
    pub fn new(duration: time::Duration) -> Self {
        let duration = time::Duration::from_millis(duration.as_millis().try_into().unwrap());
        Self { duration }
    }

    pub fn since(start: time::Instant) -> Self {
        Self::new(start.elapsed())
    }
}

impl From<time::Duration> for NexusDuration {
    fn from(duration: time::Duration) -> Self {
        Self::new(duration)
    }
}

impl fmt::Display for NexusDuration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        humantime::Duration::from(self.duration).fmt(f)
    }
}
