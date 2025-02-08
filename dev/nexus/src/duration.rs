use core::fmt;

pub struct NexusDuration {
    duration: std::time::Duration,
}

impl NexusDuration {
    pub fn new(duration: std::time::Duration) -> Self {
        let duration = std::time::Duration::from_millis(duration.as_millis().try_into().unwrap());
        Self { duration }
    }

    pub fn since(start: std::time::Instant) -> Self {
        Self::new(start.elapsed())
    }
}

impl From<std::time::Duration> for NexusDuration {
    fn from(duration: std::time::Duration) -> Self {
        Self::new(duration)
    }
}

impl fmt::Display for NexusDuration {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        humantime::Duration::from(self.duration).fmt(f)
    }
}
