use super::*;

pub struct StreamingFileWriter {
    f: File,
}

impl StreamingFileWriter {
    pub fn new(path: &Path) -> Result<Self> {
        if let Some(parent) = path.parent() {
            if !parent.exists() {
                std::fs::create_dir_all(parent).wrap_io_error_with(parent)?;
            }
        }
        let f = File::create(path).wrap_io_error_with(path)?;
        Ok(Self { f })
    }
}

impl std::io::Write for StreamingFileWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        <File as std::io::Write>::write(&mut self.f, buf)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.f.flush()
    }
}

impl std::fmt::Write for StreamingFileWriter {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        use std::io::Write;

        self.f
            .write_all(s.as_bytes())
            .map_err(|_| std::fmt::Error)?;
        Ok(())
    }
}
