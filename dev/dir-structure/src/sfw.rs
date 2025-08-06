use super::*;

pub struct StreamingFileWriter {
    f: File,
}

impl StreamingFileWriter {
    pub fn new(path: &Path) -> Result<Self> {
        utils::create_parent_dir(path)?;
        let f = File::create(path).wrap_io_error_with(path)?;
        Ok(Self { f })
    }
}

impl std::io::Write for StreamingFileWriter {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        self.f.write(buf)
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
