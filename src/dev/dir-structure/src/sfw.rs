use std::fmt;
use std::fs;
use std::fs::File;
use std::io;
use std::path::Path;

use crate::Result;
use crate::WrapIoError;

pub struct StreamingFileWriter {
    f: File,
}

impl StreamingFileWriter {
    pub fn new(path: &Path) -> Result<Self> {
        if let Some(parent) = path.parent()
            && !parent.exists()
        {
            fs::create_dir_all(parent).wrap_io_error_with(parent)?;
        }
        let f = File::create(path).wrap_io_error_with(path)?;
        Ok(Self { f })
    }
}

impl io::Write for StreamingFileWriter {
    fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        <File as io::Write>::write(&mut self.f, buf)
    }

    fn flush(&mut self) -> io::Result<()> {
        self.f.flush()
    }
}

impl fmt::Write for StreamingFileWriter {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        use std::io::Write;

        self.f.write_all(s.as_bytes()).map_err(|_| fmt::Error)?;
        Ok(())
    }
}
