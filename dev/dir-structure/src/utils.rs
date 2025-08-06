use crate::WrapIoError;

pub fn create_parent_dir(path: &std::path::Path) -> crate::Result<()> {
    if let Some(parent) = path.parent() {
        if !parent.exists() {
            std::fs::create_dir_all(parent).wrap_io_error_with(parent)?;
        }
    }
    Ok(())
}

#[cfg(feature = "tokio")]
pub async fn create_parent_dir_async(path: &std::path::Path) -> crate::Result<()> {
    if let Some(parent) = path.parent() {
        if !tokio::fs::try_exists(parent)
            .await
            .wrap_io_error_with(parent)?
        {
            tokio::fs::create_dir_all(parent)
                .await
                .wrap_io_error_with(parent)?;
        }
    }
    Ok(())
}
