mod paths;
use anyhow::Result;
use paths::{AppData, CacheConfig, UserConfig};
use std::path::{Path, PathBuf};

/// Get or create a subdirectory under user config.
/// - [`subpath`]: If Some(P), a subdirectory will be created if necessary
///   and returned. If None, the root config directory will be returned.
pub fn get_or_create_user_config_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
    UserConfig::get_config_path(subpath)
}

/// Get or create a subdirectory in the app cache.
/// - [`subpath`]: If Some(P), a subdirectory will be created if necessary
///   and returned. If None, the root cache directory will be returned.
pub fn get_or_create_cache_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
    CacheConfig::get_cache_path(subpath)
}

pub fn get_or_create_embeddings_cache_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
    let path = PathBuf::from("embeddings");
    let path = subpath.map(|p| path.join(p)).or(Some(path));
    CacheConfig::get_cache_path(path)
}

/// Get or create a subdirectory in the app data dir.
/// - [`subpath`]: If Some(P), a subdirectory will be created if necessary
///   and returned. If None, the root app data directory will be returned.
pub fn get_or_create_app_data_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
    AppData::get_data_path(subpath)
}

/// TBI
pub fn get_shadow_cache_path() -> Result<PathBuf> {
    get_or_create_cache_path("shadow-index".into())
}

/// Get or create a path inside the embeddings cache.
pub fn get_embeddings_cache_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
    let embeddings_subpath = PathBuf::from("embeddings");
    get_or_create_cache_path(Some(
        subpath
            .map(|s| embeddings_subpath.join(s))
            .unwrap_or(embeddings_subpath),
    ))
}

#[cfg(test)]
mod tests {}
