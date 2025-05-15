use std::{
    env::current_exe,
    path::{Path, PathBuf},
};

use anyhow::{Result, anyhow};
use directories::{BaseDirs, ProjectDirs, UserDirs};

struct BaseLocations();
impl BaseLocations {
    fn proj_dirs() -> Option<ProjectDirs> {
        ProjectDirs::from(
            "net",
            "crystaldrift",
            current_exe()
                .unwrap()
                .file_name()
                .unwrap()
                .to_string_lossy()
                .as_ref(),
        )
    }
    #[allow(dead_code)]
    fn base_dirs() -> BaseDirs {
        BaseDirs::new().expect("Unable to create BaseDirs instance.")
    }
    fn user_dirs() -> Option<UserDirs> {
        UserDirs::new()
    }
}

/*
Caches in /L/C/...
Baseline config in /L/AS/...
*/

trait ConfigPath {
    fn get_root_path() -> Result<PathBuf>;

    /// Get a subdir under the system_data_root, creating as necessary.
    fn get_or_create_subdir<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
        let root_path = Self::get_root_path()?;
        let full_path = subpath
            .map(|p| root_path.to_owned().join(p))
            .unwrap_or(root_path);
        std::fs::create_dir_all(&full_path)?;
        Ok(full_path.to_owned())
    }
}

pub(crate) struct AppData();
impl ConfigPath for AppData {
    fn get_root_path() -> Result<PathBuf> {
        Ok(BaseLocations::proj_dirs()
            .map(|d| d.cache_dir().to_owned())
            .ok_or(anyhow!("Unable to determine app data dir!"))?)
    }
}
impl AppData {
    /// Get or create the path to the cache directory
    pub(crate) fn get_data_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
        Self::get_or_create_subdir(subpath)
    }
}

pub(crate) struct CacheConfig();
impl ConfigPath for CacheConfig {
    /// Get the path to the system data directory (bundled with the binary).
    fn get_root_path() -> Result<PathBuf> {
        Ok(BaseLocations::proj_dirs()
            .map(|d| d.cache_dir().to_owned())
            .ok_or(anyhow!("Unable to determine cache dir!"))?)
    }
}
impl CacheConfig {
    /// Get or create the path to the cache directory
    pub(crate) fn get_cache_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
        Self::get_or_create_subdir(subpath)
    }
}

pub(crate) struct UserConfig();
impl ConfigPath for UserConfig {
    /// Get the path to the user config directory.
    /// Note that in this case, as a matter of taste, we're forcing the
    /// XDG standard rather than any non-OS-specific bit. This probably screws
    /// Windows for now.
    fn get_root_path() -> Result<PathBuf> {
        let home_dir = BaseLocations::user_dirs()
            .map(|d| d.home_dir().to_path_buf())
            .ok_or_else(|| anyhow!("Failed to get home directory"))?;
        Ok(home_dir.join(".config").join("cj"))
    }
}
impl UserConfig {
    pub(crate) fn get_config_path<P: AsRef<Path>>(subpath: Option<P>) -> Result<PathBuf> {
        Self::get_or_create_subdir(subpath)
    }
}
