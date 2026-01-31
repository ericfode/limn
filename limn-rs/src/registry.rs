//! Package registry for Limn
//!
//! Decentralized registry stored on IPFS with IPNS for updates.

use crate::error::{LimnError, Result};
use crate::ipfs;
use indexmap::IndexMap;
use semver::Version;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Get the limn home directory
pub fn limn_home() -> PathBuf {
    dirs::home_dir()
        .map(|h| h.join(".limn"))
        .unwrap_or_else(|| PathBuf::from(".limn"))
}

/// Get the registry cache directory
pub fn registry_cache_dir() -> PathBuf {
    limn_home().join("registry")
}

/// A package version entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageVersion {
    pub cid: String,
    pub published: String,
    #[serde(default, skip_serializing_if = "String::is_empty")]
    pub signature: String,
    #[serde(default, skip_serializing_if = "std::ops::Not::not")]
    pub yanked: bool,
}

/// A package entry in the registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackageEntry {
    pub latest: String,
    pub author: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub homepage: String,
    #[serde(default)]
    pub repository: String,
    #[serde(default)]
    pub license: String,
    #[serde(default)]
    pub keywords: Vec<String>,
    pub versions: IndexMap<String, PackageVersion>,
}

impl PackageEntry {
    /// Get a specific version
    pub fn get_version(&self, version: &str) -> Option<&PackageVersion> {
        self.versions.get(version)
    }

    /// Get the latest non-yanked version
    pub fn get_latest(&self) -> Option<(&String, &PackageVersion)> {
        if !self.latest.is_empty() {
            if let Some(v) = self.versions.get(&self.latest) {
                if !v.yanked {
                    return Some((&self.latest, v));
                }
            }
        }

        // Find highest non-yanked version
        self.versions
            .iter()
            .filter(|(_, v)| !v.yanked)
            .filter_map(|(ver_str, ver)| {
                Version::parse(ver_str).ok().map(|v| (v, ver_str, ver))
            })
            .max_by(|(a, _, _), (b, _, _)| a.cmp(b))
            .map(|(_, ver_str, ver)| (ver_str, ver))
    }

    /// Find a version satisfying a requirement
    pub fn find_version(&self, req: &str) -> Option<(&String, &PackageVersion)> {
        if req == "latest" || req == "*" {
            return self.get_latest();
        }

        // Try exact match first
        if let Some(v) = self.versions.get(req) {
            if !v.yanked {
                return Some((self.versions.get_key_value(req).unwrap().0, v));
            }
        }

        // Parse as semver requirement
        let req = semver::VersionReq::parse(req).ok()?;

        self.versions
            .iter()
            .filter(|(_, v)| !v.yanked)
            .filter_map(|(ver_str, ver)| {
                Version::parse(ver_str).ok().map(|v| (v, ver_str, ver))
            })
            .filter(|(v, _, _)| req.matches(v))
            .max_by(|(a, _, _), (b, _, _)| a.cmp(b))
            .map(|(_, ver_str, ver)| (ver_str, ver))
    }
}

/// The package registry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Registry {
    #[serde(default = "default_registry_version")]
    pub version: u32,
    #[serde(default)]
    pub updated: String,
    pub packages: IndexMap<String, PackageEntry>,
}

fn default_registry_version() -> u32 {
    1
}

impl Registry {
    /// Create an empty registry
    pub fn new() -> Self {
        Registry {
            version: 1,
            updated: chrono_now(),
            packages: IndexMap::new(),
        }
    }

    /// Look up a package
    pub fn lookup(&self, name: &str, version_req: &str) -> Option<(String, String)> {
        let entry = self.packages.get(name)?;
        let (version, ver) = entry.find_version(version_req)?;
        Some((version.clone(), ver.cid.clone()))
    }

    /// Search packages
    pub fn search(&self, query: &str, limit: usize) -> Vec<(&String, &PackageEntry)> {
        let query = query.to_lowercase();
        let mut results: Vec<_> = self.packages
            .iter()
            .filter_map(|(name, entry)| {
                let mut score = 0i32;

                // Name match
                let name_lower = name.to_lowercase();
                if name_lower.contains(&query) {
                    score += 100;
                    if name_lower.starts_with(&query) {
                        score += 50;
                    }
                }

                // Description match
                if entry.description.to_lowercase().contains(&query) {
                    score += 30;
                }

                // Keyword match
                for kw in &entry.keywords {
                    if kw.to_lowercase().contains(&query) {
                        score += 20;
                    }
                }

                // Author match
                if entry.author.to_lowercase().contains(&query) {
                    score += 10;
                }

                if score > 0 {
                    Some((score, name, entry))
                } else {
                    None
                }
            })
            .collect();

        results.sort_by(|a, b| b.0.cmp(&a.0));
        results.into_iter().take(limit).map(|(_, n, e)| (n, e)).collect()
    }

    /// Add or update a package
    pub fn add_package(
        &mut self,
        name: &str,
        version: &str,
        cid: &str,
        author: &str,
        description: &str,
        signature: Option<&str>,
    ) {
        let now = chrono_now();

        let entry = self.packages.entry(name.to_string()).or_insert_with(|| {
            PackageEntry {
                latest: String::new(),
                author: author.to_string(),
                description: description.to_string(),
                homepage: String::new(),
                repository: String::new(),
                license: String::new(),
                keywords: Vec::new(),
                versions: IndexMap::new(),
            }
        });

        entry.versions.insert(version.to_string(), PackageVersion {
            cid: cid.to_string(),
            published: now.clone(),
            signature: signature.unwrap_or("").to_string(),
            yanked: false,
        });

        // Update latest if newer
        if entry.latest.is_empty() {
            entry.latest = version.to_string();
        } else if let (Ok(new), Ok(old)) = (Version::parse(version), Version::parse(&entry.latest)) {
            if new > old {
                entry.latest = version.to_string();
            }
        }

        // Update metadata
        if !author.is_empty() {
            entry.author = author.to_string();
        }
        if !description.is_empty() {
            entry.description = description.to_string();
        }

        self.updated = now;
    }

    /// Yank a version
    pub fn yank(&mut self, name: &str, version: &str) -> bool {
        if let Some(entry) = self.packages.get_mut(name) {
            if let Some(ver) = entry.versions.get_mut(version) {
                ver.yanked = true;

                // Update latest if we yanked it
                if entry.latest == version {
                    entry.latest = entry.versions
                        .iter()
                        .filter(|(_, v)| !v.yanked)
                        .filter_map(|(ver_str, _)| {
                            Version::parse(ver_str).ok().map(|v| (v, ver_str.clone()))
                        })
                        .max_by(|(a, _), (b, _)| a.cmp(b))
                        .map(|(_, s)| s)
                        .unwrap_or_default();
                }

                self.updated = chrono_now();
                return true;
            }
        }
        false
    }

    /// Serialize to JSON
    pub fn to_json(&self) -> String {
        serde_json::to_string_pretty(self).unwrap_or_default()
    }

    /// Deserialize from JSON
    pub fn from_json(json: &str) -> Result<Self> {
        serde_json::from_str(json)
            .map_err(|e| LimnError::Registry(format!("Failed to parse registry: {}", e)))
    }
}

impl Default for Registry {
    fn default() -> Self {
        Self::new()
    }
}

/// Registry manager handles fetching and caching
pub struct RegistryManager {
    cache_dir: PathBuf,
    registries: IndexMap<String, Registry>,
}

impl RegistryManager {
    pub fn new() -> Self {
        let cache_dir = registry_cache_dir();
        std::fs::create_dir_all(&cache_dir).ok();

        RegistryManager {
            cache_dir,
            registries: IndexMap::new(),
        }
    }

    /// Load registry from cache
    pub fn load_cached(&mut self, name: &str) -> Option<&Registry> {
        if self.registries.contains_key(name) {
            return self.registries.get(name);
        }

        let cache_path = self.cache_dir.join(format!("{}.json", name));
        if cache_path.exists() {
            if let Ok(content) = std::fs::read_to_string(&cache_path) {
                if let Ok(registry) = Registry::from_json(&content) {
                    self.registries.insert(name.to_string(), registry);
                    return self.registries.get(name);
                }
            }
        }
        None
    }

    /// Save registry to cache
    pub fn save_cache(&self, name: &str, registry: &Registry) {
        let cache_path = self.cache_dir.join(format!("{}.json", name));
        let _ = std::fs::write(cache_path, registry.to_json());
    }

    /// Fetch registry from IPFS/IPNS
    pub fn fetch(&mut self, ipns_name: &str, force: bool) -> Result<&Registry> {
        // Return cached unless forced
        if !force {
            if let Some(reg) = self.load_cached(ipns_name) {
                return Ok(reg);
            }
        }

        if !ipfs::is_available() {
            if let Some(reg) = self.load_cached(ipns_name) {
                return Ok(reg);
            }
            return Err(LimnError::ipfs("IPFS not available and no cached registry"));
        }

        // Resolve IPNS
        let cid = ipfs::name_resolve(ipns_name)?;

        // Fetch content
        let content = ipfs::cat_string(&cid)?;

        // Parse
        let registry = Registry::from_json(&content)?;

        // Cache
        self.save_cache(ipns_name, &registry);
        self.registries.insert(ipns_name.to_string(), registry);

        Ok(self.registries.get(ipns_name).unwrap())
    }

    /// Publish registry to IPNS
    pub fn publish(&self, registry: &Registry, key_name: &str) -> Result<String> {
        if !ipfs::is_available() {
            return Err(LimnError::ipfs("IPFS not available"));
        }

        // Add to IPFS
        let json = registry.to_json();
        let cid = ipfs::add_bytes(json.as_bytes())?;

        // Publish to IPNS
        ipfs::name_publish(key_name, &cid)?;

        Ok(cid)
    }

    /// Lookup package across all registries
    pub fn lookup(&mut self, name: &str, version_req: &str) -> Option<(String, String, String)> {
        for (reg_name, registry) in &self.registries {
            if let Some((version, cid)) = registry.lookup(name, version_req) {
                return Some((version, cid, reg_name.clone()));
            }
        }
        None
    }

    /// Search across all registries
    pub fn search(&self, query: &str, limit: usize) -> Vec<(&String, &PackageEntry, &String)> {
        let mut results = Vec::new();
        for (reg_name, registry) in &self.registries {
            for (name, entry) in registry.search(query, limit) {
                results.push((name, entry, reg_name));
            }
        }
        results.truncate(limit);
        results
    }
}

impl Default for RegistryManager {
    fn default() -> Self {
        Self::new()
    }
}

fn chrono_now() -> String {
    use std::time::SystemTime;
    let now = SystemTime::now()
        .duration_since(SystemTime::UNIX_EPOCH)
        .unwrap()
        .as_secs();
    format!("{}Z", now)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_registry_add_lookup() {
        let mut registry = Registry::new();

        registry.add_package(
            "math-utils",
            "1.0.0",
            "bafybei123",
            "alice",
            "Math utilities",
            None,
        );

        let result = registry.lookup("math-utils", "latest");
        assert!(result.is_some());

        let (version, cid) = result.unwrap();
        assert_eq!(version, "1.0.0");
        assert_eq!(cid, "bafybei123");
    }

    #[test]
    fn test_version_matching() {
        let mut registry = Registry::new();

        registry.add_package("pkg", "1.0.0", "cid1", "", "", None);
        registry.add_package("pkg", "1.1.0", "cid2", "", "", None);
        registry.add_package("pkg", "2.0.0", "cid3", "", "", None);

        // Latest
        let result = registry.lookup("pkg", "latest").unwrap();
        assert_eq!(result.0, "2.0.0");

        // Caret range
        let result = registry.lookup("pkg", "^1.0.0").unwrap();
        assert_eq!(result.0, "1.1.0");

        // Exact
        let result = registry.lookup("pkg", "1.0.0").unwrap();
        assert_eq!(result.0, "1.0.0");
    }
}
