//! Package management for Limn
//!
//! Handles package creation, validation, building, and publishing.

use crate::error::{LimnError, Result};
use crate::registry::limn_home;
use crate::ipfs;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};

/// Package manifest filename
pub const MANIFEST_FILE: &str = "pak.limn";
/// Lock file filename
pub const LOCK_FILE: &str = "pak.lock.limn";
/// Build directory
pub const BUILD_DIR: &str = ".limn-pkg";
/// Dependencies directory
pub const DEPS_DIR: &str = ".limn_deps";

/// Get the package cache directory
pub fn cache_dir() -> PathBuf {
    limn_home().join("pak")
}

/// Package manifest
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Manifest {
    pub name: String,
    pub version: Version,
    pub author: String,
    pub description: String,
    pub entry: String,
    pub dependencies: HashMap<String, Dependency>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Version {
    pub major: u32,
    pub minor: u32,
    pub patch: u32,
}

impl Version {
    pub fn new(major: u32, minor: u32, patch: u32) -> Self {
        Version { major, minor, patch }
    }

    pub fn to_string(&self) -> String {
        format!("{}.{}.{}", self.major, self.minor, self.patch)
    }
}

impl Default for Version {
    fn default() -> Self {
        Version::new(1, 0, 0)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Dependency {
    pub constraint: String,
    pub is_cid: bool,
}

impl Manifest {
    /// Create a new manifest
    pub fn new(name: &str) -> Self {
        Manifest {
            name: name.to_string(),
            version: Version::default(),
            author: std::env::var("USER").unwrap_or_else(|_| "anonymous".to_string()),
            description: String::new(),
            entry: "src/main.limn".to_string(),
            dependencies: HashMap::new(),
        }
    }

    /// Parse manifest from Limn format
    pub fn parse(content: &str) -> Result<Self> {
        use regex::Regex;

        let mut manifest = Manifest::new("");

        // Parse pak_nom sa "name"
        let name_re = Regex::new(r#"pak_nom\s+sa\s+"([^"]+)""#).unwrap();
        if let Some(cap) = name_re.captures(content) {
            manifest.name = cap[1].to_string();
        }

        // Parse version parts
        let maj_re = Regex::new(r"pak_ver_maj\s+sa\s+(\d+)").unwrap();
        let min_re = Regex::new(r"pak_ver_min\s+sa\s+(\d+)").unwrap();
        let pat_re = Regex::new(r"pak_ver_pat\s+sa\s+(\d+)").unwrap();

        if let Some(cap) = maj_re.captures(content) {
            manifest.version.major = cap[1].parse().unwrap_or(0);
        }
        if let Some(cap) = min_re.captures(content) {
            manifest.version.minor = cap[1].parse().unwrap_or(0);
        }
        if let Some(cap) = pat_re.captures(content) {
            manifest.version.patch = cap[1].parse().unwrap_or(0);
        }

        // Parse author
        let aut_re = Regex::new(r#"pak_aut\s+sa\s+"([^"]+)""#).unwrap();
        if let Some(cap) = aut_re.captures(content) {
            manifest.author = cap[1].to_string();
        }

        // Parse description
        let des_re = Regex::new(r#"pak_des\s+sa\s+"([^"]+)""#).unwrap();
        if let Some(cap) = des_re.captures(content) {
            manifest.description = cap[1].to_string();
        }

        // Parse entry
        let ent_re = Regex::new(r#"pak_ent\s+sa\s+"([^"]+)""#).unwrap();
        if let Some(cap) = ent_re.captures(content) {
            manifest.entry = cap[1].to_string();
        }

        // Parse dependencies: dep_name sa "constraint" or dep_name sa cid "cid"
        let dep_re = Regex::new(r#"dep_([a-z_]+)\s+sa\s+(?:cid\s+)?"([^"]+)""#).unwrap();
        for cap in dep_re.captures_iter(content) {
            let name = cap[1].replace('_', "-");
            let constraint = cap[2].to_string();
            let is_cid = constraint.starts_with("bafybei") || constraint.starts_with("Qm");

            manifest.dependencies.insert(name, Dependency { constraint, is_cid });
        }

        if manifest.name.is_empty() {
            return Err(LimnError::package("Missing package name (pak_nom)"));
        }

        Ok(manifest)
    }

    /// Generate Limn format manifest
    pub fn to_limn(&self) -> String {
        let mut lines = vec![
            "# pak.limn - Package manifest".to_string(),
            String::new(),
            "# Package info".to_string(),
            "whe pak_nom".to_string(),
            "whe pak_ver_maj".to_string(),
            "whe pak_ver_min".to_string(),
            "whe pak_ver_pat".to_string(),
            "whe pak_aut".to_string(),
            "whe pak_des".to_string(),
            "whe pak_ent".to_string(),
            String::new(),
        ];

        // Add dependency variables
        for name in self.dependencies.keys() {
            let safe_name = name.replace('-', "_");
            lines.push(format!("whe dep_{}", safe_name));
        }

        lines.push(String::new());
        lines.push("---".to_string());
        lines.push(format!("pak_nom sa \"{}\"", self.name));
        lines.push(format!("pak_ver_maj sa {}", self.version.major));
        lines.push(format!("pak_ver_min sa {}", self.version.minor));
        lines.push(format!("pak_ver_pat sa {}", self.version.patch));
        lines.push(format!("pak_aut sa \"{}\"", self.author));
        lines.push(format!("pak_des sa \"{}\"", self.description));
        lines.push(format!("pak_ent sa \"{}\"", self.entry));
        lines.push(String::new());

        // Add dependency constraints
        if !self.dependencies.is_empty() {
            lines.push("# Dependencies".to_string());
            for (name, dep) in &self.dependencies {
                let safe_name = name.replace('-', "_");
                if dep.is_cid {
                    lines.push(format!("dep_{} sa cid \"{}\"", safe_name, dep.constraint));
                } else {
                    lines.push(format!("dep_{} sa \"{}\"", safe_name, dep.constraint));
                }
            }
        }

        lines.join("\n")
    }

    /// Load manifest from directory
    pub fn load(dir: &Path) -> Result<Self> {
        let manifest_path = dir.join(MANIFEST_FILE);
        if !manifest_path.exists() {
            return Err(LimnError::package(format!("No {} found", MANIFEST_FILE)));
        }

        let content = fs::read_to_string(&manifest_path)
            .map_err(|e| LimnError::package(format!("Failed to read manifest: {}", e)))?;

        Self::parse(&content)
    }

    /// Save manifest to directory
    pub fn save(&self, dir: &Path) -> Result<()> {
        let manifest_path = dir.join(MANIFEST_FILE);
        fs::write(&manifest_path, self.to_limn())
            .map_err(|e| LimnError::package(format!("Failed to write manifest: {}", e)))
    }
}

/// Initialize a new package
pub fn init(name: &str, dir: Option<&Path>) -> Result<PathBuf> {
    let pkg_dir = dir.map(|p| p.to_path_buf()).unwrap_or_else(|| PathBuf::from(name));

    if pkg_dir.exists() {
        return Err(LimnError::package(format!("Directory already exists: {:?}", pkg_dir)));
    }

    // Create directories
    fs::create_dir_all(&pkg_dir)?;
    fs::create_dir_all(pkg_dir.join("src"))?;
    fs::create_dir_all(pkg_dir.join("test"))?;

    // Create manifest
    let manifest = Manifest::new(name);
    manifest.save(&pkg_dir)?;

    // Create entry file
    let entry_content = format!(
        r#"# {} - Main entry point

# Example: a simple greeting
whe greeting
whe name

greeting sa "Hello from " joi name

---
# key: example
name sa "{}"
"#,
        name, name
    );
    fs::write(pkg_dir.join("src/main.limn"), entry_content)?;

    // Create test file
    let test_content = format!(
        r#"# {} - Tests

whe test_a
whe test_b
whe test_result

test_a joi test_b sa test_result

---
# key: run tests
test_a sa 1
test_b sa 2
"#,
        name
    );
    fs::write(pkg_dir.join("test/test_main.limn"), test_content)?;

    // Create .gitignore
    let gitignore = r#"# Limn
.limn_deps/
.limn-pkg/
pak.lock.limn

# OS
.DS_Store
Thumbs.db
"#;
    fs::write(pkg_dir.join(".gitignore"), gitignore)?;

    Ok(pkg_dir)
}

/// Validate package structure
pub fn check(dir: &Path) -> Result<Vec<String>> {
    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    // Check manifest
    let manifest_path = dir.join(MANIFEST_FILE);
    if !manifest_path.exists() {
        errors.push(format!("Missing {}", MANIFEST_FILE));
    } else {
        match Manifest::load(dir) {
            Ok(manifest) => {
                if manifest.name.is_empty() {
                    errors.push("Missing package name (pak_nom)".to_string());
                }
                if manifest.entry.is_empty() {
                    warnings.push("Missing entry point (pak_ent)".to_string());
                } else {
                    let entry_path = dir.join(&manifest.entry);
                    if !entry_path.exists() {
                        errors.push(format!("Entry point not found: {}", manifest.entry));
                    }
                }
            }
            Err(e) => {
                errors.push(format!("Invalid manifest: {}", e));
            }
        }
    }

    // Check src directory
    if !dir.join("src").exists() {
        warnings.push("Missing src/ directory".to_string());
    }

    if !errors.is_empty() {
        return Err(LimnError::package(errors.join("\n")));
    }

    Ok(warnings)
}

/// Build package for distribution
pub fn build(dir: &Path) -> Result<PathBuf> {
    // Validate first
    check(dir)?;

    let manifest = Manifest::load(dir)?;
    let build_dir = dir.join(BUILD_DIR);

    // Clean and create build directory
    if build_dir.exists() {
        fs::remove_dir_all(&build_dir)?;
    }
    fs::create_dir_all(&build_dir)?;

    // Copy manifest
    fs::copy(dir.join(MANIFEST_FILE), build_dir.join(MANIFEST_FILE))?;

    // Copy src
    if dir.join("src").exists() {
        copy_dir_recursive(&dir.join("src"), &build_dir.join("src"))?;
    }

    // Copy README if exists
    for readme in &["README.md", "README", "readme.md"] {
        if dir.join(readme).exists() {
            fs::copy(dir.join(readme), build_dir.join(readme))?;
            break;
        }
    }

    // Copy LICENSE if exists
    for license in &["LICENSE", "LICENSE.md", "license"] {
        if dir.join(license).exists() {
            fs::copy(dir.join(license), build_dir.join(license))?;
            break;
        }
    }

    Ok(build_dir)
}

/// Publish package to IPFS
pub fn publish(dir: &Path, _sign_key: Option<&str>) -> Result<String> {
    if !ipfs::is_available() {
        return Err(LimnError::ipfs("IPFS daemon not running. Start with: ipfs daemon"));
    }

    // Build first
    let build_dir = build(dir)?;

    // TODO: Sign if key provided

    // Add to IPFS
    let cid = ipfs::add(&build_dir, true)?;

    Ok(cid)
}

/// Fetch a package by CID to cache
pub fn fetch(cid: &str) -> Result<PathBuf> {
    let cache = cache_dir();
    fs::create_dir_all(&cache)?;

    let pkg_dir = cache.join(cid);
    if pkg_dir.exists() {
        return Ok(pkg_dir);
    }

    if !ipfs::is_available() {
        return Err(LimnError::ipfs("IPFS not available"));
    }

    ipfs::get(cid, &pkg_dir)?;
    Ok(pkg_dir)
}

fn copy_dir_recursive(src: &Path, dst: &Path) -> Result<()> {
    fs::create_dir_all(dst)?;

    for entry in fs::read_dir(src)? {
        let entry = entry?;
        let path = entry.path();
        let dest = dst.join(entry.file_name());

        if path.is_dir() {
            copy_dir_recursive(&path, &dest)?;
        } else {
            fs::copy(&path, &dest)?;
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manifest_roundtrip() {
        let mut manifest = Manifest::new("test-pkg");
        manifest.description = "A test package".to_string();
        manifest.dependencies.insert("dep1".to_string(), Dependency {
            constraint: "^1.0.0".to_string(),
            is_cid: false,
        });

        let limn = manifest.to_limn();
        let parsed = Manifest::parse(&limn).unwrap();

        assert_eq!(parsed.name, "test-pkg");
        assert_eq!(parsed.description, "A test package");
        assert!(parsed.dependencies.contains_key("dep1"));
    }
}
