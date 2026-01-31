//! IPFS integration for Limn package management

use crate::error::{LimnError, Result};
use std::path::Path;
use std::process::Command;
use serde::{Deserialize, Serialize};

/// Check if IPFS daemon is available
pub fn is_available() -> bool {
    Command::new("ipfs")
        .arg("version")
        .output()
        .map(|o| o.status.success())
        .unwrap_or(false)
}

/// Get IPFS node ID info
pub fn get_id() -> Result<IpfsId> {
    let output = Command::new("ipfs")
        .arg("id")
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs id: {}", e)))?;

    if !output.status.success() {
        return Err(LimnError::ipfs("IPFS daemon not running"));
    }

    let info: IpfsId = serde_json::from_slice(&output.stdout)
        .map_err(|e| LimnError::ipfs(format!("Failed to parse ipfs id output: {}", e)))?;

    Ok(info)
}

#[derive(Debug, Deserialize)]
pub struct IpfsId {
    #[serde(rename = "ID")]
    pub id: String,
    #[serde(rename = "AgentVersion")]
    pub agent_version: String,
}

/// Add a file or directory to IPFS
pub fn add(path: &Path, recursive: bool) -> Result<String> {
    let mut cmd = Command::new("ipfs");
    cmd.arg("add").arg("-Q"); // Quiet mode - only output hash

    if recursive {
        cmd.arg("-r");
    }

    cmd.arg(path);

    let output = cmd.output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs add: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs add failed: {}", stderr)));
    }

    let cid = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(cid)
}

/// Add bytes to IPFS
pub fn add_bytes(data: &[u8]) -> Result<String> {
    use std::io::Write;
    use std::process::Stdio;

    let mut child = Command::new("ipfs")
        .args(["add", "-Q"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| LimnError::ipfs(format!("Failed to spawn ipfs: {}", e)))?;

    {
        let stdin = child.stdin.as_mut()
            .ok_or_else(|| LimnError::ipfs("Failed to open stdin"))?;
        stdin.write_all(data)
            .map_err(|e| LimnError::ipfs(format!("Failed to write to stdin: {}", e)))?;
    }

    let output = child.wait_with_output()
        .map_err(|e| LimnError::ipfs(format!("Failed to wait for ipfs: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs add failed: {}", stderr)));
    }

    let cid = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(cid)
}

/// Get content from IPFS
pub fn cat(cid: &str) -> Result<Vec<u8>> {
    let output = Command::new("ipfs")
        .args(["cat", cid])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs cat: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs cat failed: {}", stderr)));
    }

    Ok(output.stdout)
}

/// Get content as string
pub fn cat_string(cid: &str) -> Result<String> {
    let bytes = cat(cid)?;
    String::from_utf8(bytes)
        .map_err(|e| LimnError::ipfs(format!("Invalid UTF-8 in content: {}", e)))
}

/// Download content to a path
pub fn get(cid: &str, output_path: &Path) -> Result<()> {
    let output = Command::new("ipfs")
        .args(["get", "-o"])
        .arg(output_path)
        .arg(cid)
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs get: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs get failed: {}", stderr)));
    }

    Ok(())
}

/// Pin content
pub fn pin(cid: &str) -> Result<()> {
    let output = Command::new("ipfs")
        .args(["pin", "add", cid])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs pin: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs pin failed: {}", stderr)));
    }

    Ok(())
}

/// Unpin content
pub fn unpin(cid: &str) -> Result<()> {
    let output = Command::new("ipfs")
        .args(["pin", "rm", cid])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs pin rm: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs pin rm failed: {}", stderr)));
    }

    Ok(())
}

/// Resolve IPNS name to CID
pub fn name_resolve(name: &str) -> Result<String> {
    let output = Command::new("ipfs")
        .args(["name", "resolve", &format!("/ipns/{}", name)])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs name resolve: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs name resolve failed: {}", stderr)));
    }

    let resolved = String::from_utf8_lossy(&output.stdout).trim().to_string();

    // Strip /ipfs/ prefix if present
    if let Some(cid) = resolved.strip_prefix("/ipfs/") {
        Ok(cid.to_string())
    } else {
        Ok(resolved)
    }
}

/// Publish CID to IPNS
pub fn name_publish(key: &str, cid: &str) -> Result<()> {
    let output = Command::new("ipfs")
        .args(["name", "publish", &format!("--key={}", key), &format!("/ipfs/{}", cid)])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs name publish: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs name publish failed: {}", stderr)));
    }

    Ok(())
}

/// List IPFS keys
pub fn key_list() -> Result<Vec<String>> {
    let output = Command::new("ipfs")
        .args(["key", "list"])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs key list: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs key list failed: {}", stderr)));
    }

    let keys = String::from_utf8_lossy(&output.stdout)
        .lines()
        .map(|s| s.to_string())
        .collect();

    Ok(keys)
}

/// Generate IPFS key
pub fn key_gen(name: &str) -> Result<String> {
    let output = Command::new("ipfs")
        .args(["key", "gen", "--type=ed25519", name])
        .output()
        .map_err(|e| LimnError::ipfs(format!("Failed to run ipfs key gen: {}", e)))?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(LimnError::ipfs(format!("ipfs key gen failed: {}", stderr)));
    }

    let peer_id = String::from_utf8_lossy(&output.stdout).trim().to_string();
    Ok(peer_id)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_is_available() {
        // This test only passes if IPFS is installed
        // Just check that the function doesn't panic
        let _ = is_available();
    }
}
