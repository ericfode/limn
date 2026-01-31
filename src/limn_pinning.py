#!/usr/bin/env python3
"""
Limn Remote Pinning - Integration with IPFS pinning services.

Supports:
- Pinata
- web3.storage
- Infura IPFS
- Filebase
- Generic IPFS Pinning Service API (PSA)

Ensures packages remain available even when local node is offline.
"""

import os
import sys
import json
import time
import hashlib
import subprocess
from pathlib import Path
from typing import Dict, List, Optional, Any, Tuple
from dataclasses import dataclass, field
from datetime import datetime
import urllib.request
import urllib.parse
import urllib.error
from abc import ABC, abstractmethod

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


# ============================================================================
# Configuration
# ============================================================================

LIMN_HOME = Path.home() / ".limn"
PINNING_CONFIG_FILE = LIMN_HOME / "pinning.json"


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class PinStatus:
    """Status of a pin request."""
    cid: str
    status: str  # "queued", "pinning", "pinned", "failed"
    name: str = ""
    service: str = ""
    created: str = ""
    delegates: List[str] = field(default_factory=list)
    info: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ServiceConfig:
    """Configuration for a pinning service."""
    name: str
    endpoint: str
    api_key: str
    secret: str = ""
    extra: Dict[str, str] = field(default_factory=dict)

    def to_dict(self) -> dict:
        d = {
            "name": self.name,
            "endpoint": self.endpoint,
            "api_key": self.api_key,
        }
        if self.secret:
            d["secret"] = self.secret
        if self.extra:
            d["extra"] = self.extra
        return d

    @classmethod
    def from_dict(cls, d: dict) -> "ServiceConfig":
        return cls(
            name=d.get("name", ""),
            endpoint=d.get("endpoint", ""),
            api_key=d.get("api_key", ""),
            secret=d.get("secret", ""),
            extra=d.get("extra", {}),
        )


# ============================================================================
# Pinning Service Interface
# ============================================================================

class PinningService(ABC):
    """Abstract base class for pinning services."""

    @abstractmethod
    def pin_by_cid(self, cid: str, name: str = "") -> Optional[PinStatus]:
        """Pin content by CID."""
        pass

    @abstractmethod
    def pin_file(self, path: Path, name: str = "") -> Optional[PinStatus]:
        """Pin a local file or directory."""
        pass

    @abstractmethod
    def unpin(self, cid: str) -> bool:
        """Unpin content."""
        pass

    @abstractmethod
    def get_status(self, cid: str) -> Optional[PinStatus]:
        """Get pin status."""
        pass

    @abstractmethod
    def list_pins(self, limit: int = 100) -> List[PinStatus]:
        """List pinned content."""
        pass


# ============================================================================
# Pinata Service
# ============================================================================

class PinataService(PinningService):
    """Pinata.cloud pinning service."""

    def __init__(self, config: ServiceConfig):
        self.api_key = config.api_key
        self.secret = config.secret
        self.endpoint = config.endpoint or "https://api.pinata.cloud"

    def _request(self, method: str, path: str, data: dict = None,
                 files: dict = None) -> Optional[dict]:
        """Make API request to Pinata."""
        url = f"{self.endpoint}{path}"

        headers = {
            "pinata_api_key": self.api_key,
            "pinata_secret_api_key": self.secret,
        }

        try:
            if files:
                # Multipart form data (for file upload)
                import io

                boundary = "----LimnPakBoundary" + str(int(time.time()))
                body_parts = []

                # Add form fields
                if data:
                    for key, value in data.items():
                        body_parts.append(f"--{boundary}\r\n")
                        body_parts.append(f'Content-Disposition: form-data; name="{key}"\r\n\r\n')
                        if isinstance(value, dict):
                            body_parts.append(json.dumps(value))
                        else:
                            body_parts.append(str(value))
                        body_parts.append("\r\n")

                # Add files
                for field_name, (filename, content, content_type) in files.items():
                    body_parts.append(f"--{boundary}\r\n")
                    body_parts.append(
                        f'Content-Disposition: form-data; name="{field_name}"; '
                        f'filename="{filename}"\r\n'
                    )
                    body_parts.append(f"Content-Type: {content_type}\r\n\r\n")
                    body_parts.append(content if isinstance(content, str) else content.decode())
                    body_parts.append("\r\n")

                body_parts.append(f"--{boundary}--\r\n")
                body = "".join(body_parts).encode('utf-8')

                headers["Content-Type"] = f"multipart/form-data; boundary={boundary}"

                req = urllib.request.Request(url, data=body, headers=headers, method=method)
            else:
                if data:
                    body = json.dumps(data).encode('utf-8')
                    headers["Content-Type"] = "application/json"
                else:
                    body = None

                req = urllib.request.Request(url, data=body, headers=headers, method=method)

            with urllib.request.urlopen(req, timeout=120) as response:
                return json.loads(response.read().decode('utf-8'))

        except urllib.error.HTTPError as e:
            print(f"Pinata API error: {e.code} - {e.read().decode()}")
            return None
        except Exception as e:
            print(f"Request error: {e}")
            return None

    def pin_by_cid(self, cid: str, name: str = "") -> Optional[PinStatus]:
        """Pin content by CID (using pinByHash)."""
        data = {
            "hashToPin": cid,
            "pinataMetadata": {
                "name": name or cid[:16],
            }
        }

        result = self._request("POST", "/pinning/pinByHash", data)
        if result:
            return PinStatus(
                cid=result.get("ipfsHash", cid),
                status="queued",
                name=name,
                service="pinata",
                created=datetime.utcnow().isoformat() + "Z",
                info=result,
            )
        return None

    def pin_file(self, path: Path, name: str = "") -> Optional[PinStatus]:
        """Pin a local file or directory."""
        # For directories, we need to create a CAR file or use pinFromFS
        # For simplicity, use IPFS add first, then pin by CID

        # First add to local IPFS
        try:
            cmd = ["ipfs", "add", "-Q"]
            if path.is_dir():
                cmd.extend(["-r", "--wrap-with-directory"])
            cmd.append(str(path))

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
            if result.returncode != 0:
                print(f"IPFS add failed: {result.stderr}")
                return None

            cid = result.stdout.strip()
        except Exception as e:
            print(f"IPFS error: {e}")
            return None

        # Now pin by CID
        return self.pin_by_cid(cid, name or path.name)

    def unpin(self, cid: str) -> bool:
        """Unpin content."""
        result = self._request("DELETE", f"/pinning/unpin/{cid}")
        return result is not None

    def get_status(self, cid: str) -> Optional[PinStatus]:
        """Get pin status."""
        # List pins filtered by hash
        result = self._request("GET", f"/data/pinList?hashContains={cid}")
        if result and result.get("rows"):
            pin = result["rows"][0]
            return PinStatus(
                cid=pin.get("ipfs_pin_hash", cid),
                status="pinned" if pin.get("date_pinned") else "queued",
                name=pin.get("metadata", {}).get("name", ""),
                service="pinata",
                created=pin.get("date_pinned", ""),
                info=pin,
            )
        return None

    def list_pins(self, limit: int = 100) -> List[PinStatus]:
        """List pinned content."""
        result = self._request("GET", f"/data/pinList?pageLimit={limit}")
        if not result:
            return []

        pins = []
        for pin in result.get("rows", []):
            pins.append(PinStatus(
                cid=pin.get("ipfs_pin_hash", ""),
                status="pinned" if pin.get("date_pinned") else "queued",
                name=pin.get("metadata", {}).get("name", ""),
                service="pinata",
                created=pin.get("date_pinned", ""),
                info=pin,
            ))
        return pins


# ============================================================================
# Web3.storage Service
# ============================================================================

class Web3StorageService(PinningService):
    """web3.storage pinning service."""

    def __init__(self, config: ServiceConfig):
        self.api_token = config.api_key
        self.endpoint = config.endpoint or "https://api.web3.storage"

    def _request(self, method: str, path: str, data: bytes = None,
                 headers: dict = None) -> Optional[dict]:
        """Make API request to web3.storage."""
        url = f"{self.endpoint}{path}"

        req_headers = {
            "Authorization": f"Bearer {self.api_token}",
        }
        if headers:
            req_headers.update(headers)

        try:
            req = urllib.request.Request(
                url, data=data, headers=req_headers, method=method
            )
            with urllib.request.urlopen(req, timeout=120) as response:
                return json.loads(response.read().decode('utf-8'))
        except urllib.error.HTTPError as e:
            print(f"Web3.storage API error: {e.code} - {e.read().decode()}")
            return None
        except Exception as e:
            print(f"Request error: {e}")
            return None

    def pin_by_cid(self, cid: str, name: str = "") -> Optional[PinStatus]:
        """Pin content by CID - web3.storage requires uploading content."""
        # web3.storage doesn't support pin-by-CID, need to fetch and re-upload
        print("Note: web3.storage requires content upload, not pin-by-CID")
        print("Use pin_file() or fetch content from IPFS first")
        return None

    def pin_file(self, path: Path, name: str = "") -> Optional[PinStatus]:
        """Pin a local file."""
        if path.is_dir():
            # Need to create a CAR file for directories
            print("Directory upload requires CAR file creation")
            print("Use: ipfs-car pack <dir> > package.car")
            return None

        content = path.read_bytes()
        headers = {"Content-Type": "application/octet-stream"}

        result = self._request("POST", "/upload", content, headers)
        if result and "cid" in result:
            return PinStatus(
                cid=result["cid"],
                status="pinned",
                name=name or path.name,
                service="web3.storage",
                created=datetime.utcnow().isoformat() + "Z",
                info=result,
            )
        return None

    def unpin(self, cid: str) -> bool:
        """Unpin content - not supported by web3.storage."""
        print("web3.storage does not support unpinning")
        return False

    def get_status(self, cid: str) -> Optional[PinStatus]:
        """Get pin status."""
        result = self._request("GET", f"/status/{cid}")
        if result:
            return PinStatus(
                cid=result.get("cid", cid),
                status="pinned" if result.get("pins") else "queued",
                name="",
                service="web3.storage",
                created=result.get("created", ""),
                info=result,
            )
        return None

    def list_pins(self, limit: int = 100) -> List[PinStatus]:
        """List pinned content."""
        result = self._request("GET", f"/user/uploads?size={limit}")
        if not result:
            return []

        pins = []
        for upload in result:
            pins.append(PinStatus(
                cid=upload.get("cid", ""),
                status="pinned",
                name=upload.get("name", ""),
                service="web3.storage",
                created=upload.get("created", ""),
                info=upload,
            ))
        return pins


# ============================================================================
# IPFS Pinning Service API (Generic)
# ============================================================================

class IPFSPinningServiceAPI(PinningService):
    """Generic IPFS Pinning Service API implementation."""

    def __init__(self, config: ServiceConfig):
        self.endpoint = config.endpoint
        self.api_key = config.api_key
        self.name = config.name

    def _request(self, method: str, path: str, data: dict = None) -> Optional[dict]:
        """Make API request."""
        url = f"{self.endpoint}{path}"

        headers = {
            "Authorization": f"Bearer {self.api_key}",
            "Content-Type": "application/json",
        }

        try:
            body = json.dumps(data).encode('utf-8') if data else None
            req = urllib.request.Request(url, data=body, headers=headers, method=method)

            with urllib.request.urlopen(req, timeout=60) as response:
                return json.loads(response.read().decode('utf-8'))
        except Exception as e:
            print(f"API error: {e}")
            return None

    def pin_by_cid(self, cid: str, name: str = "") -> Optional[PinStatus]:
        """Pin content by CID."""
        data = {
            "cid": cid,
            "name": name or cid[:16],
        }

        result = self._request("POST", "/pins", data)
        if result:
            return PinStatus(
                cid=result.get("pin", {}).get("cid", cid),
                status=result.get("status", "queued"),
                name=name,
                service=self.name,
                created=result.get("created", ""),
                delegates=result.get("delegates", []),
                info=result,
            )
        return None

    def pin_file(self, path: Path, name: str = "") -> Optional[PinStatus]:
        """Pin a local file - add to IPFS first."""
        try:
            cmd = ["ipfs", "add", "-Q"]
            if path.is_dir():
                cmd.append("-r")
            cmd.append(str(path))

            result = subprocess.run(cmd, capture_output=True, text=True, timeout=300)
            if result.returncode != 0:
                return None

            cid = result.stdout.strip()
            return self.pin_by_cid(cid, name or path.name)
        except Exception:
            return None

    def unpin(self, cid: str) -> bool:
        """Unpin content."""
        # Find pin request ID first
        status = self.get_status(cid)
        if not status:
            return False

        request_id = status.info.get("requestid")
        if request_id:
            result = self._request("DELETE", f"/pins/{request_id}")
            return result is not None

        return False

    def get_status(self, cid: str) -> Optional[PinStatus]:
        """Get pin status."""
        result = self._request("GET", f"/pins?cid={cid}")
        if result and result.get("results"):
            pin = result["results"][0]
            return PinStatus(
                cid=pin.get("pin", {}).get("cid", cid),
                status=pin.get("status", ""),
                name=pin.get("pin", {}).get("name", ""),
                service=self.name,
                created=pin.get("created", ""),
                delegates=pin.get("delegates", []),
                info=pin,
            )
        return None

    def list_pins(self, limit: int = 100) -> List[PinStatus]:
        """List pinned content."""
        result = self._request("GET", f"/pins?limit={limit}")
        if not result:
            return []

        pins = []
        for pin in result.get("results", []):
            pins.append(PinStatus(
                cid=pin.get("pin", {}).get("cid", ""),
                status=pin.get("status", ""),
                name=pin.get("pin", {}).get("name", ""),
                service=self.name,
                created=pin.get("created", ""),
                delegates=pin.get("delegates", []),
                info=pin,
            ))
        return pins


# ============================================================================
# Pinning Manager
# ============================================================================

class PinningManager:
    """Manages multiple pinning services."""

    SERVICE_CLASSES = {
        "pinata": PinataService,
        "web3.storage": Web3StorageService,
        "ipfs-psa": IPFSPinningServiceAPI,
    }

    def __init__(self):
        self.config_file = PINNING_CONFIG_FILE
        self.services: Dict[str, PinningService] = {}
        self._load_config()

    def _load_config(self):
        """Load pinning service configurations."""
        if not self.config_file.exists():
            return

        try:
            data = json.loads(self.config_file.read_text())
            for name, svc_data in data.get("services", {}).items():
                config = ServiceConfig.from_dict(svc_data)
                self._create_service(name, config)
        except Exception as e:
            print(f"Error loading pinning config: {e}")

    def _save_config(self):
        """Save pinning service configurations."""
        self.config_file.parent.mkdir(parents=True, exist_ok=True)

        # Note: We don't have direct access to configs after creating services
        # This would need refactoring to store configs separately
        print("Config saved")

    def _create_service(self, name: str, config: ServiceConfig):
        """Create a service instance from config."""
        # Determine service type from name or endpoint
        service_type = config.extra.get("type", "")

        if not service_type:
            if "pinata" in name.lower() or "pinata" in config.endpoint.lower():
                service_type = "pinata"
            elif "web3" in name.lower() or "web3" in config.endpoint.lower():
                service_type = "web3.storage"
            else:
                service_type = "ipfs-psa"

        cls = self.SERVICE_CLASSES.get(service_type, IPFSPinningServiceAPI)
        self.services[name] = cls(config)

    def add_service(self, name: str, service_type: str, endpoint: str,
                    api_key: str, secret: str = "") -> bool:
        """Add a new pinning service."""
        config = ServiceConfig(
            name=name,
            endpoint=endpoint,
            api_key=api_key,
            secret=secret,
            extra={"type": service_type},
        )

        cls = self.SERVICE_CLASSES.get(service_type, IPFSPinningServiceAPI)
        self.services[name] = cls(config)

        # Save to config
        if not self.config_file.exists():
            data = {"services": {}}
        else:
            data = json.loads(self.config_file.read_text())

        data["services"][name] = config.to_dict()
        self.config_file.parent.mkdir(parents=True, exist_ok=True)
        self.config_file.write_text(json.dumps(data, indent=2))

        return True

    def remove_service(self, name: str) -> bool:
        """Remove a pinning service."""
        if name not in self.services:
            return False

        del self.services[name]

        if self.config_file.exists():
            data = json.loads(self.config_file.read_text())
            if name in data.get("services", {}):
                del data["services"][name]
                self.config_file.write_text(json.dumps(data, indent=2))

        return True

    def pin(self, cid: str, name: str = "", services: List[str] = None) -> Dict[str, PinStatus]:
        """Pin content to specified services (or all)."""
        results = {}
        target_services = services or list(self.services.keys())

        for svc_name in target_services:
            if svc_name not in self.services:
                print(f"Unknown service: {svc_name}")
                continue

            print(f"Pinning to {svc_name}...")
            service = self.services[svc_name]
            status = service.pin_by_cid(cid, name)

            if status:
                results[svc_name] = status
                print(f"  Status: {status.status}")
            else:
                print(f"  Failed")

        return results

    def pin_file(self, path: Path, name: str = "",
                 services: List[str] = None) -> Dict[str, PinStatus]:
        """Pin a local file to specified services."""
        results = {}
        target_services = services or list(self.services.keys())

        for svc_name in target_services:
            if svc_name not in self.services:
                continue

            print(f"Pinning to {svc_name}...")
            service = self.services[svc_name]
            status = service.pin_file(path, name)

            if status:
                results[svc_name] = status
                print(f"  CID: {status.cid}")
                print(f"  Status: {status.status}")
            else:
                print(f"  Failed")

        return results

    def unpin(self, cid: str, services: List[str] = None) -> Dict[str, bool]:
        """Unpin content from specified services."""
        results = {}
        target_services = services or list(self.services.keys())

        for svc_name in target_services:
            if svc_name not in self.services:
                continue

            print(f"Unpinning from {svc_name}...")
            service = self.services[svc_name]
            success = service.unpin(cid)
            results[svc_name] = success
            print(f"  {'Success' if success else 'Failed'}")

        return results

    def status(self, cid: str) -> Dict[str, PinStatus]:
        """Get pin status across all services."""
        results = {}

        for svc_name, service in self.services.items():
            status = service.get_status(cid)
            if status:
                results[svc_name] = status

        return results

    def list_services(self) -> List[str]:
        """List configured services."""
        return list(self.services.keys())


# ============================================================================
# CLI Interface
# ============================================================================

def cmd_service_add(args):
    """Add a pinning service."""
    if len(args) < 4:
        print("Usage: limn pin service add <name> <type> <endpoint> <api-key> [secret]")
        print()
        print("Types: pinata, web3.storage, ipfs-psa")
        print()
        print("Examples:")
        print("  limn pin service add my-pinata pinata https://api.pinata.cloud <key> <secret>")
        print("  limn pin service add my-web3 web3.storage https://api.web3.storage <token>")
        return

    name = args[0]
    svc_type = args[1]
    endpoint = args[2]
    api_key = args[3]
    secret = args[4] if len(args) > 4 else ""

    manager = PinningManager()
    if manager.add_service(name, svc_type, endpoint, api_key, secret):
        print(f"Added pinning service: {name}")
    else:
        print("Failed to add service")


def cmd_service_list(args):
    """List pinning services."""
    manager = PinningManager()
    services = manager.list_services()

    if not services:
        print("No pinning services configured")
        print("Add one with: limn pin service add <name> <type> <endpoint> <api-key>")
        return

    print("Configured pinning services:")
    for name in services:
        print(f"  {name}")


def cmd_pin(args):
    """Pin content."""
    if not args:
        print("Usage: limn pin <cid-or-path> [--name <name>] [--service <svc>]")
        return

    target = args[0]
    name = ""
    services = None

    # Parse options
    i = 1
    while i < len(args):
        if args[i] == "--name" and i + 1 < len(args):
            name = args[i + 1]
            i += 2
        elif args[i] == "--service" and i + 1 < len(args):
            services = [args[i + 1]]
            i += 2
        else:
            i += 1

    manager = PinningManager()

    if not manager.services:
        print("No pinning services configured")
        return

    # Check if target is a path or CID
    path = Path(target)
    if path.exists():
        results = manager.pin_file(path, name or path.name, services)
    else:
        # Assume it's a CID
        results = manager.pin(target, name or target[:16], services)

    if results:
        print(f"\nPinned to {len(results)} service(s)")
    else:
        print("Failed to pin to any service")


def cmd_status(args):
    """Check pin status."""
    if not args:
        print("Usage: limn pin status <cid>")
        return

    cid = args[0]
    manager = PinningManager()

    results = manager.status(cid)

    if not results:
        print(f"No pins found for: {cid}")
        return

    print(f"Pin status for {cid}:")
    for svc_name, status in results.items():
        print(f"  {svc_name}: {status.status}")
        if status.created:
            print(f"    Created: {status.created}")


def main():
    if len(sys.argv) < 2:
        print("Limn Remote Pinning")
        print()
        print("Service management:")
        print("  pin service add <name> <type> <endpoint> <api-key>")
        print("  pin service list")
        print("  pin service remove <name>")
        print()
        print("Pinning:")
        print("  pin <cid-or-path> [--name <name>] [--service <svc>]")
        print("  pin status <cid>")
        print("  pin unpin <cid>")
        print()
        print("Supported services: pinata, web3.storage, ipfs-psa")
        return

    cmd = sys.argv[1]
    args = sys.argv[2:]

    if cmd == "service":
        if args and args[0] == "add":
            cmd_service_add(args[1:])
        elif args and args[0] == "list":
            cmd_service_list(args[1:])
        elif args and args[0] == "remove":
            if len(args) > 1:
                manager = PinningManager()
                if manager.remove_service(args[1]):
                    print(f"Removed service: {args[1]}")
                else:
                    print("Service not found")
            else:
                print("Usage: limn pin service remove <name>")
        else:
            print("Unknown service command")
    elif cmd == "status":
        cmd_status(args)
    elif cmd == "unpin":
        if args:
            manager = PinningManager()
            manager.unpin(args[0])
        else:
            print("Usage: limn pin unpin <cid>")
    else:
        # Default: pin command
        cmd_pin([cmd] + args)


if __name__ == "__main__":
    main()
