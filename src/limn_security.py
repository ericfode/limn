#!/usr/bin/env python3
"""
Limn Package Security - Signing and verification for packages.

Features:
- Ed25519 key generation and management
- Package signing (signs manifest hash)
- Signature verification
- Trusted publisher registry
- Content integrity verification
"""

import os
import sys
import json
import hashlib
import base64
from pathlib import Path
from typing import Dict, List, Optional, Tuple, Any
from dataclasses import dataclass, field
from datetime import datetime

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))

# Try to import cryptography library
try:
    from cryptography.hazmat.primitives import hashes
    from cryptography.hazmat.primitives.asymmetric import ed25519
    from cryptography.hazmat.primitives import serialization
    from cryptography.exceptions import InvalidSignature
    HAS_CRYPTO = True
except ImportError:
    HAS_CRYPTO = False


# ============================================================================
# Configuration
# ============================================================================

LIMN_HOME = Path.home() / ".limn"
KEYS_DIR = LIMN_HOME / "keys"
TRUSTED_PUBLISHERS_FILE = LIMN_HOME / "trusted_publishers.json"


# ============================================================================
# Data Structures
# ============================================================================

@dataclass
class KeyPair:
    """An Ed25519 key pair."""
    name: str
    public_key: bytes
    private_key: Optional[bytes] = None  # None if public-only
    created: str = ""

    def get_public_key_b64(self) -> str:
        """Get base64-encoded public key."""
        return base64.b64encode(self.public_key).decode('ascii')

    def get_fingerprint(self) -> str:
        """Get key fingerprint (first 16 chars of SHA256)."""
        h = hashlib.sha256(self.public_key).hexdigest()
        return h[:16]

    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "public_key": self.get_public_key_b64(),
            "created": self.created,
            "fingerprint": self.get_fingerprint(),
        }


@dataclass
class Signature:
    """A package signature."""
    package_hash: str  # SHA256 of package content
    signature: str  # Base64-encoded signature
    signer: str  # Public key fingerprint
    timestamp: str  # ISO timestamp
    algorithm: str = "ed25519"

    def to_dict(self) -> dict:
        return {
            "package_hash": self.package_hash,
            "signature": self.signature,
            "signer": self.signer,
            "timestamp": self.timestamp,
            "algorithm": self.algorithm,
        }

    @classmethod
    def from_dict(cls, d: dict) -> "Signature":
        return cls(
            package_hash=d.get("package_hash", ""),
            signature=d.get("signature", ""),
            signer=d.get("signer", ""),
            timestamp=d.get("timestamp", ""),
            algorithm=d.get("algorithm", "ed25519"),
        )

    def to_limn(self) -> str:
        """Generate Limn format signature block."""
        return f'''# pak.sig.limn - Package signature
whe sig_pkg_hash
whe sig_signature
whe sig_signer
whe sig_timestamp
whe sig_algorithm

---
sig_pkg_hash sa "{self.package_hash}"
sig_signature sa "{self.signature}"
sig_signer sa "{self.signer}"
sig_timestamp sa "{self.timestamp}"
sig_algorithm sa "{self.algorithm}"
'''


@dataclass
class TrustedPublisher:
    """A trusted package publisher."""
    name: str
    public_key: str  # Base64-encoded public key
    fingerprint: str
    added: str
    packages: List[str] = field(default_factory=list)  # Package name patterns

    def to_dict(self) -> dict:
        return {
            "name": self.name,
            "public_key": self.public_key,
            "fingerprint": self.fingerprint,
            "added": self.added,
            "packages": self.packages,
        }

    @classmethod
    def from_dict(cls, d: dict) -> "TrustedPublisher":
        return cls(
            name=d.get("name", ""),
            public_key=d.get("public_key", ""),
            fingerprint=d.get("fingerprint", ""),
            added=d.get("added", ""),
            packages=d.get("packages", []),
        )


# ============================================================================
# Key Management
# ============================================================================

class KeyManager:
    """Manages cryptographic keys."""

    def __init__(self):
        if not HAS_CRYPTO:
            print("Warning: cryptography library not available")
            print("Install with: pip install cryptography")

        self.keys_dir = KEYS_DIR
        self.keys_dir.mkdir(parents=True, exist_ok=True)

    def generate_key(self, name: str) -> Optional[KeyPair]:
        """Generate a new Ed25519 key pair."""
        if not HAS_CRYPTO:
            return None

        # Check if key already exists
        key_path = self.keys_dir / f"{name}.key"
        if key_path.exists():
            print(f"Key '{name}' already exists")
            return self.load_key(name)

        # Generate key
        private_key = ed25519.Ed25519PrivateKey.generate()
        public_key = private_key.public_key()

        # Serialize
        private_bytes = private_key.private_bytes(
            encoding=serialization.Encoding.Raw,
            format=serialization.PrivateFormat.Raw,
            encryption_algorithm=serialization.NoEncryption()
        )
        public_bytes = public_key.public_bytes(
            encoding=serialization.Encoding.Raw,
            format=serialization.PublicFormat.Raw
        )

        key_pair = KeyPair(
            name=name,
            public_key=public_bytes,
            private_key=private_bytes,
            created=datetime.utcnow().isoformat() + "Z",
        )

        # Save private key (encrypted would be better in production)
        key_data = {
            "name": name,
            "private_key": base64.b64encode(private_bytes).decode('ascii'),
            "public_key": base64.b64encode(public_bytes).decode('ascii'),
            "created": key_pair.created,
        }
        key_path.write_text(json.dumps(key_data, indent=2))

        # Save public key separately for sharing
        pub_path = self.keys_dir / f"{name}.pub"
        pub_data = {
            "name": name,
            "public_key": base64.b64encode(public_bytes).decode('ascii'),
            "fingerprint": key_pair.get_fingerprint(),
            "created": key_pair.created,
        }
        pub_path.write_text(json.dumps(pub_data, indent=2))

        print(f"Generated key: {name}")
        print(f"  Fingerprint: {key_pair.get_fingerprint()}")
        print(f"  Public key: {pub_path}")

        return key_pair

    def load_key(self, name: str, private: bool = True) -> Optional[KeyPair]:
        """Load a key pair."""
        if not HAS_CRYPTO:
            return None

        if private:
            key_path = self.keys_dir / f"{name}.key"
        else:
            key_path = self.keys_dir / f"{name}.pub"

        if not key_path.exists():
            return None

        try:
            data = json.loads(key_path.read_text())

            public_bytes = base64.b64decode(data["public_key"])
            private_bytes = None
            if private and "private_key" in data:
                private_bytes = base64.b64decode(data["private_key"])

            return KeyPair(
                name=data["name"],
                public_key=public_bytes,
                private_key=private_bytes,
                created=data.get("created", ""),
            )
        except Exception as e:
            print(f"Error loading key: {e}")
            return None

    def load_public_key_from_b64(self, b64_key: str, name: str = "imported") -> Optional[KeyPair]:
        """Load a public key from base64."""
        try:
            public_bytes = base64.b64decode(b64_key)
            return KeyPair(
                name=name,
                public_key=public_bytes,
            )
        except Exception:
            return None

    def list_keys(self) -> List[KeyPair]:
        """List all available keys."""
        keys = []
        for key_file in self.keys_dir.glob("*.key"):
            name = key_file.stem
            key = self.load_key(name)
            if key:
                keys.append(key)
        return keys

    def export_public_key(self, name: str) -> Optional[str]:
        """Export public key for sharing."""
        key = self.load_key(name, private=False)
        if key:
            return json.dumps(key.to_dict(), indent=2)
        return None


# ============================================================================
# Signing
# ============================================================================

class PackageSigner:
    """Signs packages."""

    def __init__(self, key_manager: KeyManager = None):
        self.key_manager = key_manager or KeyManager()

    def compute_package_hash(self, path: Path) -> str:
        """Compute SHA256 hash of package content."""
        if path.is_file():
            content = path.read_bytes()
            return hashlib.sha256(content).hexdigest()

        # For directories, hash manifest and all .limn files
        hasher = hashlib.sha256()

        manifest = path / "pak.limn"
        if manifest.exists():
            hasher.update(manifest.read_bytes())

        # Hash all source files in deterministic order
        src_dir = path / "src"
        if src_dir.exists():
            for limn_file in sorted(src_dir.rglob("*.limn")):
                hasher.update(limn_file.read_bytes())

        return hasher.hexdigest()

    def sign_package(self, path: Path, key_name: str) -> Optional[Signature]:
        """Sign a package with the specified key."""
        if not HAS_CRYPTO:
            print("Error: cryptography library not available")
            return None

        # Load key
        key = self.key_manager.load_key(key_name)
        if not key or not key.private_key:
            print(f"Error: Private key '{key_name}' not found")
            return None

        # Compute hash
        pkg_hash = self.compute_package_hash(path)

        # Create private key object
        private_key = ed25519.Ed25519PrivateKey.from_private_bytes(key.private_key)

        # Sign
        signature_bytes = private_key.sign(pkg_hash.encode('utf-8'))
        signature_b64 = base64.b64encode(signature_bytes).decode('ascii')

        sig = Signature(
            package_hash=pkg_hash,
            signature=signature_b64,
            signer=key.get_fingerprint(),
            timestamp=datetime.utcnow().isoformat() + "Z",
        )

        # Write signature file
        sig_path = path / "pak.sig.limn" if path.is_dir() else path.with_suffix(".sig.limn")
        sig_path.write_text(sig.to_limn())

        print(f"Signed package: {path}")
        print(f"  Hash: {pkg_hash[:16]}...")
        print(f"  Signer: {key.get_fingerprint()}")

        return sig

    def verify_signature(self, path: Path, trusted_keys: List[KeyPair] = None) -> Tuple[bool, str]:
        """
        Verify package signature.
        Returns (valid, message).
        """
        if not HAS_CRYPTO:
            return False, "cryptography library not available"

        # Find signature file
        sig_path = path / "pak.sig.limn" if path.is_dir() else path.with_suffix(".sig.limn")
        if not sig_path.exists():
            return False, "No signature file found"

        # Parse signature
        sig = self._parse_signature_file(sig_path)
        if not sig:
            return False, "Failed to parse signature"

        # Compute current hash
        current_hash = self.compute_package_hash(path)

        # Verify hash matches
        if current_hash != sig.package_hash:
            return False, f"Hash mismatch: package has been modified"

        # Find signer's public key
        signer_key = None

        # Check trusted keys
        if trusted_keys:
            for key in trusted_keys:
                if key.get_fingerprint() == sig.signer:
                    signer_key = key
                    break

        # Check local keys
        if not signer_key:
            for key in self.key_manager.list_keys():
                if key.get_fingerprint() == sig.signer:
                    signer_key = key
                    break

        # Check trusted publishers
        if not signer_key:
            trusted = self._load_trusted_publishers()
            for publisher in trusted:
                if publisher.fingerprint == sig.signer:
                    signer_key = self.key_manager.load_public_key_from_b64(
                        publisher.public_key, publisher.name
                    )
                    break

        if not signer_key:
            return False, f"Unknown signer: {sig.signer}"

        # Verify signature
        try:
            public_key = ed25519.Ed25519PublicKey.from_public_bytes(signer_key.public_key)
            signature_bytes = base64.b64decode(sig.signature)
            public_key.verify(signature_bytes, sig.package_hash.encode('utf-8'))
            return True, f"Valid signature by {signer_key.name} ({sig.signer})"
        except InvalidSignature:
            return False, "Invalid signature"
        except Exception as e:
            return False, f"Verification error: {e}"

    def _parse_signature_file(self, path: Path) -> Optional[Signature]:
        """Parse a pak.sig.limn file."""
        content = path.read_text()
        data = {}

        for line in content.split('\n'):
            line = line.strip()
            if not line or line.startswith('#') or line.startswith('whe '):
                continue

            # Parse: sig_field sa "value"
            import re
            match = re.match(r'sig_(\w+)\s+sa\s+"([^"]+)"', line)
            if match:
                field = match.group(1)
                value = match.group(2)
                data[field] = value

        if 'signature' in data and 'pkg_hash' in data:
            return Signature(
                package_hash=data.get('pkg_hash', ''),
                signature=data.get('signature', ''),
                signer=data.get('signer', ''),
                timestamp=data.get('timestamp', ''),
                algorithm=data.get('algorithm', 'ed25519'),
            )
        return None

    def _load_trusted_publishers(self) -> List[TrustedPublisher]:
        """Load trusted publishers list."""
        if not TRUSTED_PUBLISHERS_FILE.exists():
            return []

        try:
            data = json.loads(TRUSTED_PUBLISHERS_FILE.read_text())
            return [TrustedPublisher.from_dict(p) for p in data.get("publishers", [])]
        except Exception:
            return []


# ============================================================================
# Trusted Publishers
# ============================================================================

class TrustedPublisherManager:
    """Manages trusted publishers."""

    def __init__(self):
        self.file_path = TRUSTED_PUBLISHERS_FILE

    def load(self) -> List[TrustedPublisher]:
        """Load all trusted publishers."""
        if not self.file_path.exists():
            return []

        try:
            data = json.loads(self.file_path.read_text())
            return [TrustedPublisher.from_dict(p) for p in data.get("publishers", [])]
        except Exception:
            return []

    def save(self, publishers: List[TrustedPublisher]):
        """Save trusted publishers."""
        self.file_path.parent.mkdir(parents=True, exist_ok=True)
        data = {
            "version": 1,
            "updated": datetime.utcnow().isoformat() + "Z",
            "publishers": [p.to_dict() for p in publishers],
        }
        self.file_path.write_text(json.dumps(data, indent=2))

    def add_publisher(self, name: str, public_key: str,
                      packages: List[str] = None) -> TrustedPublisher:
        """Add a trusted publisher."""
        # Compute fingerprint
        try:
            key_bytes = base64.b64decode(public_key)
            fingerprint = hashlib.sha256(key_bytes).hexdigest()[:16]
        except Exception:
            fingerprint = ""

        publisher = TrustedPublisher(
            name=name,
            public_key=public_key,
            fingerprint=fingerprint,
            added=datetime.utcnow().isoformat() + "Z",
            packages=packages or ["*"],
        )

        publishers = self.load()

        # Check for duplicate
        for i, p in enumerate(publishers):
            if p.fingerprint == fingerprint:
                print(f"Updating existing publisher: {p.name}")
                publishers[i] = publisher
                self.save(publishers)
                return publisher

        publishers.append(publisher)
        self.save(publishers)
        return publisher

    def remove_publisher(self, fingerprint: str) -> bool:
        """Remove a trusted publisher by fingerprint."""
        publishers = self.load()
        original_len = len(publishers)
        publishers = [p for p in publishers if p.fingerprint != fingerprint]

        if len(publishers) < original_len:
            self.save(publishers)
            return True
        return False

    def is_trusted(self, fingerprint: str, package_name: str = None) -> bool:
        """Check if a publisher is trusted (optionally for a specific package)."""
        publishers = self.load()

        for p in publishers:
            if p.fingerprint == fingerprint:
                if package_name is None:
                    return True
                # Check package patterns
                for pattern in p.packages:
                    if pattern == "*":
                        return True
                    if pattern == package_name:
                        return True
                    # Simple glob matching
                    if pattern.endswith("*"):
                        if package_name.startswith(pattern[:-1]):
                            return True
        return False


# ============================================================================
# CLI Interface
# ============================================================================

def cmd_key_gen(args):
    """Generate a new key."""
    if not args:
        print("Usage: limn key gen <name>")
        return

    km = KeyManager()
    km.generate_key(args[0])


def cmd_key_list(args):
    """List keys."""
    km = KeyManager()
    keys = km.list_keys()

    if not keys:
        print("No keys found")
        print("Generate one with: limn key gen <name>")
        return

    print("Keys:")
    for key in keys:
        print(f"  {key.name}")
        print(f"    Fingerprint: {key.get_fingerprint()}")
        print(f"    Created: {key.created}")


def cmd_key_export(args):
    """Export a public key."""
    if not args:
        print("Usage: limn key export <name>")
        return

    km = KeyManager()
    pub = km.export_public_key(args[0])
    if pub:
        print(pub)
    else:
        print(f"Key not found: {args[0]}")


def cmd_sign(args):
    """Sign a package."""
    if len(args) < 2:
        print("Usage: limn sign <path> <key-name>")
        return

    path = Path(args[0])
    key_name = args[1]

    if not path.exists():
        print(f"Path not found: {path}")
        return

    signer = PackageSigner()
    signer.sign_package(path, key_name)


def cmd_verify(args):
    """Verify a package signature."""
    if not args:
        print("Usage: limn verify <path>")
        return

    path = Path(args[0])
    if not path.exists():
        print(f"Path not found: {path}")
        return

    signer = PackageSigner()
    valid, message = signer.verify_signature(path)

    if valid:
        print(f"✓ {message}")
    else:
        print(f"✗ {message}")


def cmd_trust_add(args):
    """Add a trusted publisher."""
    if len(args) < 2:
        print("Usage: limn trust add <name> <public-key-file-or-b64>")
        return

    name = args[0]
    key_arg = args[1]

    # Check if it's a file
    if Path(key_arg).exists():
        data = json.loads(Path(key_arg).read_text())
        public_key = data.get("public_key", "")
    else:
        public_key = key_arg

    # Optional package patterns
    packages = args[2:] if len(args) > 2 else ["*"]

    tm = TrustedPublisherManager()
    publisher = tm.add_publisher(name, public_key, packages)
    print(f"Added trusted publisher: {name}")
    print(f"  Fingerprint: {publisher.fingerprint}")


def cmd_trust_list(args):
    """List trusted publishers."""
    tm = TrustedPublisherManager()
    publishers = tm.load()

    if not publishers:
        print("No trusted publishers")
        return

    print("Trusted publishers:")
    for p in publishers:
        print(f"  {p.name}")
        print(f"    Fingerprint: {p.fingerprint}")
        print(f"    Packages: {', '.join(p.packages)}")


def main():
    if len(sys.argv) < 2:
        print("Limn Package Security")
        print()
        print("Key management:")
        print("  key gen <name>        Generate new signing key")
        print("  key list              List keys")
        print("  key export <name>     Export public key")
        print()
        print("Signing:")
        print("  sign <path> <key>     Sign a package")
        print("  verify <path>         Verify package signature")
        print()
        print("Trust management:")
        print("  trust add <name> <key>    Add trusted publisher")
        print("  trust list                List trusted publishers")
        print("  trust remove <fingerprint> Remove trusted publisher")
        return

    cmd = sys.argv[1]
    args = sys.argv[2:]

    if cmd == "key":
        if args and args[0] == "gen":
            cmd_key_gen(args[1:])
        elif args and args[0] == "list":
            cmd_key_list(args[1:])
        elif args and args[0] == "export":
            cmd_key_export(args[1:])
        else:
            print("Unknown key command")
    elif cmd == "sign":
        cmd_sign(args)
    elif cmd == "verify":
        cmd_verify(args)
    elif cmd == "trust":
        if args and args[0] == "add":
            cmd_trust_add(args[1:])
        elif args and args[0] == "list":
            cmd_trust_list(args[1:])
        elif args and args[0] == "remove":
            if len(args) > 1:
                tm = TrustedPublisherManager()
                if tm.remove_publisher(args[1]):
                    print("Publisher removed")
                else:
                    print("Publisher not found")
            else:
                print("Usage: limn trust remove <fingerprint>")
        else:
            print("Unknown trust command")
    else:
        print(f"Unknown command: {cmd}")


if __name__ == "__main__":
    main()
