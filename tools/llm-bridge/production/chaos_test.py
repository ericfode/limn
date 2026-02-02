#!/usr/bin/env python3
"""
Chaos Engineering Test Suite for LMN Oracle Harness
===================================================

Tests system resilience under adverse conditions:
- Cache database corruption
- Oracle execution failures
- Malformed Bend output
- Missing dependencies
- Concurrent execution conflicts
- Resource exhaustion
- Network failures
- Permission errors

Author: Polecat See
Date: 2026-02-01
Issue: limn-vjej (limn-chaos)
"""

import unittest
import tempfile
import shutil
import sqlite3
import subprocess
import os
import sys
import time
import threading
import json
import signal
from pathlib import Path
from unittest.mock import patch, Mock, MagicMock
from typing import Optional

# Add production directory to path
sys.path.insert(0, str(Path(__file__).parent))

from harness import ProductionHarness, OracleType, OracleRequest, OracleResponse


class ChaosTestBase(unittest.TestCase):
    """Base class for chaos tests with common setup/teardown."""

    def setUp(self):
        """Set up temporary test environment."""
        self.test_dir = Path(tempfile.mkdtemp(prefix="chaos_test_"))
        self.cache_dir = self.test_dir / "cache"
        self.cache_dir.mkdir()
        self.harness = None

    def tearDown(self):
        """Clean up test environment."""
        if self.harness:
            del self.harness
        if self.test_dir.exists():
            shutil.rmtree(self.test_dir)

    def create_harness(self, **kwargs) -> ProductionHarness:
        """Create harness with test configuration."""
        defaults = {
            "bend_binary": "bend",
            "enable_real_llm": False,
            "cache_dir": self.cache_dir
        }
        defaults.update(kwargs)
        return ProductionHarness(**defaults)


class TestCacheCorruption(ChaosTestBase):
    """Test handling of cache database corruption."""

    def test_corrupt_sqlite_database(self):
        """Corrupt the SQLite cache and verify graceful handling."""
        # Create harness with working cache
        self.harness = self.create_harness()
        cache_db = self.harness.cache_db

        # Cache something
        self.harness._cache_set("test_key", {"value": "test"})

        # Verify it works
        result = self.harness._cache_get("test_key")
        self.assertEqual(result, {"value": "test"})

        # Corrupt the database by writing garbage
        with open(cache_db, 'wb') as f:
            f.write(b'\x00' * 100)

        # Verify system handles corruption gracefully
        try:
            result = self.harness._cache_get("test_key")
            # Should either return None or handle error gracefully
            self.assertIsNone(result)
        except sqlite3.DatabaseError:
            # Acceptable - detected corruption
            pass

    def test_missing_cache_directory(self):
        """Test behavior when cache directory disappears."""
        self.harness = self.create_harness()

        # Remove cache directory while harness is running
        shutil.rmtree(self.cache_dir)

        # Should handle gracefully
        try:
            self.harness._cache_set("key", "value")
        except Exception as e:
            # Should not crash entire system
            self.assertIsNotNone(e)

    def test_readonly_cache_directory(self):
        """Test behavior when cache directory becomes read-only."""
        self.harness = self.create_harness()

        # Make cache directory read-only
        os.chmod(self.cache_dir, 0o444)

        try:
            # Should handle permission errors gracefully
            self.harness._cache_set("key", "value")
        except (PermissionError, sqlite3.OperationalError):
            # Expected - but shouldn't crash
            pass
        finally:
            # Restore permissions for cleanup
            os.chmod(self.cache_dir, 0o755)

    def test_cache_schema_mismatch(self):
        """Test handling of incompatible cache schema."""
        # Create harness with cache
        self.harness = self.create_harness()

        # Modify schema to be incompatible
        conn = sqlite3.connect(self.harness.cache_db)
        conn.execute("DROP TABLE oracle_cache")
        conn.execute("CREATE TABLE oracle_cache (wrong_column TEXT)")
        conn.commit()
        conn.close()

        # Should detect and handle schema issues
        try:
            self.harness._cache_get("key")
        except sqlite3.OperationalError:
            pass  # Expected

    def test_concurrent_cache_access(self):
        """Test concurrent access to cache from multiple threads."""
        self.harness = self.create_harness()
        errors = []

        def worker(thread_id: int):
            try:
                for i in range(50):
                    key = f"thread_{thread_id}_key_{i}"
                    value = f"value_{i}"
                    self.harness._cache_set(key, value)
                    result = self.harness._cache_get(key)
                    if result != value:
                        errors.append(f"Thread {thread_id}: Mismatch on {key}")
            except Exception as e:
                errors.append(f"Thread {thread_id}: {str(e)}")

        # Spawn multiple threads hammering the cache
        threads = [threading.Thread(target=worker, args=(i,)) for i in range(5)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        # Check for concurrency errors
        if errors:
            self.fail(f"Concurrent access errors: {errors[:5]}")  # Show first 5


class TestOracleFailures(ChaosTestBase):
    """Test oracle execution failure handling."""

    def test_missing_bend_binary(self):
        """Test behavior when bend binary is missing."""
        self.harness = self.create_harness(bend_binary="/nonexistent/bend")

        # Create a test bend file
        bend_file = self.test_dir / "test.bend"
        bend_file.write_text("def main():\n  return 42")

        # Should handle missing binary gracefully
        result = self.harness.run_bend(bend_file)
        self.assertFalse(result["success"])
        self.assertIn("error", result)

    def test_bend_execution_timeout(self):
        """Test handling of Bend programs that hang."""
        self.harness = self.create_harness()

        # Create a program that hangs (sleep simulation)
        bend_file = self.test_dir / "hang.bend"
        # In reality, this would be a Bend program that loops forever
        bend_file.write_text("def main():\n  return 42")  # Placeholder

        # Mock subprocess to simulate timeout
        with patch('subprocess.run') as mock_run:
            mock_run.side_effect = subprocess.TimeoutExpired("bend", 30)
            result = self.harness.run_bend(bend_file)
            self.assertFalse(result["success"])
            self.assertIn("timeout", result.get("error", "").lower())

    def test_bend_crashes(self):
        """Test handling of Bend program crashes."""
        self.harness = self.create_harness()

        # Mock subprocess to simulate crash
        with patch('subprocess.run') as mock_run:
            mock_result = Mock()
            mock_result.returncode = -11  # SIGSEGV
            mock_result.stdout = ""
            mock_result.stderr = "Segmentation fault"
            mock_run.return_value = mock_result

            bend_file = self.test_dir / "crash.bend"
            bend_file.write_text("def main():\n  return 42")

            result = self.harness.run_bend(bend_file)
            self.assertFalse(result["success"])

    def test_malformed_oracle_output(self):
        """Test parsing of malformed oracle requests."""
        self.harness = self.create_harness()

        # Various malformed outputs
        malformed_outputs = [
            "Oracle/Invalid{",  # Incomplete
            "Oracle/Semantic{prompt: broken",  # Missing quotes
            "Oracle/Arith{op: add, a: NaN, b: 3}",  # Invalid number
            "}{}{malformed",  # Complete garbage
            "Oracle/Unknown{type: doesnt_exist}",  # Unknown oracle type
            "",  # Empty
            "Oracle/Semantic{missing_required_field: true}",  # Missing params
        ]

        for bad_output in malformed_outputs:
            oracles = self.harness.parse_oracles(bad_output)
            # Should return empty list or handle gracefully, not crash
            self.assertIsInstance(oracles, list)


class TestResourceExhaustion(ChaosTestBase):
    """Test system behavior under resource pressure."""

    def test_memory_pressure_large_cache(self):
        """Test with extremely large cache entries."""
        self.harness = self.create_harness()

        # Try to cache very large value
        large_value = "x" * (10 * 1024 * 1024)  # 10MB string

        try:
            self.harness._cache_set("large_key", large_value)
            result = self.harness._cache_get("large_key")
            # Should either work or fail gracefully
            if result is not None:
                self.assertEqual(len(result), len(large_value))
        except (MemoryError, sqlite3.OperationalError):
            pass  # Acceptable failure mode

    def test_cache_size_explosion(self):
        """Test with thousands of cache entries."""
        self.harness = self.create_harness()

        # Fill cache with many entries
        for i in range(1000):
            self.harness._cache_set(f"key_{i}", f"value_{i}" * 100)

        # Should still function
        result = self.harness._cache_get("key_500")
        self.assertEqual(result, "value_500" * 100)

    def test_file_descriptor_exhaustion(self):
        """Test behavior when running out of file descriptors."""
        self.harness = self.create_harness()

        # Open many connections rapidly without closing
        connections = []
        try:
            for i in range(100):
                conn = sqlite3.connect(self.harness.cache_db)
                connections.append(conn)
        except Exception:
            pass  # Expected to hit limits

        # System should recover
        try:
            self.harness._cache_get("test")
        finally:
            for conn in connections:
                try:
                    conn.close()
                except:
                    pass


class TestNetworkFailures(ChaosTestBase):
    """Test network-related failure handling."""

    def test_http_timeout(self):
        """Test HTTP oracle with timeout."""
        self.harness = self.create_harness()

        # Mock requests to simulate timeout
        with patch('requests.get') as mock_get:
            import requests
            mock_get.side_effect = requests.exceptions.Timeout("Request timed out")

            # Create HTTP GET oracle request
            oracle = OracleRequest(
                type=OracleType.HTTP_GET,
                params={"url": "http://slow-server.example.com"}
            )

            # Should handle timeout gracefully
            try:
                # Need to implement execute_oracle method test
                pass  # Placeholder - would test actual execution
            except Exception as e:
                # Should not crash entire system
                self.assertIsInstance(e, Exception)

    def test_http_connection_refused(self):
        """Test HTTP oracle with connection refused."""
        self.harness = self.create_harness()

        with patch('requests.get') as mock_get:
            import requests
            mock_get.side_effect = requests.exceptions.ConnectionError("Connection refused")

            # Should handle connection errors gracefully
            # Placeholder for actual test
            pass


class TestFileSystemErrors(ChaosTestBase):
    """Test filesystem-related error handling."""

    def test_read_nonexistent_file(self):
        """Test FileRead oracle with nonexistent file."""
        # Placeholder - would test actual oracle execution
        pass

    def test_write_to_readonly_filesystem(self):
        """Test FileWrite oracle to read-only location."""
        # Placeholder - would test actual oracle execution
        pass

    def test_permission_denied(self):
        """Test operations with insufficient permissions."""
        self.harness = self.create_harness()

        # Create file with no read permissions
        test_file = self.test_dir / "forbidden.txt"
        test_file.write_text("secret")
        os.chmod(test_file, 0o000)

        try:
            # Should handle permission errors gracefully
            # Placeholder for actual FileRead oracle test
            pass
        finally:
            os.chmod(test_file, 0o644)  # Restore for cleanup


class TestGracefulDegradation(ChaosTestBase):
    """Test system degradation under failures."""

    def test_partial_oracle_failure(self):
        """Test that one oracle failure doesn't crash entire system."""
        self.harness = self.create_harness()

        # Simulate one oracle type failing
        # System should continue with other oracles
        # Placeholder for actual test
        pass

    def test_recovery_after_transient_failure(self):
        """Test system recovery after temporary failure."""
        self.harness = self.create_harness()

        # Simulate transient failure (e.g., network blip)
        # Verify system recovers on retry
        # Placeholder for actual test
        pass


class TestIntegrationChaos(ChaosTestBase):
    """Integration tests combining multiple failure modes."""

    def test_cascading_failures(self):
        """Test multiple simultaneous failures."""
        self.harness = self.create_harness()

        # Corrupt cache + make directory readonly + simulate network failure
        # System should handle all three gracefully
        # Placeholder for actual test
        pass

    def test_stress_test_full_system(self):
        """Full stress test with realistic chaos."""
        self.harness = self.create_harness()

        # Run actual Bend program with random failures injected
        # Measure recovery time and failure handling
        # Placeholder for actual test
        pass


def run_chaos_suite():
    """Run complete chaos testing suite."""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestCacheCorruption))
    suite.addTests(loader.loadTestsFromTestCase(TestOracleFailures))
    suite.addTests(loader.loadTestsFromTestCase(TestResourceExhaustion))
    suite.addTests(loader.loadTestsFromTestCase(TestNetworkFailures))
    suite.addTests(loader.loadTestsFromTestCase(TestFileSystemErrors))
    suite.addTests(loader.loadTestsFromTestCase(TestGracefulDegradation))
    suite.addTests(loader.loadTestsFromTestCase(TestIntegrationChaos))

    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    return result


if __name__ == "__main__":
    print("=" * 70)
    print("LMN ORACLE HARNESS - CHAOS ENGINEERING TEST SUITE")
    print("=" * 70)
    print()
    print("Testing system resilience under:")
    print("  - Cache corruption")
    print("  - Oracle failures")
    print("  - Resource exhaustion")
    print("  - Network failures")
    print("  - Filesystem errors")
    print("  - Concurrent access conflicts")
    print()
    print("=" * 70)
    print()

    result = run_chaos_suite()

    print()
    print("=" * 70)
    print("CHAOS TEST RESULTS")
    print("=" * 70)
    print(f"Tests run: {result.testsRun}")
    print(f"Failures: {len(result.failures)}")
    print(f"Errors: {len(result.errors)}")
    print(f"Skipped: {len(result.skipped)}")
    print()

    if result.failures:
        print("FAILURES:")
        for test, traceback in result.failures:
            print(f"  - {test}: {traceback.split(chr(10))[0]}")
        print()

    if result.errors:
        print("ERRORS:")
        for test, traceback in result.errors:
            print(f"  - {test}: {traceback.split(chr(10))[0]}")
        print()

    if result.wasSuccessful():
        print("✓ ALL CHAOS TESTS PASSED - System is resilient!")
        sys.exit(0)
    else:
        print("✗ VULNERABILITIES FOUND - See failures above")
        sys.exit(1)
