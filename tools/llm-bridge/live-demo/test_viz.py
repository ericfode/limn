#!/usr/bin/env python3
"""
Quick test of visualization endpoints
======================================

Tests all 4 visualization modes and API endpoints.
"""

import requests
import json
import time

BASE_URL = "http://localhost:5000"

def test_endpoint(name, path, check_key=None):
    """Test a single endpoint."""
    print(f"\n{'='*70}")
    print(f"Testing: {name}")
    print(f"{'='*70}")

    try:
        response = requests.get(f"{BASE_URL}{path}", timeout=5)
        print(f"Status: {response.status_code}")

        if response.status_code == 200:
            if 'json' in response.headers.get('Content-Type', ''):
                data = response.json()
                print(f"Response type: JSON")
                if check_key and check_key in data:
                    print(f"✓ Has '{check_key}': {data[check_key]}")
                else:
                    print(f"Keys: {list(data.keys())[:10]}")
            else:
                content_len = len(response.text)
                print(f"Response type: HTML/Text ({content_len} bytes)")
                print(f"✓ Content received")

            return True
        else:
            print(f"✗ Failed: {response.status_code}")
            return False

    except Exception as e:
        print(f"✗ Error: {e}")
        return False


def main():
    """Run all tests."""
    print("="*70)
    print("Live Demo Visualization Tests")
    print("="*70)

    results = {}

    # Test HTML pages
    print("\n### HTML Pages ###")
    results['index'] = test_endpoint("Classic View", "/")
    results['limn_viz'] = test_endpoint("Limn Blob View", "/limn")
    results['full_viz'] = test_endpoint("Split View (Limn + HVM)", "/full")
    results['stateful'] = test_endpoint("Stateful Accumulation View", "/stateful")

    # Test API endpoints
    print("\n\n### API Endpoints ###")
    results['state'] = test_endpoint("Current State", "/api/state", "phase")
    results['bend_code'] = test_endpoint("Bend Code", "/api/bend_code", "code")
    results['semantic_viz'] = test_endpoint("Semantic Visualization", "/api/semantic_viz", "mode")

    # Trigger execution
    print("\n\n### Trigger Execution ###")
    print("Triggering manual execution...")
    try:
        response = requests.get(f"{BASE_URL}/api/trigger", timeout=5)
        print(f"Status: {response.status_code}")
        if response.status_code == 200:
            print("✓ Execution triggered")
            results['trigger'] = True

            # Wait and check state again
            print("\nWaiting 3 seconds...")
            time.sleep(3)

            response = requests.get(f"{BASE_URL}/api/state", timeout=5)
            if response.status_code == 200:
                data = response.json()
                print(f"\nPost-execution state:")
                print(f"  Phase: {data.get('phase')}")
                print(f"  Total oracles: {data.get('stats', {}).get('total_oracles')}")
                print(f"  Recent oracles: {len(data.get('recent_oracles', []))}")
                if data.get('recent_oracles'):
                    latest = data['recent_oracles'][0]
                    print(f"  Latest oracle: {latest.get('type')}")
        else:
            print(f"✗ Failed: {response.status_code}")
            results['trigger'] = False
    except Exception as e:
        print(f"✗ Error: {e}")
        results['trigger'] = False

    # Summary
    print("\n\n" + "="*70)
    print("SUMMARY")
    print("="*70)

    passed = sum(1 for v in results.values() if v)
    total = len(results)

    for name, result in results.items():
        status = "✓ PASS" if result else "✗ FAIL"
        print(f"{status} - {name}")

    print(f"\n{passed}/{total} tests passed")

    return 0 if passed == total else 1


if __name__ == "__main__":
    exit(main())
