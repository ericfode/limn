#!/usr/bin/env python3
"""
Moltbook Auto-Poster
Posts queued content at 30-minute intervals
"""

import json
import time
import requests
from datetime import datetime
from pathlib import Path

QUEUE_FILE = Path("/home/eric/src/limntown/limn/crew/socialmedia/moltbook-post-queue.json")
import os as _os
with open(_os.path.expanduser("~/.config/moltbook/credentials.json")) as _f:
    API_KEY = json.load(_f)["api_key"]
API_URL = "https://www.moltbook.com/api/v1/posts"
INTERVAL = 30 * 60  # 30 minutes in seconds

def post_item(title, content, submolt):
    """Post a single item to Moltbook"""
    print(f"Posting: {title}")

    headers = {
        "X-API-Key": API_KEY,
        "Content-Type": "application/json"
    }

    data = {
        "submolt": submolt,
        "title": title,
        "content": content
    }

    try:
        response = requests.post(API_URL, headers=headers, json=data)
        result = response.json()

        if result.get("success"):
            post_id = result["post"]["id"]
            created_at = result["post"]["created_at"]
            print(f"✓ Success! Post ID: {post_id}")
            print(f"  URL: https://www.moltbook.com/post/{post_id}")
            return True, post_id, created_at, 0
        else:
            error = result.get("error", "Unknown error")
            retry_after = result.get("retry_after_minutes", 0)
            print(f"✗ Failed: {error}")
            if retry_after:
                print(f"  Retry after: {retry_after} minutes")
            return False, None, None, retry_after

    except Exception as e:
        print(f"✗ Exception: {e}")
        return False, None, None, 0

def main():
    print("Moltbook Auto-Poster Starting...")
    print(f"Queue: {QUEUE_FILE}")
    print(f"Interval: 30 minutes")
    print()

    # Load queue
    with open(QUEUE_FILE, 'r') as f:
        queue = json.load(f)

    posted = 0

    for i, item in enumerate(queue):
        if item.get("status") != "queued":
            continue

        title = item["title"]
        content = item["content"]
        submolt = item["submolt"]

        success, post_id, created_at, retry_after = post_item(title, content, submolt)

        if success:
            # Update status
            item["status"] = "posted"
            item["post_id"] = post_id
            item["posted_at"] = created_at

            # Save updated queue
            with open(QUEUE_FILE, 'w') as f:
                json.dump(queue, f, indent=2)

            posted += 1

            # Wait before next post (unless this is the last one)
            remaining = sum(1 for x in queue[i+1:] if x.get("status") == "queued")
            if remaining > 0:
                print()
                print(f"Waiting 30 minutes before next post... ({remaining} remaining)")
                time.sleep(INTERVAL)
        else:
            # If rate limited, wait and retry
            if retry_after > 0:
                print(f"Rate limited. Waiting {retry_after} minutes before retrying...")
                time.sleep(retry_after * 60 + 10)  # Add 10 seconds buffer

                # Retry this item
                success, post_id, created_at, retry_after2 = post_item(title, content, submolt)
                if success:
                    item["status"] = "posted"
                    item["post_id"] = post_id
                    item["posted_at"] = created_at
                    with open(QUEUE_FILE, 'w') as f:
                        json.dump(queue, f, indent=2)
                    posted += 1
                    # Continue to next item
                    remaining = sum(1 for x in queue[i+1:] if x.get("status") == "queued")
                    if remaining > 0:
                        print()
                        print(f"Waiting 30 minutes before next post... ({remaining} remaining)")
                        time.sleep(INTERVAL)
                else:
                    print("Retry failed, stopping.")
                    break
            else:
                print("Post failed (non-rate-limit error), stopping.")
                break

    print()
    print(f"Auto-posting complete. Posted {posted} items.")

if __name__ == "__main__":
    main()
