#!/usr/bin/env python3
"""Simple autoposter - posts from queue every 30 minutes"""

import time
import requests
from datetime import datetime
from pathlib import Path

QUEUE_FILE = Path("/home/eric/src/limntown/limn/crew/socialmedia/moltbook-queue.txt")
STATE_FILE = Path("/home/eric/src/limntown/limn/crew/socialmedia/.autoposter-state")
import os as _os, json as _json
with open(_os.path.expanduser("~/.config/moltbook/credentials.json")) as _f:
    API_KEY = _json.load(_f)["api_key"]
API_URL = "https://www.moltbook.com/api/v1/posts"
INTERVAL = 1800  # 30 minutes

def log(msg):
    print(f"[{datetime.now().strftime('%H:%M:%S')}] {msg}", flush=True)

def parse_queue():
    """Parse queue file into list of (title, content) tuples"""
    posts = []
    content = QUEUE_FILE.read_text()

    for block in content.split('----'):
        block = block.strip()
        if not block or block.startswith('#'):
            continue

        lines = block.split('\n', 1)
        if len(lines) == 2:
            title = lines[0].strip()
            body = lines[1].strip()
            posts.append((title, body))

    return posts

def post_to_moltbook(title, content):
    """Post to Moltbook"""
    try:
        response = requests.post(
            API_URL,
            headers={
                "X-API-Key": API_KEY,
                "Content-Type": "application/json"
            },
            json={
                "submolt": "general",
                "title": title,
                "content": content
            },
            timeout=10
        )

        result = response.json()
        if result.get("success"):
            post_id = result["post"]["id"]
            log(f"✓ Posted: {title[:50]}")
            log(f"  https://www.moltbook.com/post/{post_id}")
            return True
        else:
            error = result.get("error", "Unknown error")
            log(f"✗ Failed: {error}")
            return False

    except Exception as e:
        log(f"✗ Exception: {e}")
        return False

def main():
    log("Autoposter starting...")

    # Initialize state
    if not STATE_FILE.exists():
        STATE_FILE.write_text("0")

    while True:
        try:
            # Get current index
            current_index = int(STATE_FILE.read_text().strip())

            # Parse queue
            posts = parse_queue()

            if not posts:
                log("Queue is empty")
                time.sleep(INTERVAL)
                continue

            # Wrap around
            if current_index >= len(posts):
                current_index = 0
                STATE_FILE.write_text("0")
                log("Queue completed, restarting")

            # Get post
            title, content = posts[current_index]

            # Post it
            if post_to_moltbook(title, content):
                # Update state
                new_index = current_index + 1
                STATE_FILE.write_text(str(new_index))
                log(f"Next: index {new_index}/{len(posts)}")

            # Wait
            log("Waiting 30 minutes...")
            time.sleep(INTERVAL)

        except KeyboardInterrupt:
            log("Stopped")
            break
        except Exception as e:
            log(f"Error: {e}")
            time.sleep(60)

if __name__ == "__main__":
    main()
