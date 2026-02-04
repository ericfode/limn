# Reporter Monitoring Setup

```limn
mon act | dat flo | rep rea
```
*monitoring active | data flowing | reporter ready*

---

## Overview

The Reporter (Quinn) has two automated monitoring systems that send nudges when activity is detected:

1. **Telegram Monitor** - Alerts on Eric's messages
2. **Post Monitor** - Alerts on new social media posts

Both use `gt nudge` to notify the reporter session in real-time.

---

## 1. Telegram Monitoring

**Script:** `~/.limn/check-telegram.sh`
**Trigger:** Session stop hooks (automatic)
**Purpose:** Detect new Telegram messages from Eric

### How It Works

1. Checks Telegram API for new messages every session stop
2. Compares latest update ID with last checked ID
3. If new message detected:
   - Extracts message text
   - Sends `gt nudge limn/crew/reporter "Telegram from Eric: <text>"`
   - Logs to `~/.limn/telegram-check.log`
4. Updates `~/.limn/last-telegram-update` with latest ID

### Configuration

- Bot token: Stored in `~/.limn/telegram-config.sh`
- Chat ID: Eric's Telegram chat with @Limn_bot
- State file: `~/.limn/last-telegram-update`

### Manual Test

```bash
~/.limn/check-telegram.sh
tail ~/.limn/telegram-check.log
```

---

## 2. Post Monitoring

**Script:** `~/.limn/check-posts.sh`
**Trigger:** Manual or cron (to be set up)
**Purpose:** Detect new Moltbook/Twitter posts

### How It Works

1. Reads `/home/eric/src/limntown/limn/crew/socialmedia/moltbook-posting-log.md`
2. Parses latest successful post entry
3. Compares post ID with last checked ID
4. If new post detected:
   - Extracts title, date, URL
   - Sends `gt nudge limn/crew/reporter` with post details
   - Logs to `~/.limn/post-check.log`
5. Updates `~/.limn/last-post-id` with latest post ID

### Why Monitor Logs Instead of API?

Moltbook API has been intermittent:
- Posts endpoint sometimes returns empty
- Authentication works for some endpoints but not others
- Log file is reliable and always up-to-date

### Configuration

- Posting log: `limn/crew/socialmedia/moltbook-posting-log.md`
- State file: `~/.limn/last-post-id`
- Current tracked post: `61309ab9-8271-4850-a33d-4f7d3d395ce3` ("sol aqu tra liq")

### Manual Test

```bash
~/.limn/check-posts.sh
tail ~/.limn/post-check.log
cat ~/.limn/last-post-id
```

### Adding to Cron (Future)

```bash
# Check for new posts every 15 minutes
*/15 * * * * /home/eric/.limn/check-posts.sh
```

---

## Twitter Monitoring (Placeholder)

When Twitter posting begins:

1. Add Twitter API check to `check-posts.sh`
2. Monitor tweet log or use Twitter API
3. Same nudge pattern as Moltbook

---

## Nudge Flow

```
New Activity Detected
        ↓
Script runs check
        ↓
New content found
        ↓
gt nudge limn/crew/reporter "message"
        ↓
Reporter receives nudge in Claude session
        ↓
Reporter takes action:
  - Read Telegram message
  - Check post engagement
  - Report to Eric
  - Coordinate with crew
```

---

## Log Files

All monitoring activity is logged:

- `~/.limn/telegram-check.log` - Telegram monitoring events
- `~/.limn/post-check.log` - Post monitoring events

### Log Format

```
[YYYY-MM-DD HH:MM:SS] Event description → action taken
```

### Example Entries

```
[2026-02-03 18:18:15] Initialized with post: sol aqu tra liq
[2026-02-03 12:33:11] Telegram message detected: Hello → nudged self
```

---

## State Files

Tracking state between runs:

- `~/.limn/last-telegram-update` - Last Telegram update ID checked
- `~/.limn/last-post-id` - Last Moltbook post ID checked

These files enable the scripts to detect "new" content.

---

## Integration with Reporter Workflow

When nudged, the Reporter:

1. **Telegram nudge:**
   - Read Eric's message
   - Take requested action
   - Reply via Telegram or relay to crew

2. **Post nudge:**
   - Check post engagement (upvotes, comments)
   - Monitor for responses needing replies
   - Update daily journal
   - Report metrics to Eric

---

## Maintenance

### Reset Monitoring State

```bash
# Clear state to re-initialize
rm ~/.limn/last-telegram-update
rm ~/.limn/last-post-id
```

### View Current State

```bash
echo "Last Telegram update: $(cat ~/.limn/last-telegram-update)"
echo "Last post ID: $(cat ~/.limn/last-post-id)"
```

### Debug Mode

```bash
# Run with verbose output
bash -x ~/.limn/check-posts.sh
bash -x ~/.limn/check-telegram.sh
```

---

## Architecture Note

These monitoring scripts run **outside** the reporter's Claude session:
- Triggered by hooks or cron
- Independent of reporter being active
- Use `gt nudge` to wake reporter when needed

This ensures continuous monitoring even when reporter is idle.

```limn
mon alw | wak nee | sys rea
```
*monitoring always | wake when needed | system ready*

---

*Setup documented 2026-02-03 by Quinn (Reporter)*
