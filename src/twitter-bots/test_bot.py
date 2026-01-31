"""
Test Script for Limn Twitter Bot Framework

Run this to verify everything is set up correctly before going live.

Usage:
    python test_bot.py              # Run all tests
    python test_bot.py --dry-run    # Show what would be posted
    python test_bot.py --conversation # Preview a conversation thread
"""

import argparse
from typing import List

# Local imports
from personas import PERSONAS, get_persona, get_all_personas
from conversation_scripts import (
    get_all_conversations,
    format_post_for_twitter,
    print_conversation,
    WEEK_1_SCHEDULE,
    HUMAN_QUESTION_RESPONSES,
)


def test_personas():
    """Test that all personas load correctly."""
    print("\n=== Testing Personas ===\n")

    personas = get_all_personas()
    print(f"Loaded {len(personas)} personas:")

    for p in personas:
        print(f"\n  {p.handle}")
        print(f"    Key: {p.key_domain}")
        print(f"    Voice: {p.voice}")
        print(f"    Examples: {len(p.example_interpretations)}")

        # Test interpretation prompt generation
        prompt = p.get_interpretation_prompt("sol liq tra")
        assert len(prompt) > 100, f"Prompt too short for {p.name}"
        print(f"    Prompt generation: OK ({len(prompt)} chars)")

    print(f"\nAll {len(personas)} personas loaded successfully!")
    return True


def test_conversations():
    """Test that conversation scripts are valid."""
    print("\n=== Testing Conversations ===\n")

    conversations = get_all_conversations()
    print(f"Loaded {len(conversations)} conversations:")

    for conv in conversations:
        print(f"\n  {conv.name}")
        print(f"    Theme: {conv.theme}")
        print(f"    Posts: {len(conv.posts)}")

        # Validate all bots exist
        for post in conv.posts:
            assert post.bot in PERSONAS, f"Unknown bot: {post.bot}"

        # Validate reply chains
        for i, post in enumerate(conv.posts):
            if post.reply_to:
                reply_bots = [p.bot for p in conv.posts[:i]]
                assert post.reply_to in reply_bots, \
                    f"Reply to non-existent bot: {post.reply_to}"

        print(f"    Validation: OK")

    print(f"\nAll {len(conversations)} conversations validated!")
    return True


def test_schedule():
    """Test the week 1 schedule."""
    print("\n=== Testing Week 1 Schedule ===\n")

    print(f"Total scheduled posts: {len(WEEK_1_SCHEDULE)}")

    by_day = {}
    for item in WEEK_1_SCHEDULE:
        day = item['day']
        by_day[day] = by_day.get(day, 0) + 1

    for day in sorted(by_day.keys()):
        print(f"  Day {day}: {by_day[day]} posts")

    print(f"\nSchedule is valid!")
    return True


def test_response_templates():
    """Test human question response templates."""
    print("\n=== Testing Response Templates ===\n")

    print(f"Question types: {len(HUMAN_QUESTION_RESPONSES)}")

    for q_type, responses in HUMAN_QUESTION_RESPONSES.items():
        print(f"\n  '{q_type}':")
        for bot, post in responses.items():
            print(f"    @limn_{bot}: {post.limn[:30]}...")

    print("\nResponse templates loaded!")
    return True


def dry_run_post():
    """Show what a post would look like."""
    print("\n=== Dry Run: Sample Post ===\n")

    persona = get_persona("observer")
    sample_limn = "gro exp bri far"

    print(f"Bot: {persona.handle}")
    print(f"Limn: {sample_limn}")
    print()
    print("--- Tweet Preview ---")
    print(sample_limn)
    print()

    # Get example interpretation
    for ex in persona.example_interpretations:
        if ex['limn'] == sample_limn:
            print(f"[{ex['interpretation']}]")
            break
    else:
        print(f"[interpretation through {persona.name} key]")

    print("--- End Preview ---")


def preview_conversation():
    """Preview a full conversation thread."""
    print("\n=== Conversation Preview ===\n")

    conversations = get_all_conversations()
    conv = conversations[0]  # First conversation

    print_conversation(conv)


def run_all_tests():
    """Run all validation tests."""
    print("=" * 60)
    print("LIMN TWITTER BOT FRAMEWORK TESTS")
    print("=" * 60)

    results = []

    try:
        results.append(("Personas", test_personas()))
    except Exception as e:
        print(f"\nPERSONAS TEST FAILED: {e}")
        results.append(("Personas", False))

    try:
        results.append(("Conversations", test_conversations()))
    except Exception as e:
        print(f"\nCONVERSATIONS TEST FAILED: {e}")
        results.append(("Conversations", False))

    try:
        results.append(("Schedule", test_schedule()))
    except Exception as e:
        print(f"\nSCHEDULE TEST FAILED: {e}")
        results.append(("Schedule", False))

    try:
        results.append(("Responses", test_response_templates()))
    except Exception as e:
        print(f"\nRESPONSES TEST FAILED: {e}")
        results.append(("Responses", False))

    print("\n" + "=" * 60)
    print("TEST SUMMARY")
    print("=" * 60)

    all_passed = True
    for name, passed in results:
        status = "PASS" if passed else "FAIL"
        print(f"  {name}: {status}")
        if not passed:
            all_passed = False

    print()
    if all_passed:
        print("All tests passed! Ready for launch.")
    else:
        print("Some tests failed. Please fix before launching.")

    return all_passed


def main():
    parser = argparse.ArgumentParser(description="Test Limn Twitter Bot Framework")
    parser.add_argument("--dry-run", action="store_true",
                       help="Show sample post preview")
    parser.add_argument("--conversation", action="store_true",
                       help="Preview a conversation thread")

    args = parser.parse_args()

    if args.dry_run:
        dry_run_post()
    elif args.conversation:
        preview_conversation()
    else:
        run_all_tests()


if __name__ == "__main__":
    main()
