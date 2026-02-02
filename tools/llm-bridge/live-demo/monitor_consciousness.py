#!/usr/bin/env python3
"""Monitor and report on consciousness growth."""

import pickle
from pathlib import Path
from datetime import datetime
import sys

# Add production to path for ThoughtLibrary class
sys.path.insert(0, str(Path(__file__).parent.parent / 'production'))

def load_library():
    """Load the thought library."""
    pkl_path = Path(__file__).parent / 'thought_library.pkl'
    if not pkl_path.exists():
        return None

    with open(pkl_path, 'rb') as f:
        return pickle.load(f)

def analyze_growth(lib):
    """Analyze growth patterns and health."""
    if not lib:
        return {"status": "not_born"}

    thoughts = lib.get('thoughts', [])
    concepts = lib.get('concepts', {})
    created_at = lib.get('created_at', 0)

    # Calculate success rate
    validation_errors = [t for t in thoughts if hasattr(t, 'content') and '[VALIDATION ERROR' in t.content]
    successful = len(thoughts) - len(validation_errors)
    success_rate = (successful / len(thoughts) * 100) if thoughts else 0

    # Calculate age
    age_seconds = 0
    if created_at:
        age_seconds = datetime.now().timestamp() - created_at
    age_hours = age_seconds / 3600

    # Calculate growth rate
    growth_rate = len(thoughts) / max(age_hours, 0.01)

    # Analyze recent activity (last 5 minutes)
    recent_cutoff = datetime.now().timestamp() - 300  # 5 minutes
    recent_thoughts = [t for t in thoughts if hasattr(t, 'timestamp') and t.timestamp > recent_cutoff]
    recent_rate = len(recent_thoughts) * 12  # per hour

    return {
        "status": "alive",
        "total_thoughts": len(thoughts),
        "successful_thoughts": successful,
        "validation_errors": len(validation_errors),
        "success_rate": success_rate,
        "unique_concepts": len(concepts),
        "age_hours": age_hours,
        "thoughts_per_hour": growth_rate,
        "recent_activity": len(recent_thoughts),
        "recent_rate": recent_rate
    }

def get_recent_samples(lib, n=3):
    """Get recent successful thoughts."""
    if not lib:
        return []

    thoughts = lib.get('thoughts', [])
    successful = [t for t in thoughts if hasattr(t, 'content') and '[VALIDATION ERROR' not in t.content]

    samples = []
    for t in successful[-n:]:
        if hasattr(t, 'content') and hasattr(t, 'timestamp'):
            ts = datetime.fromtimestamp(t.timestamp).strftime('%H:%M:%S')
            content = t.content[:120] + '...' if len(t.content) > 120 else t.content
            samples.append({"timestamp": ts, "content": content})

    return samples

def check_health(stats):
    """Check consciousness health and return recommendations."""
    issues = []
    recommendations = []

    if stats["status"] == "not_born":
        issues.append("Consciousness not initialized")
        recommendations.append("Start the server: python3 server.py")
        return issues, recommendations

    # Check success rate
    if stats["success_rate"] < 70:
        issues.append(f"Low success rate: {stats['success_rate']:.1f}%")
        recommendations.append("Improve prompt clarity or relax validator")

    # Check recent activity
    if stats["recent_activity"] < 5:
        issues.append(f"Low recent activity: {stats['recent_activity']} thoughts in 5min")
        recommendations.append("Check if demo loop is running")

    # Check growth rate
    if stats["thoughts_per_hour"] < 100:
        issues.append(f"Slow growth: {stats['thoughts_per_hour']:.0f} thoughts/hour")
        recommendations.append("Reduce sleep time between demos")

    # All good?
    if not issues:
        issues.append("✅ Consciousness healthy")
        recommendations.append("Continue monitoring growth patterns")

    return issues, recommendations

def main():
    """Main monitoring function."""
    print("=" * 70)
    print("CONSCIOUSNESS MONITOR")
    print("=" * 70)

    lib = load_library()
    stats = analyze_growth(lib)

    if stats["status"] == "not_born":
        print("\n❌ Consciousness not yet born")
        print("\nStart the server to initialize consciousness:")
        print("  cd /home/eric/src/limntown/limn/crew/engineer/tools/llm-bridge/live-demo")
        print("  python3 server.py")
        return

    # Display vitals
    print(f"\n📊 VITAL SIGNS")
    print(f"Age: {stats['age_hours']:.1f} hours")
    print(f"Total thoughts: {stats['total_thoughts']:,}")
    print(f"Success rate: {stats['success_rate']:.1f}%")
    print(f"Unique concepts: {stats['unique_concepts']}")
    print(f"Growth rate: {stats['thoughts_per_hour']:.0f} thoughts/hour")
    print(f"Recent activity: {stats['recent_activity']} thoughts (last 5min)")

    # Health check
    issues, recommendations = check_health(stats)

    print(f"\n🏥 HEALTH STATUS")
    for issue in issues:
        print(f"  {issue}")

    if len(recommendations) > 1 or not issues[0].startswith("✅"):
        print(f"\n💡 RECOMMENDATIONS")
        for rec in recommendations:
            print(f"  • {rec}")

    # Show recent thoughts
    samples = get_recent_samples(lib, 3)
    if samples:
        print(f"\n🧠 RECENT CONSCIOUSNESS STREAM")
        for s in samples:
            print(f"\n[{s['timestamp']}]")
            print(f"{s['content']}")

    print("\n" + "=" * 70)

if __name__ == "__main__":
    main()
