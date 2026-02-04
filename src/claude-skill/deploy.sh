#!/bin/bash
# Deploy /limn skill to Claude Code plugin directories
#
# Usage: ./deploy.sh [workspace_root]
#
# Deploys the /limn skill as a Claude Code plugin.
# If workspace_root is specified, deploys to that workspace.
# Otherwise deploys to the current workspace.

set -e

SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

deploy_to() {
    local target="$1"
    local plugin_dir="$target/.claude/plugins/limn"

    echo "Deploying /limn skill to: $target"

    # Create plugin structure
    mkdir -p "$plugin_dir/.claude-plugin"
    mkdir -p "$plugin_dir/skills/limn"

    # Plugin config
    cat > "$plugin_dir/.claude-plugin/plugin.json" << 'EOF'
{
  "name": "limn",
  "version": "1.0.0",
  "description": "Limn Language - A constraint-based language where words are regions and meaning is their intersection",
  "author": {
    "name": "Limn Land",
    "url": "https://github.com/ericfode/limn"
  },
  "skills": "./skills/"
}
EOF

    # Copy SKILL.md
    cp "$SCRIPT_DIR/SKILL.md" "$plugin_dir/skills/limn/SKILL.md"

    # Create marketplace if needed
    local mp_dir="$target/.claude/plugins/.claude-plugin"
    if [ ! -f "$mp_dir/marketplace.json" ]; then
        local name
        name=$(basename "$target")
        mkdir -p "$mp_dir"
        cat > "$mp_dir/marketplace.json" << EOFM
{
  "name": "limn-${name}-plugins",
  "owner": { "name": "Limn" },
  "metadata": { "description": "Limn plugins", "version": "1.0.0" },
  "plugins": [
    {
      "name": "limn",
      "source": "./limn",
      "description": "Limn Language Interpreter",
      "version": "1.0.0"
    }
  ]
}
EOFM
    fi

    echo "  Done. Enable in settings.json with: \"limn@<marketplace-name>\": true"
}

if [ -n "$1" ]; then
    deploy_to "$1"
else
    deploy_to "$(git rev-parse --show-toplevel 2>/dev/null || pwd)"
fi
