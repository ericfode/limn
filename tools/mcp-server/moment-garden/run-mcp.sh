#!/bin/bash
# Moment Garden MCP Server Launcher
# Loads dependencies in correct order for Scryer Prolog

cd "$(dirname "$0")"

# Create a combined loader file
cat > /tmp/mcp-loader.pl <<'EOF'
:- ['../json'].
:- ['state'].
:- ['propagation'].
:- ['mcp-server'].
EOF

exec scryer-prolog /tmp/mcp-loader.pl
