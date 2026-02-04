#!/bin/bash
# Moment Garden MCP Server Launcher
# SWI-Prolog version

cd "$(dirname "$0")"

# Check for SWI-Prolog
if ! command -v swipl &> /dev/null; then
    echo "Error: SWI-Prolog (swipl) not found." >&2
    echo "Install with: sudo apt-get install swi-prolog-core" >&2
    exit 1
fi

exec swipl mcp-server.pl
