#!/usr/bin/env bash
## Test Moment Garden MCP Server
## =========================================================
## mcp tes | too cal | sta ver
## *MCP test. Tool call. State verify.*

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$SCRIPT_DIR/../../.."
MCP_SERVER="$REPO_ROOT/crew/engineer/tools/mcp-server/moment-garden-mcp"

echo "Testing Moment Garden MCP Server"
echo "=================================="
echo ""

# Create test directory
TEST_DIR=$(mktemp -d)
trap "rm -rf $TEST_DIR" EXIT

cd "$REPO_ROOT"

# Helper function to call MCP server
call_mcp() {
    local request="$1"
    echo "$request" | "$MCP_SERVER" 2>/dev/null | head -1
}

echo "1. Testing MCP Initialization"
echo "------------------------------"
INIT_REQUEST='{"jsonrpc":"2.0","id":1,"method":"initialize","params":{}}'
INIT_RESPONSE=$(call_mcp "$INIT_REQUEST")
echo "Request:  $INIT_REQUEST"
echo "Response: $INIT_RESPONSE"
echo ""

echo "2. Testing tools/list"
echo "---------------------"
LIST_REQUEST='{"jsonrpc":"2.0","id":2,"method":"tools/list","params":{}}'
LIST_RESPONSE=$(call_mcp "$LIST_REQUEST")
echo "Request:  $LIST_REQUEST"
echo "Response: $LIST_RESPONSE"
echo ""

echo "3. Testing garden_create"
echo "------------------------"
CREATE_REQUEST='{"jsonrpc":"2.0","id":3,"method":"tools/call","params":{"name":"garden_create","arguments":{"garden_id":"garden-test1"}}}'
CREATE_RESPONSE=$(call_mcp "$CREATE_REQUEST")
echo "Request:  $CREATE_REQUEST"
echo "Response: $CREATE_RESPONSE"
echo ""

echo "4. Testing garden_calculate_ripples"
echo "------------------------------------"
RIPPLE_REQUEST='{"jsonrpc":"2.0","id":4,"method":"tools/call","params":{"name":"garden_calculate_ripples","arguments":{"seed_num":5,"temporal_key":"now"}}}'
RIPPLE_RESPONSE=$(call_mcp "$RIPPLE_REQUEST")
echo "Request:  $RIPPLE_REQUEST"
echo "Response: $RIPPLE_RESPONSE"
echo ""

echo "5. Testing garden_save_reading"
echo "-------------------------------"
SAVE_REQUEST='{"jsonrpc":"2.0","id":5,"method":"tools/call","params":{"name":"garden_save_reading","arguments":{"garden_id":"garden-test1","reader_id":"alice","temporal_key":"now","path":[5,4,1],"collapses":[{"seed":5,"meaning":"Now manifests here"},{"seed":4,"meaning":"Losing self to find other"}]}}}'
SAVE_RESPONSE=$(call_mcp "$SAVE_REQUEST")
echo "Request:  $SAVE_REQUEST"
echo "Response: $SAVE_RESPONSE"
echo ""

echo "6. Testing garden_get_readings"
echo "-------------------------------"
GET_REQUEST='{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"garden_get_readings","arguments":{"garden_id":"garden-test1"}}}'
GET_RESPONSE=$(call_mcp "$GET_REQUEST")
echo "Request:  $GET_REQUEST"
echo "Response: $GET_RESPONSE"
echo ""

echo "All tests complete!"
echo ""
echo "Check .beads/moment-garden/garden-test1.pl for persisted state"
