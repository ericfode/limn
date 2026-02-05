#!/bin/bash
# Auto eval + deploy v3 after training completes
# Usage: nohup bash run_v3_eval_and_deploy.sh &

set -e
cd "$(dirname "$0")"
source venv/bin/activate

MODEL_DIR="output/limn-slm-v3/limn-slm-final"
SERVE_MODEL="output/limn-slm-final"

echo "=== Waiting for v3 training to complete ==="
echo "Watching PID 2096281..."

# Wait for training process to finish
while kill -0 2096281 2>/dev/null; do
    echo "$(date '+%H:%M:%S') Training still running..."
    sleep 60
done

echo "$(date '+%H:%M:%S') Training complete!"

# Check that model was saved
if [ ! -f "$MODEL_DIR/adapter_config.json" ]; then
    echo "ERROR: Model not found at $MODEL_DIR"
    echo "Checking latest checkpoint instead..."
    LATEST_CP=$(ls -td output/limn-slm-v3/checkpoints/checkpoint-* 2>/dev/null | head -1)
    if [ -n "$LATEST_CP" ]; then
        MODEL_DIR="$LATEST_CP"
        echo "Using checkpoint: $MODEL_DIR"
    else
        echo "No checkpoints found. Aborting."
        exit 1
    fi
fi

# Run eval
echo ""
echo "=== Running v3 Evaluation ==="
python eval_v3.py --model "$MODEL_DIR" --output eval_v3_results.json -v \
    2>&1 | tee output/v3_eval_output.log

echo ""
echo "=== Eval complete. Results in eval_v3_results.json ==="

# Stop old serve.py if running
echo ""
echo "=== Deploying v3 ==="
OLD_PID=$(pgrep -f "serve.py" || true)
if [ -n "$OLD_PID" ]; then
    echo "Stopping old serve.py (PID $OLD_PID)..."
    kill "$OLD_PID" 2>/dev/null || true
    sleep 2
fi

# Start new serve.py with v3 model
echo "Starting serve.py with v3 model..."
nohup python serve.py --model "$MODEL_DIR" --port 8741 \
    > output/serve_v3.log 2>&1 &
echo "serve.py started (PID $!)"

echo ""
echo "=== v3 deployed on port 8741 ==="
echo "Done at $(date)"
