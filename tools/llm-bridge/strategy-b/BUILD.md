# Building and Running Strategy B Tests

## Prerequisites

1. **Bend**: Install from https://github.com/HigherOrderCO/Bend
   ```bash
   cargo install bend-lang
   ```

2. **GCC**: For compiling C libraries and generated C code
   ```bash
   sudo apt-get install build-essential  # Ubuntu/Debian
   ```

3. **HVM Source**: Clone HVM for header files
   ```bash
   git clone https://github.com/HigherOrderCO/HVM.git
   export HVM_PATH=$(pwd)/HVM
   ```

## Building the C Library

Compile the process spawning library:

```bash
gcc -shared -o libprocess.so \
    -I $HVM_PATH/src/ \
    process_lib.c \
    -Wl,--unresolved-symbols=ignore-all \
    -fPIC
```

This creates `libprocess.so` with two functions:
- `exec_command`: Run shell commands
- `call_llm`: Call LLM CLI tool

## Compiling Bend Programs

### Test Echo

```bash
# Generate C code from Bend
bend gen-c test_echo.bend > test_echo.c

# Compile to executable
gcc -rdynamic -lm test_echo.c -o test_echo

# Run
./test_echo
```

### Test LLM

```bash
bend gen-c test_llm.bend > test_llm.c
gcc -rdynamic -lm test_llm.c -o test_llm
./test_llm
```

### Benchmark

```bash
bend gen-c benchmark.bend > benchmark.c
gcc -rdynamic -lm benchmark.c -o benchmark
./benchmark
```

## Configuration

### LLM CLI Path

Set environment variable to use custom LLM CLI:

```bash
export LLM_CLI_PATH=/path/to/your/llm-cli
./test_llm
```

### Mock LLM Delay

Adjust simulated processing time:

```bash
export LLM_DELAY_MS=500  # 500ms delay
./mock-llm "test query"
```

## Running Python Benchmark

No Bend required:

```bash
./benchmark.py
```

This measures:
- Process spawn overhead
- Single call latency
- Multiple calls throughput
- Memory overhead (estimated)

## Troubleshooting

### Library Not Found

```
Error: Could not load libprocess.so
```

Solution: Compile the C library or add current directory to library path:
```bash
export LD_LIBRARY_PATH=.:$LD_LIBRARY_PATH
```

### HVM Headers Not Found

```
fatal error: hvm.h: No such file or directory
```

Solution: Set HVM_PATH and verify:
```bash
ls $HVM_PATH/src/hvm.h
```

### Undefined Symbols

```
undefined reference to `readback_str`
```

Solution: Add `-Wl,--unresolved-symbols=ignore-all` flag. These symbols are provided by the HVM runtime at link time.

## Testing Without Bend

If Bend is not installed, you can still:

1. **Test mock LLM**:
   ```bash
   ./mock-llm "your query"
   ```

2. **Run Python benchmark**:
   ```bash
   ./benchmark.py
   ```

3. **Inspect C code**:
   The `.c` files show what the interface would look like.

## Next Steps

After successful compilation:

1. Run benchmarks to measure performance
2. Compare with Strategy A (inline FFI)
3. Test with real LLM CLI tools
4. Evaluate production viability
