# Strategy B: External Process Calls from Bend via FFI

## Overview

This strategy tests calling external processes (including LLM CLIs) from Bend using its Foreign Function Interface (FFI) through dynamic library loading.

## Bend's IO/DyLib Functions

Bend provides three core FFI functions:

- **IO/DyLib/open**: Opens a shared library (.so, .dll, .dylib)
  - Takes: path (String), mode (0=immediate, 1=lazy)
  - Returns: Result containing library ID (u24)

- **IO/DyLib/call**: Calls a function in the loaded library
  - Takes: library ID, function name (String), arguments (Any)
  - Returns: Any (depends on called function)

- **IO/DyLib/close**: Closes the dynamic library
  - Takes: library ID
  - Returns: nothing

## C Interface Requirements

Functions must match HVM's signature:

```c
Port function_name(Net* net, Book* book, Port arg);
```

Where:
- `net`: Current HVM network state
- `book`: Function definitions
- `arg`: Function arguments (encoded as HVM Port)

HVM provides utility functions for type conversion:
- `readback_str`: HVM Port to C string
- `inject_bytes`: C bytes to HVM Port
- `new_port`: Create new Port values

## Compilation

**C library:**
```bash
gcc -shared -o libname.so -I /path/to/HVM/src/ source.c \
  -Wl,--unresolved-symbols=ignore-all -fPIC
```

**Bend program:**
```bash
bend gen-c program.bend > program.c
gcc -rdynamic -lm program.c -o program
```

The `-rdynamic` flag allows the library to access symbols from the main program.

## Test Plan

1. **Simple Echo Test**: Basic C function that echoes input
2. **Process Spawning**: C library that runs shell commands
3. **Mock LLM CLI**: Simulated LLM interface
4. **Performance Benchmark**: Measure overhead

## Expected Challenges

- HVM type conversion overhead
- Process spawning latency
- String marshalling costs
- Library loading time
- Error handling complexity

## References

- [Bend FFI Documentation](https://github.com/HigherOrderCO/Bend/blob/main/docs/ffi.md)
- [HVM Dylib PR #394](https://github.com/HigherOrderCO/HVM/pull/394)
- [Bend Dylib Issue #621](https://github.com/HigherOrderCO/Bend/issues/621)
