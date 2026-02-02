/*
 * echo_lib.c - Simple echo function for Bend FFI testing
 *
 * This library provides a basic echo function that demonstrates
 * calling C code from Bend via IO/DyLib/call.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// HVM types (minimal definitions for standalone compilation)
// In real use, include from HVM headers
typedef unsigned long long Port;
typedef struct Net Net;
typedef struct Book Book;

// Simple echo function that returns the input string with a prefix
Port echo_string(Net* net, Book* book, Port arg) {
    // In a real implementation, you would:
    // 1. Use readback_str(net, arg) to get the C string
    // 2. Process the string
    // 3. Use inject_bytes(net, result) to return HVM string

    // For now, this is a stub showing the signature
    // Actual implementation requires HVM headers

    fprintf(stderr, "[echo_lib] Function called\n");

    // Return the input unchanged (in real impl, would add "Echo: " prefix)
    return arg;
}

// More advanced function that could spawn a process
Port run_command(Net* net, Book* book, Port arg) {
    // This would use popen() to run a command and capture output
    // For example:
    // 1. readback_str(net, arg) to get command
    // 2. popen(command, "r") to run it
    // 3. Read output
    // 4. inject_bytes(net, output) to return as HVM string

    fprintf(stderr, "[echo_lib] run_command called\n");

    return arg;
}
