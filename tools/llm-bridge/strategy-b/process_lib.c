/*
 * process_lib.c - Process spawning library for Bend FFI
 *
 * This library provides functions to spawn external processes
 * and capture their output, callable from Bend programs.
 *
 * Compile with:
 *   gcc -shared -o libprocess.so -I /path/to/HVM/src/ process_lib.c \
 *     -Wl,--unresolved-symbols=ignore-all -fPIC
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// HVM interface (requires actual HVM headers for full implementation)
// These are placeholder declarations
typedef unsigned long long Port;
typedef struct Net Net;
typedef struct Book Book;

// External HVM functions (provided by HVM runtime)
// Declared as weak symbols for compilation without HVM
char* readback_str(Net* net, Port port) __attribute__((weak));
Port inject_bytes(Net* net, const char* str) __attribute__((weak));
Port new_port(int tag, int val) __attribute__((weak));

/*
 * exec_command - Execute a shell command and return its output
 *
 * Args: String containing the command to execute
 * Returns: String containing the command's stdout
 *
 * Example Bend usage:
 *   with IO:
 *     dl <- IO/DyLib/open("./libprocess.so", 0)
 *     result <- IO/DyLib/call(dl, "exec_command", "echo test")
 *     * <- IO/print(result)
 */
Port exec_command(Net* net, Book* book, Port arg) {
    // Readback the command string from HVM
    char* command = readback_str ? readback_str(net, arg) : NULL;

    if (!command) {
        fprintf(stderr, "[process_lib] Error: could not read command string\n");
        return inject_bytes ? inject_bytes(net, "") : arg;
    }

    fprintf(stderr, "[process_lib] Executing: %s\n", command);

    // Execute command and capture output
    FILE* pipe = popen(command, "r");
    if (!pipe) {
        fprintf(stderr, "[process_lib] Error: popen failed\n");
        free(command);
        return inject_bytes ? inject_bytes(net, "ERROR: popen failed") : arg;
    }

    // Read output
    char buffer[4096];
    size_t total_size = 0;
    char* output = malloc(1);
    output[0] = '\0';

    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
        size_t len = strlen(buffer);
        output = realloc(output, total_size + len + 1);
        strcat(output, buffer);
        total_size += len;
    }

    int status = pclose(pipe);
    fprintf(stderr, "[process_lib] Command exited with status: %d\n", status);
    fprintf(stderr, "[process_lib] Output length: %zu bytes\n", total_size);

    // Convert back to HVM string
    Port result = inject_bytes ? inject_bytes(net, output) : arg;

    free(command);
    free(output);

    return result;
}

/*
 * call_llm - Call an LLM CLI tool with a query
 *
 * Args: String containing the query/prompt
 * Returns: String containing the LLM's response
 *
 * This function calls an external LLM CLI tool.
 * The tool path can be configured via environment variable LLM_CLI_PATH
 */
Port call_llm(Net* net, Book* book, Port arg) {
    char* query = readback_str ? readback_str(net, arg) : NULL;

    if (!query) {
        fprintf(stderr, "[process_lib] Error: could not read query string\n");
        return inject_bytes ? inject_bytes(net, "") : arg;
    }

    // Get LLM CLI path from environment, default to ./mock-llm
    const char* llm_cli = getenv("LLM_CLI_PATH");
    if (!llm_cli) {
        llm_cli = "./mock-llm";
    }

    // Build command
    char command[8192];
    snprintf(command, sizeof(command), "%s '%s'", llm_cli, query);

    fprintf(stderr, "[process_lib] Calling LLM: %s\n", command);

    // Execute
    FILE* pipe = popen(command, "r");
    if (!pipe) {
        fprintf(stderr, "[process_lib] Error: could not execute LLM CLI\n");
        free(query);
        return inject_bytes ? inject_bytes(net, "ERROR: LLM call failed") : arg;
    }

    // Read response
    char buffer[4096];
    size_t total_size = 0;
    char* response = malloc(1);
    response[0] = '\0';

    while (fgets(buffer, sizeof(buffer), pipe) != NULL) {
        size_t len = strlen(buffer);
        response = realloc(response, total_size + len + 1);
        strcat(response, buffer);
        total_size += len;
    }

    pclose(pipe);
    fprintf(stderr, "[process_lib] LLM response length: %zu bytes\n", total_size);

    Port result = inject_bytes ? inject_bytes(net, response) : arg;

    free(query);
    free(response);

    return result;
}
