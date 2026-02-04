# test_echo.bend - Simple echo test using Bend FFI
#
# This program demonstrates basic FFI usage with IO/DyLib functions.
# It loads a shared library and calls a simple echo function.
#
# Compile and run:
#   bend gen-c test_echo.bend > test_echo.c
#   gcc -rdynamic -lm test_echo.c -o test_echo
#   ./test_echo

def main():
  with IO:
    # Open the shared library
    dl_result <- IO/DyLib/open("./libprocess.so", 0)

    # Check if library loaded successfully
    dl = match dl_result:
      case Result/Ok:
        dl_result.value
      case Result/Err:
        * <- IO/print("Error: Could not load library\n")
        return wrap(1)

    # Call echo function
    * <- IO/print("Calling exec_command...\n")
    output <- IO/DyLib/call(dl, "exec_command", "echo 'Hello from Bend!'")

    # Print result
    * <- IO/print("Output: ")
    * <- IO/print(output)
    * <- IO/print("\n")

    # Close library
    * <- IO/DyLib/close(dl)

    return wrap(0)
