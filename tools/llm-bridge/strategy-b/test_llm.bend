# test_llm.bend - Test calling LLM via FFI
#
# This program demonstrates calling an external LLM CLI tool
# through Bend's FFI interface.
#
# Compile and run:
#   bend gen-c test_llm.bend > test_llm.c
#   gcc -rdynamic -lm test_llm.c -o test_llm
#   ./test_llm

def main():
  with IO:
    # Open the process library
    dl_result <- IO/DyLib/open("./libprocess.so", 0)

    dl = match dl_result:
      case Result/Ok:
        dl_result.value
      case Result/Err:
        * <- IO/print("Error: Could not load libprocess.so\n")
        * <- IO/print("Make sure to compile it first:\n")
        * <- IO/print("  gcc -shared -o libprocess.so -I /path/to/HVM/src/ process_lib.c -Wl,--unresolved-symbols=ignore-all -fPIC\n")
        return wrap(1)

    # Call LLM with a query
    * <- IO/print("Sending query to LLM...\n")
    query = "What is the airspeed velocity of an unladen swallow?"
    response <- IO/DyLib/call(dl, "call_llm", query)

    # Print response
    * <- IO/print("\nLLM Response:\n")
    * <- IO/print(response)
    * <- IO/print("\n")

    # Close library
    * <- IO/DyLib/close(dl)

    return wrap(0)
