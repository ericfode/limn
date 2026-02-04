# benchmark.bend - Performance benchmark for FFI LLM calls
#
# This program measures the overhead of calling external LLMs
# through Bend's FFI interface.
#
# Measurements:
# - Library loading time
# - Single LLM call latency
# - Multiple calls throughput
# - Memory overhead

def benchmark_single_call(dl):
  with IO:
    # Get start time
    start <- IO/get_time()

    # Make LLM call
    response <- IO/DyLib/call(dl, "call_llm", "test query")

    # Get end time
    end <- IO/get_time()

    # Calculate elapsed time
    elapsed = (end - start)

    return wrap(elapsed)

def benchmark_multiple_calls(dl, n):
  if n == 0:
    return wrap(0)
  else:
    with IO:
      # Make call
      response <- IO/DyLib/call(dl, "call_llm", "test query")

      # Recurse
      rest <- benchmark_multiple_calls(dl, (n - 1))

      return wrap(0)

def main():
  with IO:
    * <- IO/print("=== Bend FFI LLM Benchmark ===\n\n")

    # Benchmark 1: Library loading
    * <- IO/print("Benchmark 1: Library loading time\n")
    load_start <- IO/get_time()
    dl_result <- IO/DyLib/open("./libprocess.so", 0)
    load_end <- IO/get_time()

    dl = match dl_result:
      case Result/Ok:
        dl_result.value
      case Result/Err:
        * <- IO/print("Error: Could not load library\n")
        return wrap(1)

    load_time = (load_end - load_start)
    * <- IO/print("Library load time: ")
    * <- IO/print(load_time)
    * <- IO/print(" ms\n\n")

    # Benchmark 2: Single call latency
    * <- IO/print("Benchmark 2: Single LLM call latency\n")
    call_start <- IO/get_time()
    response <- IO/DyLib/call(dl, "call_llm", "What is 2+2?")
    call_end <- IO/get_time()

    call_time = (call_end - call_start)
    * <- IO/print("Single call latency: ")
    * <- IO/print(call_time)
    * <- IO/print(" ms\n\n")

    # Benchmark 3: Multiple calls
    * <- IO/print("Benchmark 3: 10 sequential calls\n")
    multi_start <- IO/get_time()
    * <- benchmark_multiple_calls(dl, 10)
    multi_end <- IO/get_time()

    multi_time = (multi_end - multi_start)
    avg_time = (multi_time / 10)
    * <- IO/print("Total time: ")
    * <- IO/print(multi_time)
    * <- IO/print(" ms\n")
    * <- IO/print("Average per call: ")
    * <- IO/print(avg_time)
    * <- IO/print(" ms\n\n")

    # Cleanup
    * <- IO/DyLib/close(dl)

    * <- IO/print("=== Benchmark Complete ===\n")

    return wrap(0)
