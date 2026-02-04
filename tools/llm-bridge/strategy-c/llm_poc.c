/*
 * Proof of Concept: %llm Primitive for HVM4
 *
 * This is a standalone demonstration showing how the %llm primitive
 * would be implemented. Not intended to compile standalone - requires
 * HVM4 runtime context.
 *
 * Author: Rex (Engineer)
 * Date: 2026-02-01
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <curl/curl.h>

// ============================================================================
// HVM4 Type Definitions (from hvm4.c)
// ============================================================================

typedef uint64_t u64;
typedef uint32_t u32;
typedef uint8_t  u8;

typedef u64 Term;

// Term tags (simplified - see hvm4.c for complete list)
#define NUM  30
#define C00  13  // Constructor arity 0
#define C01  14  // Constructor arity 1
#define C02  15  // Constructor arity 2

// Known constructor names (from nick encoding)
#define NAM_NIL  0xABCD  // Placeholder - actual value from table
#define NAM_CON  0x1234  // Placeholder
#define NAM_CHR  0x5678  // Placeholder

// Term accessors (simplified)
#define term_tag(t)  ((t >> 56) & 0x7F)
#define term_ext(t)  ((t >> 32) & 0xFFFFFF)
#define term_val(t)  (t & 0xFFFFFFFF)

// Forward declarations
typedef Term (*PrimFn)(Term *args);
fn Term wnf(Term term);  // Weak-head normal form evaluator
fn u32  table_find(const char *name, u32 len);
fn void prim_register(const char *name, u32 len, u32 arity, PrimFn fun);

// Global heap (defined in runtime)
extern Term *HEAP;
fn Term heap_read(u32 loc) {
  return HEAP[loc];
}

// ============================================================================
// Helper: HTTP Response Buffer
// ============================================================================

struct MemoryStruct {
  char *memory;
  size_t size;
};

static size_t WriteMemoryCallback(void *contents, size_t size, size_t nmemb,
                                   void *userp) {
  size_t realsize = size * nmemb;
  struct MemoryStruct *mem = (struct MemoryStruct *)userp;

  char *ptr = realloc(mem->memory, mem->size + realsize + 1);
  if (ptr == NULL) {
    fprintf(stderr, "ERROR: realloc failed (out of memory)\n");
    return 0;
  }

  mem->memory = ptr;
  memcpy(&(mem->memory[mem->size]), contents, realsize);
  mem->size += realsize;
  mem->memory[mem->size] = 0;

  return realsize;
}

// ============================================================================
// String Conversion: HVM List → C String
// ============================================================================

/*
 * Converts HVM4 list of #Chr{code} to C string.
 *
 * HVM representation:
 *   "hi" = #Con{#Chr{104}, #Con{#Chr{105}, #Nil}}
 *
 * Returns: Malloc'd string (caller must free)
 */
static char* hvm_list_to_cstring(Term list) {
  // Allocate initial buffer
  size_t capacity = 4096;
  size_t pos = 0;
  char *buf = malloc(capacity);
  if (!buf) {
    fprintf(stderr, "ERROR: malloc failed\n");
    exit(1);
  }

  Term cur = wnf(list);

  while (1) {
    // Expand buffer if needed
    if (pos >= capacity - 4) {  // -4 for UTF-8 safety
      capacity *= 2;
      char *new_buf = realloc(buf, capacity);
      if (!new_buf) {
        fprintf(stderr, "ERROR: realloc failed\n");
        free(buf);
        exit(1);
      }
      buf = new_buf;
    }

    // Check for #Nil (end of list)
    if (term_tag(cur) == C00 && term_ext(cur) == NAM_NIL) {
      buf[pos] = '\0';
      return buf;
    }

    // Check for #Con{head, tail}
    if (term_tag(cur) == C02 && term_ext(cur) == NAM_CON) {
      u32  con_loc = term_val(cur);
      Term head    = heap_read(con_loc + 0);
      Term tail    = heap_read(con_loc + 1);

      // Evaluate head to WHNF
      Term head_wnf = wnf(head);

      // Expect #Chr{code}
      if (term_tag(head_wnf) == C01 && term_ext(head_wnf) == NAM_CHR) {
        u32  chr_loc  = term_val(head_wnf);
        Term code     = heap_read(chr_loc + 0);
        Term code_wnf = wnf(code);

        if (term_tag(code_wnf) == NUM) {
          u32 codepoint = term_val(code_wnf);

          // Convert codepoint to UTF-8
          if (codepoint <= 0x7F) {
            buf[pos++] = (char)codepoint;
          } else if (codepoint <= 0x7FF) {
            buf[pos++] = (char)(0xC0 | (codepoint >> 6));
            buf[pos++] = (char)(0x80 | (codepoint & 0x3F));
          } else if (codepoint <= 0xFFFF) {
            buf[pos++] = (char)(0xE0 | (codepoint >> 12));
            buf[pos++] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
            buf[pos++] = (char)(0x80 | (codepoint & 0x3F));
          } else if (codepoint <= 0x10FFFF) {
            buf[pos++] = (char)(0xF0 | (codepoint >> 18));
            buf[pos++] = (char)(0x80 | ((codepoint >> 12) & 0x3F));
            buf[pos++] = (char)(0x80 | ((codepoint >> 6) & 0x3F));
            buf[pos++] = (char)(0x80 | (codepoint & 0x3F));
          } else {
            fprintf(stderr, "ERROR: invalid Unicode codepoint: %u\n", codepoint);
            free(buf);
            exit(1);
          }

          // Continue with tail
          cur = wnf(tail);
          continue;
        }
      }
    }

    // Invalid format
    fprintf(stderr, "ERROR: %llm expects string list (got invalid format)\n");
    free(buf);
    exit(1);
  }
}

// ============================================================================
// String Conversion: C String → HVM List
// ============================================================================

/*
 * Converts C string to HVM4 list of #Chr{code}.
 *
 * Returns: HVM Term representing string
 */
static Term cstring_to_hvm_list(const char *str) {
  // Base case: empty string
  if (str[0] == '\0') {
    return term_new_ctr(NAM_NIL, 0, 0);
  }

  // Decode UTF-8 character
  u32 codepoint;
  int bytes;

  if ((str[0] & 0x80) == 0) {
    // 1-byte ASCII
    codepoint = str[0];
    bytes = 1;
  } else if ((str[0] & 0xE0) == 0xC0) {
    // 2-byte UTF-8
    codepoint = ((str[0] & 0x1F) << 6) | (str[1] & 0x3F);
    bytes = 2;
  } else if ((str[0] & 0xF0) == 0xE0) {
    // 3-byte UTF-8
    codepoint = ((str[0] & 0x0F) << 12) | ((str[1] & 0x3F) << 6) |
                (str[2] & 0x3F);
    bytes = 3;
  } else if ((str[0] & 0xF8) == 0xF0) {
    // 4-byte UTF-8
    codepoint = ((str[0] & 0x07) << 18) | ((str[1] & 0x3F) << 12) |
                ((str[2] & 0x3F) << 6) | (str[3] & 0x3F);
    bytes = 4;
  } else {
    fprintf(stderr, "ERROR: invalid UTF-8 sequence\n");
    exit(1);
  }

  // Construct #Chr{codepoint}
  u32 chr_loc = heap_alloc(1);
  Term code = term_new_num(codepoint);
  heap_set(chr_loc, code);
  Term chr = term_new_ctr(NAM_CHR, 1, chr_loc);

  // Recursive: construct tail
  Term tail = cstring_to_hvm_list(str + bytes);

  // Construct #Con{chr, tail}
  u32 con_loc = heap_alloc(2);
  heap_set(con_loc + 0, chr);
  heap_set(con_loc + 1, tail);

  return term_new_ctr(NAM_CON, 2, con_loc);
}

// ============================================================================
// JSON Parsing (Simple)
// ============================================================================

/*
 * Extract "content[0].text" from Anthropic API response.
 *
 * Example response:
 * {"content":[{"type":"text","text":"Hello!"}], ...}
 *
 * Returns: Malloc'd string (caller must free)
 */
static char* extract_text_from_json(const char *json) {
  // Find "text":"
  const char *text_start = strstr(json, "\"text\":\"");
  if (!text_start) {
    fprintf(stderr, "ERROR: No 'text' field in API response\n");
    fprintf(stderr, "Response: %s\n", json);
    return strdup("ERROR: No content in response");
  }

  // Move past "text":"
  text_start += 8;

  // Find closing quote (handling escapes)
  const char *p = text_start;
  size_t len = 0;
  while (*p) {
    if (*p == '"' && (p == text_start || *(p - 1) != '\\')) {
      break;
    }
    p++;
    len++;
  }

  if (*p != '"') {
    fprintf(stderr, "ERROR: Malformed JSON (unterminated string)\n");
    return strdup("ERROR: Malformed response");
  }

  // Allocate and copy (with unescaping)
  char *result = malloc(len + 1);
  if (!result) {
    fprintf(stderr, "ERROR: malloc failed\n");
    exit(1);
  }

  // Simple copy (TODO: handle escape sequences properly)
  strncpy(result, text_start, len);
  result[len] = '\0';

  return result;
}

// ============================================================================
// HTTP Client (libcurl)
// ============================================================================

/*
 * Call Anthropic API with query string.
 *
 * Returns: Malloc'd response string (caller must free)
 */
static char* call_anthropic_api(const char *query) {
  CURL *curl = curl_easy_init();
  if (!curl) {
    fprintf(stderr, "ERROR: curl_easy_init failed\n");
    exit(1);
  }

  // Get API key from environment
  const char *api_key = getenv("ANTHROPIC_API_KEY");
  if (!api_key) {
    fprintf(stderr, "ERROR: ANTHROPIC_API_KEY environment variable not set\n");
    fprintf(stderr, "Set it with: export ANTHROPIC_API_KEY=\"sk-ant-...\"\n");
    curl_easy_cleanup(curl);
    exit(1);
  }

  // Get model from environment (default: sonnet 4.5)
  const char *model = getenv("LLM_MODEL");
  if (!model) {
    model = "claude-sonnet-4-5-20250929";
  }

  // Get max_tokens from environment (default: 1024)
  const char *max_tokens_str = getenv("LLM_MAX_TOKENS");
  int max_tokens = max_tokens_str ? atoi(max_tokens_str) : 1024;

  // Build JSON request body
  // TODO: Properly escape query string
  size_t json_size = strlen(query) + 1024;
  char *json = malloc(json_size);
  if (!json) {
    fprintf(stderr, "ERROR: malloc failed\n");
    curl_easy_cleanup(curl);
    exit(1);
  }

  snprintf(json, json_size,
    "{"
    "\"model\":\"%s\","
    "\"max_tokens\":%d,"
    "\"messages\":["
      "{\"role\":\"user\",\"content\":\"%s\"}"
    "]"
    "}",
    model, max_tokens, query);

  // Setup headers
  struct curl_slist *headers = NULL;
  headers = curl_slist_append(headers, "Content-Type: application/json");

  char auth_header[512];
  snprintf(auth_header, sizeof(auth_header), "x-api-key: %s", api_key);
  headers = curl_slist_append(headers, auth_header);
  headers = curl_slist_append(headers, "anthropic-version: 2023-06-01");

  // Setup response buffer
  struct MemoryStruct chunk = {
    .memory = malloc(1),
    .size = 0
  };

  // Configure curl
  curl_easy_setopt(curl, CURLOPT_URL, "https://api.anthropic.com/v1/messages");
  curl_easy_setopt(curl, CURLOPT_POSTFIELDS, json);
  curl_easy_setopt(curl, CURLOPT_HTTPHEADER, headers);
  curl_easy_setopt(curl, CURLOPT_WRITEFUNCTION, WriteMemoryCallback);
  curl_easy_setopt(curl, CURLOPT_WRITEDATA, (void *)&chunk);

  // Security: SSL verification
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYPEER, 1L);
  curl_easy_setopt(curl, CURLOPT_SSL_VERIFYHOST, 2L);

  // Timeout
  const char *timeout_str = getenv("LLM_TIMEOUT");
  long timeout = timeout_str ? atol(timeout_str) : 30L;
  curl_easy_setopt(curl, CURLOPT_TIMEOUT, timeout);

  // Perform request
  CURLcode res = curl_easy_perform(curl);

  // Cleanup
  curl_easy_cleanup(curl);
  curl_slist_free_all(headers);
  free(json);

  // Check for errors
  if (res != CURLE_OK) {
    fprintf(stderr, "ERROR: HTTP request failed: %s\n",
            curl_easy_strerror(res));
    free(chunk.memory);
    exit(1);
  }

  return chunk.memory;
}

// ============================================================================
// Main Primitive Implementation
// ============================================================================

/*
 * %llm primitive function.
 *
 * Args:
 *   args[0]: Query string (HVM list of #Chr)
 *
 * Returns: Response string (HVM list of #Chr)
 */
fn Term prim_fn_llm(Term *args) {
  // Step 1: Extract query string from HVM list
  char *query = hvm_list_to_cstring(args[0]);

  // Step 2: Call Anthropic API
  char *json_response = call_anthropic_api(query);

  // Step 3: Parse JSON to extract text
  char *response_text = extract_text_from_json(json_response);

  // Step 4: Convert C string to HVM list
  Term result = cstring_to_hvm_list(response_text);

  // Cleanup
  free(query);
  free(json_response);
  free(response_text);

  return result;
}

// ============================================================================
// Registration
// ============================================================================

/*
 * Initialize and register %llm primitive.
 */
fn void prim_llm_init(void) {
  prim_register("llm", 3, 1, prim_fn_llm);
}

// ============================================================================
// Integration Point
// ============================================================================

/*
 * Add this call to prim/init.c:
 *
 * fn void prim_init(void) {
 *   prim_log_init();
 *   prim_llm_init();  // <-- Add this line
 * }
 */

// ============================================================================
// Usage Example
// ============================================================================

/*
 * HVM4 code:
 *
 * @main = %llm("What is 2+2?")
 *
 * Run:
 * $ export ANTHROPIC_API_KEY="sk-ant-..."
 * $ ./main test.hvm4
 * 4
 */
