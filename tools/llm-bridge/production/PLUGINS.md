# Oracle Plugin System

The LMN Oracle system supports a plugin architecture that allows you to extend it with custom oracle types without modifying the core codebase.

## Quick Start

### 1. Create a Plugin

Create a new Python file (e.g., `my_plugin.py`):

```python
from oracle_plugin import OraclePlugin, register_oracle

@register_oracle("Weather")
class WeatherOracle(OraclePlugin):
    """
    Weather oracle - fetches weather information.

    Usage in Bend:
        weather = oracle(Oracle/Weather { location: "San Francisco" })
    """

    # Define the regex pattern to match in Bend output
    pattern = r'Oracle/Weather[^{]*\{\s*location:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        """Extract location parameter from regex match."""
        return {"location": match.group(1)}

    def execute(self, params):
        """Execute the oracle - fetch weather data."""
        location = params["location"]

        # Your implementation here
        # This could call a real weather API
        return {
            "location": location,
            "temperature": 72,
            "condition": "Sunny"
        }
```

### 2. Use in Bend Code

Add to your `.bend` file:

```bend
def check_weather():
  sf_weather = oracle(Oracle/Weather { location: "San Francisco" })
  ny_weather = oracle(Oracle/Weather { location: "New York" })
  return (sf_weather, ny_weather)
```

### 3. Load the Plugin

In your Python harness code:

```python
from harness import ProductionHarness

harness = ProductionHarness(
    plugin_paths=["my_plugin.py"]
)

result = harness.execute("program.bend")
```

## Plugin Architecture

### Plugin Class Structure

```python
from oracle_plugin import OraclePlugin, register_oracle

@register_oracle("OracleTypeName")
class MyOracle(OraclePlugin):
    # Required: Regex pattern to match in Bend output
    pattern = r'Oracle/OracleTypeName[^{]*\{...\}'

    def extract_params(self, match):
        """
        Extract parameters from regex match.

        Args:
            match: re.Match object from pattern

        Returns:
            Dict of parameters
        """
        return {"param1": match.group(1), ...}

    def execute(self, params):
        """
        Execute the oracle logic.

        Args:
            params: Dict of parameters from extract_params

        Returns:
            Any value - will be cached and returned to Bend
        """
        # Your implementation
        return result
```

### Plugin Lifecycle

1. **Registration**: `@register_oracle()` decorator registers the plugin class
2. **Loading**: `load_plugins()` loads plugin files and registers them with harness
3. **Parsing**: When Bend output is parsed, plugin patterns are checked
4. **Extraction**: `extract_params()` extracts parameters from matched patterns
5. **Execution**: `execute()` runs the oracle logic with extracted parameters
6. **Caching**: Results are automatically cached like built-in oracles

### Accessing Harness Features

Plugins have access to the harness instance via `self.harness`:

```python
def execute(self, params):
    # Access cache
    cache_key = "my_key"
    cached = self.harness._cache_get(cache_key)

    # Access memory store
    self.harness.memory["key"] = "value"

    # Access other harness features
    if self.harness.enable_real_llm:
        # ...
```

## Pattern Writing Guide

Patterns use Python regex to match Bend oracle output.

### Basic Pattern

```python
# Matches: Oracle/Simple { value: "hello" }
pattern = r'Oracle/Simple[^{]*\{\s*value:\s*"([^"]+)"\s*\}'
```

### Multiple Parameters

```python
# Matches: Oracle/Complex { x: "a", y: "b", z: 123 }
pattern = r'Oracle/Complex[^{]*\{\s*x:\s*"([^"]+)",\s*y:\s*"([^"]+)",\s*z:\s*(\d+)\s*\}'
```

### No Parameters

```python
# Matches: Oracle/NoParams
pattern = r'Oracle/NoParams(?:\s|,|\))'
```

### Pattern Tips

- Use `[^{]*` after oracle name to skip whitespace/comments
- Use `\s*` for optional whitespace
- Use `"([^"]+)"` to capture string values
- Use `(\d+)` to capture integers
- Use `(?:\s|,|\))` for no-parameter oracles

## Example Plugins

### String Processing Oracle

```python
@register_oracle("StringOps")
class StringOpsOracle(OraclePlugin):
    """
    String operations oracle.

    Operations: uppercase, lowercase, reverse, length
    """

    pattern = r'Oracle/StringOps[^{]*\{\s*op:\s*"([^"]+)",\s*text:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {
            "op": match.group(1),
            "text": match.group(2)
        }

    def execute(self, params):
        op = params["op"]
        text = params["text"]

        if op == "uppercase":
            return text.upper()
        elif op == "lowercase":
            return text.lower()
        elif op == "reverse":
            return text[::-1]
        elif op == "length":
            return len(text)
        else:
            raise ValueError(f"Unknown operation: {op}")
```

### External API Oracle

```python
@register_oracle("GeoIP")
class GeoIPOracle(OraclePlugin):
    """
    GeoIP lookup oracle - finds location from IP address.
    """

    pattern = r'Oracle/GeoIP[^{]*\{\s*ip:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"ip": match.group(1)}

    def execute(self, params):
        import requests

        ip = params["ip"]
        response = requests.get(f"https://ipapi.co/{ip}/json/")

        if response.status_code == 200:
            data = response.json()
            return {
                "ip": ip,
                "city": data.get("city"),
                "country": data.get("country_name"),
                "latitude": data.get("latitude"),
                "longitude": data.get("longitude")
            }
        else:
            raise RuntimeError(f"GeoIP lookup failed: {response.status_code}")
```

### Caching Oracle

```python
@register_oracle("SmartCache")
class SmartCacheOracle(OraclePlugin):
    """
    Smart caching oracle with custom TTL.
    """

    pattern = r'Oracle/SmartCache[^{]*\{\s*key:\s*"([^"]+)",\s*compute:\s*"([^"]+)",\s*ttl:\s*(\d+)\s*\}'

    def extract_params(self, match):
        return {
            "key": match.group(1),
            "compute": match.group(2),
            "ttl": int(match.group(3))
        }

    def execute(self, params):
        import time

        key = params["key"]
        compute_fn = params["compute"]
        ttl = params["ttl"]

        # Check custom cache with TTL
        cache_key = f"smart_cache:{key}"
        cached = self.harness._cache_get(cache_key)

        if cached:
            timestamp, value = cached
            if time.time() - timestamp < ttl:
                return value

        # Compute new value (simplified)
        result = eval(compute_fn)  # Don't do this in production!

        # Store with timestamp
        self.harness._cache_set(cache_key, (time.time(), result))

        return result
```

## Plugin Distribution

### Directory Structure

```
plugins/
├── weather/
│   ├── __init__.py
│   └── weather_oracle.py
├── database/
│   ├── __init__.py
│   ├── postgres_oracle.py
│   └── redis_oracle.py
└── ml/
    ├── __init__.py
    └── sklearn_oracle.py
```

### Loading Plugin Directory

```python
harness = ProductionHarness(
    plugin_paths=[
        "plugins/weather",
        "plugins/database",
        "plugins/ml"
    ]
)
```

## Best Practices

### 1. Error Handling

Always handle errors gracefully:

```python
def execute(self, params):
    try:
        # Your logic
        return result
    except Exception as e:
        return {"error": str(e)}
```

### 2. Validation

Validate parameters:

```python
def execute(self, params):
    if "required_param" not in params:
        raise ValueError("Missing required_param")

    value = params["required_param"]
    if not isinstance(value, str):
        raise TypeError("required_param must be string")

    # ...
```

### 3. Documentation

Document your plugin:

```python
@register_oracle("MyOracle")
class MyOracle(OraclePlugin):
    """
    Brief description.

    Parameters:
        param1: Description of param1
        param2: Description of param2

    Returns:
        Description of return value

    Example:
        result = oracle(Oracle/MyOracle { param1: "x", param2: "y" })
    """
```

### 4. Testing

Test your plugin:

```python
# test_my_plugin.py
from my_plugin import MyOracle
from harness import ProductionHarness

def test_my_oracle():
    harness = ProductionHarness()
    plugin = MyOracle(harness)

    result = plugin.execute({"param": "value"})
    assert result == expected_result
```

## Advanced Features

### Composite Oracles

Create oracles that use other oracles:

```python
@register_oracle("Composite")
class CompositeOracle(OraclePlugin):
    def execute(self, params):
        # Use built-in oracles via harness
        file_content = self.harness._exec_file_read({"path": "/tmp/data"})

        # Use other plugin oracles
        processed = execute_plugin(
            self.harness,
            "StringOps",
            {"op": "uppercase", "text": file_content}
        )

        return processed
```

### Stateful Oracles

Maintain state across oracle calls:

```python
@register_oracle("Counter")
class CounterOracle(OraclePlugin):
    def __init__(self, harness):
        super().__init__(harness)
        self.count = 0

    def execute(self, params):
        self.count += 1
        return self.count
```

### Async Oracles

Support async operations:

```python
@register_oracle("AsyncAPI")
class AsyncAPIOracle(OraclePlugin):
    async def execute_async(self, params):
        import aiohttp

        async with aiohttp.ClientSession() as session:
            async with session.get(params["url"]) as response:
                return await response.text()

    def execute(self, params):
        import asyncio
        return asyncio.run(self.execute_async(params))
```

## Troubleshooting

### Plugin Not Loading

1. Check file path is correct
2. Verify `@register_oracle()` decorator is present
3. Check for syntax errors in plugin file
4. Enable debug logging: `harness.verbose = True`

### Pattern Not Matching

1. Test pattern with regex tester (regex101.com)
2. Print Bend output to see actual format
3. Check for whitespace differences
4. Verify parameter names match Bend code

### Execution Errors

1. Add try/except in execute()
2. Check parameter types
3. Verify external dependencies are installed
4. Test plugin independently

## Migration Guide

### Converting Built-in Oracle to Plugin

If you want to externalize a built-in oracle:

1. Create plugin file
2. Copy handler implementation to `execute()`
3. Define pattern from patterns dict
4. Copy parameter extraction to `extract_params()`
5. Register with `@register_oracle()`

Example:

```python
# Before (built-in):
# In harness.py
OracleType.MY_ORACLE = "MyOracle"
patterns[OracleType.MY_ORACLE] = r'...'
handlers[OracleType.MY_ORACLE] = self._exec_my_oracle

def _exec_my_oracle(self, params):
    return result

# After (plugin):
# In my_plugin.py
@register_oracle("MyOracle")
class MyOracle(OraclePlugin):
    pattern = r'...'

    def extract_params(self, match):
        return {...}

    def execute(self, params):
        return result
```

## Future Extensions

The plugin system can be extended to support:

- Dynamic pattern generation
- Oracle composition/pipelines
- Plugin dependencies
- Version management
- Hot reloading
- Remote plugins
- Oracle marketplaces

---

**Plugin Support Status**: ✓ Enabled

The oracle system is now fully extensible through plugins!
