r"""
Oracle Plugin System
====================

Provides a plugin architecture for extending the oracle system
with new oracle types without modifying core files.

Usage:
------

1. Create a plugin file (e.g., my_plugin.py):

```python
from oracle_plugin import OraclePlugin, register_oracle

@register_oracle("MyCustom")
class MyCustomOracle(OraclePlugin):
    # Define the regex pattern to match in Bend output
    pattern = r'Oracle/MyCustom[^{]*\{\s*param:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"param": match.group(1)}

    def execute(self, params):
        # Execute the oracle logic
        return f"Custom oracle executed with {params['param']}"
```

2. Load the plugin in harness:

```python
from oracle_plugin import load_plugins

harness = ProductionHarness(...)
load_plugins(harness, ["my_plugin.py"])
```

3. Use in Bend code:

```bend
def my_function():
  result = oracle(Oracle/MyCustom { param: "value" })
  return result
```
"""

from typing import Dict, Any, List, Optional, Pattern
from pathlib import Path
import re
import importlib.util
import sys


class OraclePlugin:
    """
    Base class for oracle plugins.

    Subclasses should:
    1. Define a 'pattern' attribute (regex string)
    2. Implement extract_params(match) -> Dict
    3. Implement execute(params) -> Any
    """

    pattern: str = ""
    oracle_type: str = ""

    def __init__(self, harness):
        """Initialize plugin with reference to harness."""
        self.harness = harness

    def extract_params(self, match) -> Dict[str, Any]:
        """Extract parameters from regex match."""
        raise NotImplementedError("Subclasses must implement extract_params")

    def execute(self, params: Dict[str, Any]) -> Any:
        """Execute the oracle with given parameters."""
        raise NotImplementedError("Subclasses must implement execute")


# Plugin registry
_plugin_registry: Dict[str, type] = {}


def register_oracle(oracle_type: str):
    """
    Decorator to register an oracle plugin.

    Usage:
        @register_oracle("MyOracle")
        class MyOraclePlugin(OraclePlugin):
            ...
    """
    def decorator(cls):
        cls.oracle_type = oracle_type
        _plugin_registry[oracle_type] = cls
        return cls
    return decorator


def load_plugin_file(path: Path) -> List[type]:
    """
    Load a plugin file and return registered plugin classes.

    Args:
        path: Path to plugin Python file

    Returns:
        List of plugin classes registered by this file
    """
    if not path.exists():
        raise FileNotFoundError(f"Plugin file not found: {path}")

    # Track plugins before loading
    before = set(_plugin_registry.keys())

    # Load the module
    spec = importlib.util.spec_from_file_location(path.stem, path)
    if spec is None or spec.loader is None:
        raise ImportError(f"Could not load plugin: {path}")

    module = importlib.util.module_from_spec(spec)
    sys.modules[path.stem] = module
    spec.loader.exec_module(module)

    # Find new plugins
    after = set(_plugin_registry.keys())
    new_types = after - before

    return [_plugin_registry[t] for t in new_types]


def load_plugins(harness, plugin_paths: List[str] = None) -> int:
    """
    Load oracle plugins and register them with the harness.

    Args:
        harness: ProductionHarness instance
        plugin_paths: List of paths to plugin files or directories

    Returns:
        Number of plugins loaded
    """
    if plugin_paths is None:
        plugin_paths = []

    loaded_count = 0

    for path_str in plugin_paths:
        path = Path(path_str)

        if path.is_file():
            # Load single plugin file
            plugins = load_plugin_file(path)
            for plugin_cls in plugins:
                register_plugin_with_harness(harness, plugin_cls)
                loaded_count += 1

        elif path.is_dir():
            # Load all .py files in directory
            for plugin_file in path.glob("*.py"):
                if plugin_file.name.startswith("_"):
                    continue
                plugins = load_plugin_file(plugin_file)
                for plugin_cls in plugins:
                    register_plugin_with_harness(harness, plugin_cls)
                    loaded_count += 1

    return loaded_count


def register_plugin_with_harness(harness, plugin_cls: type):
    """
    Register a plugin class with the harness.

    This adds the plugin's pattern, parameter extraction,
    and handler to the harness's internal structures.
    """
    # Create plugin instance
    plugin = plugin_cls(harness)

    # Add to harness's plugin registry
    if not hasattr(harness, '_plugins'):
        harness._plugins = {}

    harness._plugins[plugin.oracle_type] = plugin


def get_plugin_patterns(harness) -> Dict[str, str]:
    """Get all registered plugin patterns."""
    if not hasattr(harness, '_plugins'):
        return {}

    return {
        oracle_type: plugin.pattern
        for oracle_type, plugin in harness._plugins.items()
    }


def extract_plugin_params(harness, oracle_type: str, match) -> Dict[str, Any]:
    """Extract parameters using the appropriate plugin."""
    if not hasattr(harness, '_plugins'):
        return {}

    plugin = harness._plugins.get(oracle_type)
    if plugin:
        return plugin.extract_params(match)
    return {}


def execute_plugin(harness, oracle_type: str, params: Dict[str, Any]) -> Any:
    """Execute an oracle using the appropriate plugin."""
    if not hasattr(harness, '_plugins'):
        raise ValueError(f"No plugins loaded")

    plugin = harness._plugins.get(oracle_type)
    if not plugin:
        raise ValueError(f"Unknown plugin oracle: {oracle_type}")

    return plugin.execute(params)


# Example plugin demonstrating the system
@register_oracle("Example")
class ExampleOracle(OraclePlugin):
    """
    Example oracle plugin showing how to extend the system.

    Matches: Oracle/Example { message: "hello" }
    Returns: "Example: hello"
    """

    pattern = r'Oracle/Example[^{]*\{\s*message:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"message": match.group(1)}

    def execute(self, params):
        message = params["message"]
        return f"Example: {message}"
