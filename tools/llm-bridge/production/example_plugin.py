"""
Example Oracle Plugins
======================

Demonstration plugins showing how to extend the oracle system.

To use:
    harness = ProductionHarness(plugin_paths=["example_plugin.py"])
"""

from oracle_plugin import OraclePlugin, register_oracle
import json


@register_oracle("Echo")
class EchoOracle(OraclePlugin):
    """
    Echo oracle - returns the input message.

    Example in Bend:
        result = oracle(Oracle/Echo { message: "hello world" })
        # Returns: "hello world"
    """

    pattern = r'Oracle/Echo[^{]*\{\s*message:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"message": match.group(1)}

    def execute(self, params):
        return params["message"]


@register_oracle("Reverse")
class ReverseOracle(OraclePlugin):
    """
    Reverse oracle - reverses a string.

    Example in Bend:
        result = oracle(Oracle/Reverse { text: "hello" })
        # Returns: "olleh"
    """

    pattern = r'Oracle/Reverse[^{]*\{\s*text:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"text": match.group(1)}

    def execute(self, params):
        return params["text"][::-1]


@register_oracle("Length")
class LengthOracle(OraclePlugin):
    """
    Length oracle - returns the length of a string.

    Example in Bend:
        result = oracle(Oracle/Length { text: "hello" })
        # Returns: 5
    """

    pattern = r'Oracle/Length[^{]*\{\s*text:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"text": match.group(1)}

    def execute(self, params):
        return len(params["text"])


@register_oracle("WordCount")
class WordCountOracle(OraclePlugin):
    """
    WordCount oracle - counts words in text.

    Example in Bend:
        result = oracle(Oracle/WordCount { text: "hello world foo" })
        # Returns: 3
    """

    pattern = r'Oracle/WordCount[^{]*\{\s*text:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"text": match.group(1)}

    def execute(self, params):
        text = params["text"]
        return len(text.split())


@register_oracle("JSONParse")
class JSONParseOracle(OraclePlugin):
    """
    JSONParse oracle - parses JSON string.

    Example in Bend:
        result = oracle(Oracle/JSONParse { json: "{\"key\": \"value\"}" })
        # Returns: {"key": "value"}
    """

    pattern = r'Oracle/JSONParse[^{]*\{\s*json:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"json": match.group(1)}

    def execute(self, params):
        try:
            return json.loads(params["json"])
        except json.JSONDecodeError as e:
            return {"error": f"Invalid JSON: {e}"}


@register_oracle("Concat")
class ConcatOracle(OraclePlugin):
    """
    Concat oracle - concatenates two strings.

    Example in Bend:
        result = oracle(Oracle/Concat { a: "hello", b: "world" })
        # Returns: "helloworld"
    """

    pattern = r'Oracle/Concat[^{]*\{\s*a:\s*"([^"]+)",\s*b:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"a": match.group(1), "b": match.group(2)}

    def execute(self, params):
        return params["a"] + params["b"]


@register_oracle("Contains")
class ContainsOracle(OraclePlugin):
    """
    Contains oracle - checks if text contains substring.

    Example in Bend:
        result = oracle(Oracle/Contains { text: "hello world", substring: "world" })
        # Returns: true
    """

    pattern = r'Oracle/Contains[^{]*\{\s*text:\s*"([^"]+)",\s*substring:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"text": match.group(1), "substring": match.group(2)}

    def execute(self, params):
        return params["substring"] in params["text"]


@register_oracle("Replace")
class ReplaceOracle(OraclePlugin):
    """
    Replace oracle - replaces substring in text.

    Example in Bend:
        result = oracle(Oracle/Replace { text: "hello world", old: "world", new: "there" })
        # Returns: "hello there"
    """

    pattern = r'Oracle/Replace[^{]*\{\s*text:\s*"([^"]+)",\s*old:\s*"([^"]+)",\s*new:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {
            "text": match.group(1),
            "old": match.group(2),
            "new": match.group(3)
        }

    def execute(self, params):
        return params["text"].replace(params["old"], params["new"])


@register_oracle("Split")
class SplitOracle(OraclePlugin):
    """
    Split oracle - splits text by delimiter.

    Example in Bend:
        result = oracle(Oracle/Split { text: "a,b,c", delimiter: "," })
        # Returns: ["a", "b", "c"]
    """

    pattern = r'Oracle/Split[^{]*\{\s*text:\s*"([^"]+)",\s*delimiter:\s*"([^"]+)"\s*\}'

    def extract_params(self, match):
        return {"text": match.group(1), "delimiter": match.group(2)}

    def execute(self, params):
        return params["text"].split(params["delimiter"])


@register_oracle("Repeat")
class RepeatOracle(OraclePlugin):
    """
    Repeat oracle - repeats text N times.

    Example in Bend:
        result = oracle(Oracle/Repeat { text: "ha", count: 3 })
        # Returns: "hahaha"
    """

    pattern = r'Oracle/Repeat[^{]*\{\s*text:\s*"([^"]+)",\s*count:\s*(\d+)\s*\}'

    def extract_params(self, match):
        return {"text": match.group(1), "count": int(match.group(2))}

    def execute(self, params):
        return params["text"] * params["count"]
