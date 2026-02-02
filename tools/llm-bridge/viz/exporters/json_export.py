"""JSON export for visualizations."""

import json
from typing import Any, Dict
from pathlib import Path


class JsonExporter:
    """Export visualizations as JSON."""

    @staticmethod
    def export(data: Dict[str, Any], output_path: Path):
        """
        Export data as formatted JSON.

        Args:
            data: Dictionary to export
            output_path: Output file path
        """
        with open(output_path, 'w') as f:
            json.dump(data, f, indent=2)

    @staticmethod
    def export_string(data: Dict[str, Any]) -> str:
        """
        Export data as JSON string.

        Args:
            data: Dictionary to export

        Returns:
            JSON string
        """
        return json.dumps(data, indent=2)
