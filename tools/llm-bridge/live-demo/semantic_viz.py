#!/usr/bin/env python3
"""
Semantic Visualization - LMN Keywords Drive Display
===================================================

Two visualization modes:
1. Debug Mode: Parse Limn internals (existing)
2. Semantic Mode: LMN code contains viz hints (NEW)

Example:
    viz gph | nod con | edg flo
    > "visualize as graph with connected nodes and flowing edges"

The LMN code itself tells us how to visualize it.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import re
from typing import Dict, List, Any
from pathlib import Path


class SemanticVisualizer:
    """Extract and interpret visualization hints from LMN code."""

    def __init__(self, bootstrap_path: Path = None):
        """Initialize with bootstrap vocabulary."""
        self.bootstrap = self._load_bootstrap(bootstrap_path)
        self.viz_patterns = self._init_viz_patterns()

    def _load_bootstrap(self, path: Path = None) -> Dict[str, str]:
        """Load bootstrap vocabulary."""
        if path and path.exists():
            content = path.read_text()
            # Parse bootstrap definitions
            vocab = {}
            for line in content.split('\n'):
                if '|' in line and not line.startswith('#'):
                    parts = line.split('|')
                    if len(parts) == 2:
                        word = parts[0].strip()
                        meaning = parts[1].strip()
                        vocab[word] = meaning
            return vocab

        # Default minimal bootstrap
        return {
            'viz': 'visualize',
            'gph': 'graph',
            'lin': 'timeline',
            'tre': 'tree',
            'tab': 'table',
            'net': 'network',
            'nod': 'nodes',
            'edg': 'edges',
            'con': 'connected',
            'flo': 'flow'
        }

    def _init_viz_patterns(self) -> Dict[str, Dict]:
        """Initialize visualization type patterns."""
        return {
            'graph': {
                'keywords': ['gph', 'nod', 'edg', 'con'],
                'structure': 'network',
                'layout': 'force-directed',
                'elements': ['nodes', 'edges', 'connections']
            },
            'timeline': {
                'keywords': ['lin', 'tim', 'seq', 'ord'],
                'structure': 'linear',
                'layout': 'horizontal',
                'elements': ['events', 'timestamps', 'sequence']
            },
            'tree': {
                'keywords': ['tre', 'hie', 'par', 'chi'],
                'structure': 'hierarchical',
                'layout': 'vertical',
                'elements': ['nodes', 'parent', 'children']
            },
            'table': {
                'keywords': ['tab', 'row', 'col', 'cel'],
                'structure': 'grid',
                'layout': 'tabular',
                'elements': ['rows', 'columns', 'cells']
            },
            'network': {
                'keywords': ['net', 'mes', 'int', 'rel'],
                'structure': 'mesh',
                'layout': 'organic',
                'elements': ['nodes', 'links', 'relationships']
            }
        }

    def extract_viz_hints(self, limn_code: str) -> List[Dict[str, Any]]:
        """Extract visualization hints from LMN code.

        Looks for patterns like:
            viz gph | nod con | edg flo
            viz lin | tim seq
            viz tre | par chi

        Returns list of visualization specifications.
        """
        hints = []

        # Find viz directive lines
        viz_lines = [line for line in limn_code.split('\n')
                     if 'viz' in line.lower()]

        for line in viz_lines:
            hint = self._parse_viz_line(line)
            if hint:
                hints.append(hint)

        return hints

    def _parse_viz_line(self, line: str) -> Dict[str, Any]:
        """Parse a single viz directive line.

        Format: viz <type> | <aspect1> <aspect2> | <aspect3> <aspect4>
        """
        # Remove comments
        if '#' in line:
            line = line[:line.index('#')]

        # Split by pipes
        parts = [p.strip() for p in line.split('|')]

        if not parts:
            return None

        # First part should contain 'viz' and type
        first = parts[0].split()
        if 'viz' not in first:
            return None

        # Identify visualization type
        viz_type = None
        for word in first:
            if word != 'viz':
                for vtype, vspec in self.viz_patterns.items():
                    if word in vspec['keywords']:
                        viz_type = vtype
                        break

        if not viz_type:
            viz_type = 'graph'  # Default

        # Extract aspects from remaining parts
        aspects = []
        for part in parts[1:]:
            aspects.extend(part.split())

        # Build visualization spec
        spec = {
            'type': viz_type,
            'structure': self.viz_patterns[viz_type]['structure'],
            'layout': self.viz_patterns[viz_type]['layout'],
            'elements': self.viz_patterns[viz_type]['elements'],
            'aspects': aspects,
            'raw': line.strip()
        }

        return spec

    def generate_visualization_config(self, spec: Dict[str, Any]) -> Dict[str, Any]:
        """Generate visualization configuration from spec.

        Converts LMN viz hints into concrete visualization parameters.
        """
        config = {
            'type': spec['type'],
            'layout': {
                'type': spec['layout'],
                'directed': 'flo' in spec['aspects'],
                'hierarchical': spec['structure'] == 'hierarchical'
            },
            'nodes': {
                'shape': 'circle' if 'nod' in spec['aspects'] else 'box',
                'size': 'auto',
                'color': 'auto'
            },
            'edges': {
                'arrows': 'flo' in spec['aspects'] or 'dir' in spec['aspects'],
                'smooth': True,
                'width': 2
            },
            'physics': {
                'enabled': spec['type'] in ['graph', 'network'],
                'solver': 'forceAtlas2Based'
            }
        }

        return config

    def visualize_limn_state(self, limn_state: str, hints: List[Dict] = None) -> Dict:
        """Generate visualization data from Limn state.

        Args:
            limn_state: Current Limn state string
            hints: Optional pre-extracted hints

        Returns:
            Visualization data structure
        """
        if hints is None:
            hints = self.extract_viz_hints(limn_state)

        # If no hints, use debug mode (parse structure)
        if not hints:
            return self._debug_mode_viz(limn_state)

        # Use semantic mode (follow hints)
        return self._semantic_mode_viz(limn_state, hints[0] if hints else None)

    def _debug_mode_viz(self, limn_state: str) -> Dict:
        """Debug mode: Parse Limn structure and infer visualization."""
        # Extract oracle lines
        oracle_lines = [line for line in limn_state.split('\n')
                       if '~' in line or '→' in line]

        nodes = []
        edges = []

        for idx, line in enumerate(oracle_lines):
            # Create node for each oracle
            if '~' in line:
                parts = line.split('→')
                oracle_part = parts[0].strip()
                result_part = parts[1].strip() if len(parts) > 1 else ''

                nodes.append({
                    'id': f'oracle_{idx}',
                    'label': oracle_part,
                    'result': result_part,
                    'type': 'oracle'
                })

                # Add edge to result if present
                if result_part:
                    edges.append({
                        'from': f'oracle_{idx}',
                        'to': f'result_{idx}',
                        'label': '→'
                    })
                    nodes.append({
                        'id': f'result_{idx}',
                        'label': result_part,
                        'type': 'result'
                    })

        return {
            'mode': 'debug',
            'type': 'graph',
            'nodes': nodes,
            'edges': edges
        }

    def _semantic_mode_viz(self, limn_state: str, hint: Dict) -> Dict:
        """Semantic mode: Follow visualization hints in LMN code."""
        if not hint:
            return self._debug_mode_viz(limn_state)

        # Generate config from hint
        config = self.generate_visualization_config(hint)

        # Extract data based on viz type
        if hint['type'] == 'graph' or hint['type'] == 'network':
            return self._extract_graph_data(limn_state, config)
        elif hint['type'] == 'timeline':
            return self._extract_timeline_data(limn_state, config)
        elif hint['type'] == 'tree':
            return self._extract_tree_data(limn_state, config)
        elif hint['type'] == 'table':
            return self._extract_table_data(limn_state, config)

        return self._debug_mode_viz(limn_state)

    def _extract_graph_data(self, limn_state: str, config: Dict) -> Dict:
        """Extract graph visualization data."""
        nodes = []
        edges = []

        lines = limn_state.split('\n')
        node_id = 0

        for line in lines:
            if '→' in line:
                parts = line.split('→')
                source = parts[0].strip()
                target = parts[1].strip() if len(parts) > 1 else None

                source_id = f'node_{node_id}'
                nodes.append({
                    'id': source_id,
                    'label': source,
                    'shape': config['nodes']['shape']
                })
                node_id += 1

                if target:
                    target_id = f'node_{node_id}'
                    nodes.append({
                        'id': target_id,
                        'label': target,
                        'shape': config['nodes']['shape']
                    })
                    edges.append({
                        'from': source_id,
                        'to': target_id,
                        'arrows': 'to' if config['edges']['arrows'] else 'none'
                    })
                    node_id += 1

        return {
            'mode': 'semantic',
            'type': 'graph',
            'config': config,
            'nodes': nodes,
            'edges': edges
        }

    def _extract_timeline_data(self, limn_state: str, config: Dict) -> Dict:
        """Extract timeline visualization data."""
        events = []

        for line in limn_state.split('\n'):
            if 'epo' in line:  # Epoch timestamp
                parts = line.split()
                timestamp = next((p for p in parts if p.isdigit()), None)
                if timestamp:
                    events.append({
                        'timestamp': int(timestamp),
                        'label': line.strip(),
                        'type': 'event'
                    })

        return {
            'mode': 'semantic',
            'type': 'timeline',
            'config': config,
            'events': sorted(events, key=lambda e: e['timestamp'])
        }

    def _extract_tree_data(self, limn_state: str, config: Dict) -> Dict:
        """Extract tree visualization data."""
        # Simplified tree extraction
        root = {'id': 'root', 'label': 'System', 'children': []}

        lines = [l.strip() for l in limn_state.split('\n') if l.strip()]
        for idx, line in enumerate(lines):
            root['children'].append({
                'id': f'node_{idx}',
                'label': line
            })

        return {
            'mode': 'semantic',
            'type': 'tree',
            'config': config,
            'root': root
        }

    def _extract_table_data(self, limn_state: str, config: Dict) -> Dict:
        """Extract table visualization data."""
        rows = []

        for line in limn_state.split('\n'):
            if '|' in line:
                cells = [c.strip() for c in line.split('|')]
                rows.append({'cells': cells})

        return {
            'mode': 'semantic',
            'type': 'table',
            'config': config,
            'rows': rows
        }


def main():
    """Test semantic visualization."""
    viz = SemanticVisualizer()

    # Test with viz hints
    test_limn = """
viz gph | nod con | edg flo

~ tim now clk → 1770001234 epo
~ fil rea dat → "content" txt
sys run | ora exe
    """

    hints = viz.extract_viz_hints(test_limn)
    print("Extracted hints:", hints)

    viz_data = viz.visualize_limn_state(test_limn)
    print("\nVisualization data:", viz_data)


if __name__ == "__main__":
    main()
