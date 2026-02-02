#!/usr/bin/env python3
"""
Derived Model Generation Engine
================================

Implements LMN model transformation and generation:
- der (derive new model from existing)
- tra (transform representation)
- gen (generate new structure)
- mod (model manipulation)

Meta-programming: The system can transform itself.

Author: Rex (Engineer)
Date: 2026-02-01
"""

import re
from typing import Dict, List, Any, Optional
from dataclasses import dataclass
from enum import Enum


class ModelType(Enum):
    """Types of models that can be derived."""
    GRAPH = "graph"
    TREE = "tree"
    TABLE = "table"
    TIMELINE = "timeline"
    NETWORK = "network"
    STATE_MACHINE = "state_machine"
    PIPELINE = "pipeline"


class TransformationType(Enum):
    """Types of transformations that can be applied."""
    SIMPLIFY = "simplify"      # Remove complexity
    EXPAND = "expand"          # Add detail
    COMPOSE = "compose"        # Combine models
    DECOMPOSE = "decompose"    # Split into parts
    INVERT = "invert"          # Reverse structure
    ABSTRACT = "abstract"      # Generalize
    CONCRETIZE = "concretize"  # Specialize


@dataclass
class Model:
    """A derived model representation."""
    type: ModelType
    structure: Dict[str, Any]
    metadata: Dict[str, Any]
    limn_repr: str


class ModelEngine:
    """Generate and transform models based on LMN state."""

    def __init__(self):
        """Initialize model engine."""
        self.models = {}  # Registry of generated models
        self.transformations = self._init_transformations()

    def _init_transformations(self) -> Dict[str, callable]:
        """Initialize transformation functions."""
        return {
            'simplify': self._transform_simplify,
            'expand': self._transform_expand,
            'compose': self._transform_compose,
            'decompose': self._transform_decompose,
            'invert': self._transform_invert,
            'abstract': self._transform_abstract,
            'concretize': self._transform_concretize
        }

    def derive_model(self, source_state: str, model_type: str = None) -> Model:
        """Derive a new model from source state.

        LMN: ~ der mod @ source_state → new_model

        Args:
            source_state: Limn representation of current state
            model_type: Target model type (inferred if None)

        Returns:
            Derived model
        """
        # Infer model type from source if not specified
        if model_type is None:
            model_type = self._infer_model_type(source_state)

        # Extract structure based on type
        structure = self._extract_structure(source_state, model_type)

        # Generate Limn representation
        limn_repr = self._generate_limn_repr(structure, model_type)

        # Build model
        model = Model(
            type=ModelType(model_type),
            structure=structure,
            metadata={
                'source_lines': len(source_state.split('\n')),
                'derived_at': 'epo',  # Would use actual timestamp
                'complexity': self._calculate_complexity(structure)
            },
            limn_repr=limn_repr
        )

        return model

    def _infer_model_type(self, state: str) -> str:
        """Infer the best model type from state structure."""
        # Count indicators
        indicators = {
            'graph': len(re.findall(r'→', state)),
            'tree': len(re.findall(r'par|chi', state)),
            'timeline': len(re.findall(r'tim|seq|ord', state)),
            'table': len(re.findall(r'\|', state)),
            'state_machine': len(re.findall(r'sta|tra|eve', state)),
            'pipeline': len(re.findall(r'→.*→', state))
        }

        # Return type with most indicators
        return max(indicators.items(), key=lambda x: x[1])[0]

    def _extract_structure(self, state: str, model_type: str) -> Dict[str, Any]:
        """Extract structure based on model type."""
        extractors = {
            'graph': self._extract_graph,
            'tree': self._extract_tree,
            'timeline': self._extract_timeline,
            'table': self._extract_table,
            'state_machine': self._extract_state_machine,
            'pipeline': self._extract_pipeline
        }

        extractor = extractors.get(model_type, self._extract_graph)
        return extractor(state)

    def _extract_graph(self, state: str) -> Dict[str, Any]:
        """Extract graph structure from state."""
        nodes = set()
        edges = []

        for line in state.split('\n'):
            if '→' in line:
                parts = line.split('→')
                source = parts[0].strip()
                target = parts[1].strip() if len(parts) > 1 else None

                nodes.add(source)
                if target:
                    nodes.add(target)
                    edges.append({'from': source, 'to': target})

        return {
            'nodes': [{'id': n, 'label': n} for n in nodes],
            'edges': edges
        }

    def _extract_tree(self, state: str) -> Dict[str, Any]:
        """Extract tree structure from state."""
        # Simplified tree extraction
        root = {'id': 'root', 'children': []}

        lines = [l.strip() for l in state.split('\n') if l.strip()]
        for idx, line in enumerate(lines):
            root['children'].append({
                'id': f'node_{idx}',
                'label': line
            })

        return root

    def _extract_timeline(self, state: str) -> Dict[str, Any]:
        """Extract timeline structure from state."""
        events = []

        for line in state.split('\n'):
            # Look for epoch timestamps
            if 'epo' in line:
                parts = line.split()
                timestamp = next((p for p in parts if p.isdigit()), None)
                if timestamp:
                    events.append({
                        'timestamp': int(timestamp),
                        'label': line.strip()
                    })

        return {
            'events': sorted(events, key=lambda e: e['timestamp']),
            'start': events[0]['timestamp'] if events else 0,
            'end': events[-1]['timestamp'] if events else 0
        }

    def _extract_table(self, state: str) -> Dict[str, Any]:
        """Extract table structure from state."""
        rows = []

        for line in state.split('\n'):
            if '|' in line:
                cells = [c.strip() for c in line.split('|')]
                rows.append({'cells': cells})

        return {'rows': rows, 'columns': len(rows[0]['cells']) if rows else 0}

    def _extract_state_machine(self, state: str) -> Dict[str, Any]:
        """Extract state machine structure."""
        states = set()
        transitions = []

        for line in state.split('\n'):
            if '→' in line:
                parts = line.split('→')
                from_state = parts[0].strip()
                to_state = parts[1].strip() if len(parts) > 1 else None

                states.add(from_state)
                if to_state:
                    states.add(to_state)
                    transitions.append({
                        'from': from_state,
                        'to': to_state,
                        'event': 'transition'
                    })

        return {
            'states': list(states),
            'transitions': transitions,
            'initial': list(states)[0] if states else None
        }

    def _extract_pipeline(self, state: str) -> Dict[str, Any]:
        """Extract pipeline structure."""
        stages = []

        for line in state.split('\n'):
            if '→' in line:
                parts = line.split('→')
                for part in parts:
                    stage = part.strip()
                    if stage and stage not in stages:
                        stages.append(stage)

        return {
            'stages': stages,
            'flow': 'sequential'
        }

    def _generate_limn_repr(self, structure: Dict, model_type: str) -> str:
        """Generate Limn representation of model."""
        lines = []

        # Add type header
        lines.append(f"# Derived Model: {model_type}")
        lines.append(f"mod {model_type[:3]} | der fro sta")

        # Generate based on type
        if model_type == 'graph':
            lines.append(f"nod {len(structure['nodes'])} | edg {len(structure['edges'])}")
            for edge in structure['edges'][:5]:  # First 5
                lines.append(f"{edge['from']} → {edge['to']}")

        elif model_type == 'timeline':
            lines.append(f"eve {len(structure['events'])}")
            for event in structure['events'][:5]:
                lines.append(f"tim {event['timestamp']} epo | {event['label']}")

        elif model_type == 'state_machine':
            lines.append(f"sta {len(structure['states'])} | tra {len(structure['transitions'])}")
            for trans in structure['transitions'][:5]:
                lines.append(f"{trans['from']} → {trans['to']}")

        return '\n'.join(lines)

    def _calculate_complexity(self, structure: Dict) -> int:
        """Calculate structural complexity score."""
        # Simple heuristic: count total elements
        total = 0
        for value in structure.values():
            if isinstance(value, list):
                total += len(value)
            elif isinstance(value, dict):
                total += len(value)
        return total

    def transform_model(self, model: Model, transformation: str, **params) -> Model:
        """Transform an existing model.

        LMN: ~ tra mod @ transformation → new_model

        Args:
            model: Source model
            transformation: Type of transformation
            **params: Transformation parameters

        Returns:
            Transformed model
        """
        if transformation not in self.transformations:
            raise ValueError(f"Unknown transformation: {transformation}")

        transform_fn = self.transformations[transformation]
        new_structure = transform_fn(model.structure, **params)

        # Generate new model
        return Model(
            type=model.type,
            structure=new_structure,
            metadata={
                **model.metadata,
                'transformed_from': model.limn_repr[:50],
                'transformation': transformation
            },
            limn_repr=self._generate_limn_repr(new_structure, model.type.value)
        )

    def _transform_simplify(self, structure: Dict, threshold: float = 0.5) -> Dict:
        """Simplify model by removing low-importance elements."""
        if 'nodes' in structure and 'edges' in structure:
            # Keep only most connected nodes
            node_degrees = {}
            for edge in structure['edges']:
                node_degrees[edge['from']] = node_degrees.get(edge['from'], 0) + 1
                node_degrees[edge['to']] = node_degrees.get(edge['to'], 0) + 1

            max_degree = max(node_degrees.values()) if node_degrees else 1
            min_degree = max_degree * threshold

            important_nodes = {n for n, d in node_degrees.items() if d >= min_degree}

            return {
                'nodes': [n for n in structure['nodes'] if n['id'] in important_nodes],
                'edges': [e for e in structure['edges']
                         if e['from'] in important_nodes and e['to'] in important_nodes]
            }

        return structure

    def _transform_expand(self, structure: Dict, factor: int = 2) -> Dict:
        """Expand model by adding detail."""
        # Simplified: duplicate elements
        if 'nodes' in structure:
            expanded_nodes = structure['nodes'][:]
            for i, node in enumerate(structure['nodes']):
                for j in range(factor - 1):
                    expanded_nodes.append({
                        'id': f"{node['id']}_exp_{j}",
                        'label': f"{node['label']} (expanded)"
                    })
            structure['nodes'] = expanded_nodes

        return structure

    def _transform_compose(self, structure: Dict, other: Dict = None) -> Dict:
        """Compose two models into one."""
        if other is None:
            return structure

        # Merge structures
        composed = {}
        for key in set(structure.keys()) | set(other.keys()):
            if key in structure and key in other:
                if isinstance(structure[key], list):
                    composed[key] = structure[key] + other[key]
                else:
                    composed[key] = structure[key]
            elif key in structure:
                composed[key] = structure[key]
            else:
                composed[key] = other[key]

        return composed

    def _transform_decompose(self, structure: Dict, parts: int = 2) -> List[Dict]:
        """Decompose model into smaller parts."""
        # Split nodes into groups
        if 'nodes' in structure:
            chunk_size = len(structure['nodes']) // parts
            decomposed = []

            for i in range(parts):
                start = i * chunk_size
                end = start + chunk_size if i < parts - 1 else len(structure['nodes'])
                node_subset = structure['nodes'][start:end]
                node_ids = {n['id'] for n in node_subset}

                decomposed.append({
                    'nodes': node_subset,
                    'edges': [e for e in structure.get('edges', [])
                             if e['from'] in node_ids and e['to'] in node_ids]
                })

            return decomposed

        return [structure]

    def _transform_invert(self, structure: Dict) -> Dict:
        """Invert model structure."""
        if 'edges' in structure:
            # Reverse all edges
            inverted_edges = [
                {'from': e['to'], 'to': e['from']}
                for e in structure['edges']
            ]
            structure['edges'] = inverted_edges

        return structure

    def _transform_abstract(self, structure: Dict, level: int = 1) -> Dict:
        """Abstract model to higher level."""
        # Group similar elements
        if 'nodes' in structure:
            # Simple abstraction: group by first word
            groups = {}
            for node in structure['nodes']:
                key = node['label'].split()[0] if ' ' in node['label'] else node['label']
                if key not in groups:
                    groups[key] = []
                groups[key].append(node)

            abstract_nodes = [
                {'id': key, 'label': f"{key} ({len(nodes)} items)"}
                for key, nodes in groups.items()
            ]

            return {'nodes': abstract_nodes, 'edges': []}

        return structure

    def _transform_concretize(self, structure: Dict, details: Dict = None) -> Dict:
        """Concretize model with specific details."""
        # Add details to each node
        if 'nodes' in structure and details:
            for node in structure['nodes']:
                node.update(details)

        return structure

    def generate_model(self, spec: Dict[str, Any]) -> Model:
        """Generate a new model from specification.

        LMN: ~ gen mod @ spec → new_model

        Args:
            spec: Model specification with type and parameters

        Returns:
            Generated model
        """
        model_type = spec.get('type', 'graph')
        params = spec.get('params', {})

        # Generate structure based on type
        if model_type == 'graph':
            structure = self._generate_graph(**params)
        elif model_type == 'tree':
            structure = self._generate_tree(**params)
        elif model_type == 'state_machine':
            structure = self._generate_state_machine(**params)
        else:
            structure = {}

        return Model(
            type=ModelType(model_type),
            structure=structure,
            metadata={'generated': True, 'spec': spec},
            limn_repr=self._generate_limn_repr(structure, model_type)
        )

    def _generate_graph(self, nodes: int = 5, edges: int = 7) -> Dict:
        """Generate a graph structure."""
        node_list = [{'id': f'n{i}', 'label': f'Node {i}'} for i in range(nodes)]
        edge_list = [
            {'from': f'n{i}', 'to': f'n{(i+1) % nodes}'}
            for i in range(min(edges, nodes))
        ]

        return {'nodes': node_list, 'edges': edge_list}

    def _generate_tree(self, depth: int = 3, branching: int = 2) -> Dict:
        """Generate a tree structure."""
        def build_tree(level, parent_id, max_depth):
            if level >= max_depth:
                return []

            children = []
            for i in range(branching):
                child_id = f"{parent_id}_{i}"
                child = {
                    'id': child_id,
                    'label': f'Level {level} Node {i}',
                    'children': build_tree(level + 1, child_id, max_depth)
                }
                children.append(child)

            return children

        return {
            'id': 'root',
            'label': 'Root',
            'children': build_tree(1, 'root', depth)
        }

    def _generate_state_machine(self, states: int = 4) -> Dict:
        """Generate a state machine structure."""
        state_list = [f'state_{i}' for i in range(states)]
        transitions = [
            {'from': state_list[i], 'to': state_list[(i+1) % states], 'event': f'event_{i}'}
            for i in range(states)
        ]

        return {
            'states': state_list,
            'transitions': transitions,
            'initial': state_list[0]
        }


def main():
    """Test model engine."""
    engine = ModelEngine()

    # Test derive
    test_state = """
sys run | ora exe
~ tim now clk → 1770001234 epo
~ fil rea dat → "content" txt
ora com | res mat
    """

    print("=== Deriving Model ===")
    model = engine.derive_model(test_state)
    print(f"Type: {model.type}")
    print(f"Structure: {model.structure}")
    print(f"Limn:\n{model.limn_repr}")

    print("\n=== Transforming Model (simplify) ===")
    simplified = engine.transform_model(model, 'simplify', threshold=0.3)
    print(f"Simplified:\n{simplified.limn_repr}")

    print("\n=== Generating Model ===")
    generated = engine.generate_model({
        'type': 'graph',
        'params': {'nodes': 5, 'edges': 6}
    })
    print(f"Generated:\n{generated.limn_repr}")


if __name__ == "__main__":
    main()
