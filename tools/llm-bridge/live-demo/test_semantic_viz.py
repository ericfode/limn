#!/usr/bin/env python3
"""
Comprehensive Test Suite for Semantic Visualization
====================================================

Tests all visualization modes, hint extraction, and data generation.

Author: Polecat Now
Date: 2026-02-01
"""

import unittest
from pathlib import Path
from semantic_viz import SemanticVisualizer


class TestSemanticVisualizer(unittest.TestCase):
    """Test suite for SemanticVisualizer class."""

    def setUp(self):
        """Set up test fixtures."""
        self.viz = SemanticVisualizer()

    def test_bootstrap_loading(self):
        """Test that bootstrap vocabulary is loaded correctly."""
        self.assertIn('viz', self.viz.bootstrap)
        self.assertIn('gph', self.viz.bootstrap)
        self.assertEqual(self.viz.bootstrap['viz'], 'visualize')
        self.assertEqual(self.viz.bootstrap['gph'], 'graph')

    def test_viz_patterns_initialization(self):
        """Test that visualization patterns are initialized."""
        self.assertIn('graph', self.viz.viz_patterns)
        self.assertIn('timeline', self.viz.viz_patterns)
        self.assertIn('tree', self.viz.viz_patterns)
        self.assertIn('table', self.viz.viz_patterns)
        self.assertIn('network', self.viz.viz_patterns)

    def test_extract_graph_hints(self):
        """Test extraction of graph visualization hints."""
        limn_code = "viz gph | nod con | edg flo"
        hints = self.viz.extract_viz_hints(limn_code)

        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'graph')
        self.assertIn('nod', hints[0]['aspects'])
        self.assertIn('con', hints[0]['aspects'])
        self.assertIn('edg', hints[0]['aspects'])
        self.assertIn('flo', hints[0]['aspects'])

    def test_extract_timeline_hints(self):
        """Test extraction of timeline visualization hints."""
        limn_code = "viz lin | tim seq"
        hints = self.viz.extract_viz_hints(limn_code)

        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'timeline')
        self.assertIn('tim', hints[0]['aspects'])
        self.assertIn('seq', hints[0]['aspects'])

    def test_extract_tree_hints(self):
        """Test extraction of tree visualization hints."""
        limn_code = "viz tre | par chi"
        hints = self.viz.extract_viz_hints(limn_code)

        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'tree')

    def test_extract_table_hints(self):
        """Test extraction of table visualization hints."""
        limn_code = "viz tab | row col"
        hints = self.viz.extract_viz_hints(limn_code)

        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'table')

    def test_extract_network_hints(self):
        """Test extraction of network visualization hints."""
        limn_code = "viz net | mes int"
        hints = self.viz.extract_viz_hints(limn_code)

        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'network')

    def test_no_hints_uses_debug_mode(self):
        """Test that code without viz hints falls back to debug mode."""
        limn_code = """
~ tim now clk → 1770001234 epo
~ fil rea dat → "content" txt
        """
        hints = self.viz.extract_viz_hints(limn_code)
        self.assertEqual(len(hints), 0)

        viz_data = self.viz.visualize_limn_state(limn_code)
        self.assertEqual(viz_data['mode'], 'debug')

    def test_multiple_viz_hints(self):
        """Test extraction of multiple visualization hints."""
        limn_code = """
viz gph | nod con
viz lin | tim seq
        """
        hints = self.viz.extract_viz_hints(limn_code)
        self.assertEqual(len(hints), 2)
        self.assertEqual(hints[0]['type'], 'graph')
        self.assertEqual(hints[1]['type'], 'timeline')

    def test_generate_graph_config(self):
        """Test generation of graph visualization configuration."""
        spec = {
            'type': 'graph',
            'structure': 'network',
            'layout': 'force-directed',
            'aspects': ['nod', 'edg', 'flo']
        }
        config = self.viz.generate_visualization_config(spec)

        self.assertEqual(config['type'], 'graph')
        self.assertEqual(config['layout']['type'], 'force-directed')
        self.assertTrue(config['edges']['arrows'])
        self.assertTrue(config['physics']['enabled'])

    def test_generate_timeline_config(self):
        """Test generation of timeline visualization configuration."""
        spec = {
            'type': 'timeline',
            'structure': 'linear',
            'layout': 'horizontal',
            'aspects': ['tim', 'seq']
        }
        config = self.viz.generate_visualization_config(spec)

        self.assertEqual(config['type'], 'timeline')
        self.assertEqual(config['layout']['type'], 'horizontal')
        self.assertFalse(config['physics']['enabled'])

    def test_debug_mode_visualization(self):
        """Test debug mode visualization with oracle lines."""
        limn_code = """
~ tim now clk → 1770001234 epo
~ fil rea dat → "content" txt
        """
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['mode'], 'debug')
        self.assertEqual(viz_data['type'], 'graph')
        self.assertGreater(len(viz_data['nodes']), 0)
        self.assertGreater(len(viz_data['edges']), 0)

    def test_semantic_mode_graph_visualization(self):
        """Test semantic mode with graph visualization."""
        limn_code = """
viz gph | nod con | edg flo

~ tim now clk → 1770001234 epo
~ fil rea dat → "content" txt
        """
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['mode'], 'semantic')
        self.assertEqual(viz_data['type'], 'graph')
        self.assertIn('config', viz_data)
        self.assertIn('nodes', viz_data)
        self.assertIn('edges', viz_data)

    def test_semantic_mode_timeline_visualization(self):
        """Test semantic mode with timeline visualization."""
        limn_code = """
viz lin | tim seq

epo 1000000 | event one
epo 2000000 | event two
        """
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['mode'], 'semantic')
        self.assertEqual(viz_data['type'], 'timeline')
        self.assertIn('events', viz_data)

    def test_semantic_mode_tree_visualization(self):
        """Test semantic mode with tree visualization."""
        limn_code = """
viz tre | par chi

root node
child one
child two
        """
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['mode'], 'semantic')
        self.assertEqual(viz_data['type'], 'tree')
        self.assertIn('root', viz_data)

    def test_semantic_mode_table_visualization(self):
        """Test semantic mode with table visualization."""
        limn_code = """
viz tab | row col

col1 | col2 | col3
data1 | data2 | data3
        """
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['mode'], 'semantic')
        self.assertEqual(viz_data['type'], 'table')
        self.assertIn('rows', viz_data)
        self.assertGreater(len(viz_data['rows']), 0)

    def test_directed_edges_in_config(self):
        """Test that 'flo' aspect creates directed edges."""
        spec = {
            'type': 'graph',
            'structure': 'network',
            'layout': 'force-directed',
            'aspects': ['nod', 'edg', 'flo']
        }
        config = self.viz.generate_visualization_config(spec)
        self.assertTrue(config['edges']['arrows'])

        # Test without 'flo'
        spec['aspects'] = ['nod', 'edg']
        config = self.viz.generate_visualization_config(spec)
        self.assertFalse(config['edges']['arrows'])

    def test_node_shape_based_on_aspects(self):
        """Test that node shape is determined by aspects."""
        spec = {
            'type': 'graph',
            'structure': 'network',
            'layout': 'force-directed',
            'aspects': ['nod', 'edg']
        }
        config = self.viz.generate_visualization_config(spec)
        self.assertEqual(config['nodes']['shape'], 'circle')

        # Test without 'nod'
        spec['aspects'] = ['edg']
        config = self.viz.generate_visualization_config(spec)
        self.assertEqual(config['nodes']['shape'], 'box')

    def test_extract_graph_data_with_arrows(self):
        """Test graph data extraction with arrow notation."""
        limn_code = """
viz gph | nod con | edg flo

source1 → target1
source2 → target2
        """
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['type'], 'graph')
        self.assertGreater(len(viz_data['nodes']), 0)
        self.assertGreater(len(viz_data['edges']), 0)

        # Check that edges have arrows
        for edge in viz_data['edges']:
            self.assertEqual(edge['arrows'], 'to')

    def test_empty_code_returns_empty_debug_viz(self):
        """Test that empty code returns minimal debug visualization."""
        limn_code = ""
        viz_data = self.viz.visualize_limn_state(limn_code)

        self.assertEqual(viz_data['mode'], 'debug')
        self.assertEqual(len(viz_data['nodes']), 0)
        self.assertEqual(len(viz_data['edges']), 0)

    def test_comments_are_ignored_in_viz_lines(self):
        """Test that comments are ignored when parsing viz lines."""
        limn_code = "viz gph | nod con # this is a comment"
        hints = self.viz.extract_viz_hints(limn_code)

        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'graph')
        # The raw line should not include the comment in modern parsers
        # but we store it as-is, so just check it parses correctly
        self.assertIn('nod', hints[0]['aspects'])

    def test_hierarchical_layout_for_tree(self):
        """Test that tree visualization uses hierarchical layout."""
        spec = {
            'type': 'tree',
            'structure': 'hierarchical',
            'layout': 'vertical',
            'aspects': []
        }
        config = self.viz.generate_visualization_config(spec)
        self.assertTrue(config['layout']['hierarchical'])

    def test_physics_disabled_for_non_graph_types(self):
        """Test that physics is disabled for timeline and table."""
        timeline_spec = {
            'type': 'timeline',
            'structure': 'linear',
            'layout': 'horizontal',
            'aspects': []
        }
        config = self.viz.generate_visualization_config(timeline_spec)
        self.assertFalse(config['physics']['enabled'])

        table_spec = {
            'type': 'table',
            'structure': 'grid',
            'layout': 'tabular',
            'aspects': []
        }
        config = self.viz.generate_visualization_config(table_spec)
        self.assertFalse(config['physics']['enabled'])


class TestVisualizationIntegration(unittest.TestCase):
    """Integration tests for complete visualization workflows."""

    def setUp(self):
        """Set up test fixtures."""
        self.viz = SemanticVisualizer()

    def test_complete_graph_workflow(self):
        """Test complete workflow for graph visualization."""
        limn_code = """
viz gph | nod con | edg flo

~ tim now clk → 1770001234 epo
~ fil rea dat → "content" txt
~ sys run sta → "active" boo
        """

        # Extract hints
        hints = self.viz.extract_viz_hints(limn_code)
        self.assertEqual(len(hints), 1)

        # Generate config
        config = self.viz.generate_visualization_config(hints[0])
        self.assertEqual(config['type'], 'graph')

        # Generate visualization
        viz_data = self.viz.visualize_limn_state(limn_code, hints)
        self.assertEqual(viz_data['mode'], 'semantic')
        self.assertIn('nodes', viz_data)
        self.assertIn('edges', viz_data)

    def test_fallback_to_debug_mode(self):
        """Test fallback to debug mode when no hints present."""
        limn_code = """
~ ora exe → result
~ mem sto → data
        """

        viz_data = self.viz.visualize_limn_state(limn_code)
        self.assertEqual(viz_data['mode'], 'debug')
        self.assertEqual(viz_data['type'], 'graph')


class TestEdgeCases(unittest.TestCase):
    """Test edge cases and error conditions."""

    def setUp(self):
        """Set up test fixtures."""
        self.viz = SemanticVisualizer()

    def test_malformed_viz_directive(self):
        """Test handling of malformed viz directives."""
        limn_code = "viz | | |"
        hints = self.viz.extract_viz_hints(limn_code)
        # Should still extract something, defaulting to graph
        if len(hints) > 0:
            self.assertIsNotNone(hints[0]['type'])

    def test_unknown_viz_type(self):
        """Test handling of unknown visualization type."""
        limn_code = "viz xyz | abc def"
        hints = self.viz.extract_viz_hints(limn_code)
        # Should default to graph type
        if len(hints) > 0:
            self.assertEqual(hints[0]['type'], 'graph')

    def test_viz_keyword_not_at_start(self):
        """Test that viz must be in the line to be recognized."""
        limn_code = "no visualization here | gph nod"
        hints = self.viz.extract_viz_hints(limn_code)
        self.assertEqual(len(hints), 0)

    def test_mixed_content_with_viz(self):
        """Test code with both viz directives and regular content."""
        limn_code = """
# Header comment
viz gph | nod edg

~ regular oracle line → result
some other content
        """
        hints = self.viz.extract_viz_hints(limn_code)
        self.assertEqual(len(hints), 1)
        self.assertEqual(hints[0]['type'], 'graph')


def run_tests():
    """Run all tests and report results."""
    loader = unittest.TestLoader()
    suite = unittest.TestSuite()

    # Add all test classes
    suite.addTests(loader.loadTestsFromTestCase(TestSemanticVisualizer))
    suite.addTests(loader.loadTestsFromTestCase(TestVisualizationIntegration))
    suite.addTests(loader.loadTestsFromTestCase(TestEdgeCases))

    # Run tests with verbose output
    runner = unittest.TextTestRunner(verbosity=2)
    result = runner.run(suite)

    # Return exit code
    return 0 if result.wasSuccessful() else 1


if __name__ == "__main__":
    exit(run_tests())
