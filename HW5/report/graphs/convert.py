#!/usr/bin/env python3
"""
DSM Ring Latency Data CSV Generator

Parses DSM ring test log output and generates CSV files with
add and lookup latency data in matrix format.

Usage:
    python generate_csv.py < test_output.log
    python generate_csv.py test_output.log
"""

import sys
import re
import csv
from collections import defaultdict

def parse_log(log_content):
    """
    Parse the log content and extract test configurations and latencies.
    
    Returns:
        List of tuples: (nodes, machines, entries, add_ops, look_ops)
    """
    tests = []
    lines = log_content.split('\n')
    
    i = 0
    while i < len(lines):
        line = lines[i].strip()
        
        # Look for test configuration line: "--> 1 Node(s), 1 Machine(s), 1000 Entries"
        match = re.search(r'(\d+)\s+Node\(s\),\s+(\d+)\s+Machine\(s\),\s+(\d+)\s+Entries', line)
        if match:
            nodes = int(match.group(1))
            machines = int(match.group(2))
            entries_per_machine = int(match.group(3))
            
            add_ops = []
            look_ops = []
            
            # Scan forward to find the operation results
            j = i + 1
            while j < len(lines):
                op_line = lines[j].strip()
                
                # Parse add operations: {op: add, count: 1, id: 1000, time: 2ms}
                add_match = re.search(r'\{op:\s*add,\s+count:\s+(\d+),\s+id:\s+(\d+),\s+time:\s+(\d+)ms\}', op_line)
                if add_match:
                    time = int(add_match.group(3))
                    add_ops.append(time)
                
                # Parse look operations: {op: look, count: 1, id: 1000, time: 3ms, failed: 0, timeout: 0}
                look_match = re.search(r'\{op:\s*look,\s+count:\s+(\d+),\s+id:\s+(\d+),\s+time:\s+(\d+)ms', op_line)
                if look_match:
                    time = int(look_match.group(3))
                    look_ops.append(time)
                
                # Stop when we hit the next test configuration or end of operations
                if (re.search(r'\d+\s+Node\(s\),\s+\d+\s+Machine\(s\)', op_line) or 
                    re.search(r'-->\s+Probing\(\d+\)', op_line) and look_ops):
                    break
                
                j += 1
            
            if add_ops and look_ops:
                tests.append((nodes, machines, entries_per_machine, add_ops, look_ops))
                print(f"  Found: {nodes}N/{machines}M - {len(add_ops)} add ops, {len(look_ops)} look ops")
            
            i = j
        else:
            i += 1
    
    return tests

def calculate_averages(tests):
    """
    Calculate average latencies for each configuration.
    
    Returns:
        List of tuples: (nodes, machines, avg_add_ms, avg_look_ms)
    """
    results = []
    for nodes, machines, entries, add_ops, look_ops in tests:
        avg_add = sum(add_ops) / len(add_ops) if add_ops else 0
        avg_look = sum(look_ops) / len(look_ops) if look_ops else 0
        results.append((nodes, machines, avg_add, avg_look))
    
    return results

def generate_matrix_csv(data, metric_index, filename, metric_name):
    """
    Generate a CSV file in matrix format.
    
    Args:
        data: List of tuples (nodes, machines, avg_add, avg_look)
        metric_index: 2 for add latency, 3 for lookup latency
        filename: Output CSV filename
        metric_name: Name of the metric
    """
    # Build matrix
    matrix = defaultdict(dict)
    all_machines = set()
    all_nodes = set()
    
    for nodes, machines, add_lat, look_lat in data:
        all_nodes.add(nodes)
        all_machines.add(machines)
        matrix[nodes][machines] = [add_lat, look_lat][metric_index - 2]
    
    # Sort for consistent output
    node_counts = sorted(all_nodes)
    machine_counts = sorted(all_machines)
    
    # Write CSV
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        
        # Header
        header = ['Nodes/Machines'] + [f'{m}M' for m in machine_counts]
        writer.writerow(header)
        
        # Data rows
        for nodes in node_counts:
            row = [f'{nodes}N']
            for machines in machine_counts:
                value = matrix[nodes].get(machines, '')
                if value != '':
                    row.append(f'{value:.2f}')
                else:
                    row.append('')
            writer.writerow(row)
    
    print(f"Generated {filename}")

def generate_long_format_csv(data, filename):
    """
    Generate a CSV file in long format (one row per test).
    
    Args:
        data: List of tuples (nodes, machines, avg_add, avg_look)
        filename: Output CSV filename
    """
    with open(filename, 'w', newline='') as csvfile:
        writer = csv.writer(csvfile)
        writer.writerow(['Nodes', 'Machines', 'AvgAddLatency_ms', 'AvgLookupLatency_ms'])
        
        for nodes, machines, avg_add, avg_look in sorted(data):
            writer.writerow([nodes, machines, f'{avg_add:.2f}', f'{avg_look:.2f}'])
    
    print(f"Generated {filename}")

def main():
    # Read input
    if len(sys.argv) > 1:
        with open(sys.argv[1], 'r') as f:
            log_content = f.read()
    else:
        log_content = sys.stdin.read()
    
    if not log_content.strip():
        print("Error: No input data provided")
        sys.exit(1)
    
    # Parse log
    print("Parsing log data...")
    tests = parse_log(log_content)
    
    if not tests:
        print("\nError: No test configurations found in the log.")
        print("Expected format:")
        print("  --> 1 Node(s), 1 Machine(s), 1000 Entries")
        print("  {op: add, count: 1, id: 1000, time: 2ms}")
        print("  {op: look, count: 1, id: 1000, time: 3ms, failed: 0, timeout: 0}")
        sys.exit(1)
    
    print(f"\nFound {len(tests)} test configurations")
    
    # Calculate averages
    results = calculate_averages(tests)
    
    # Generate CSV files
    print("\nGenerating CSV files...")
    generate_matrix_csv(results, 2, 'add_latency_matrix.csv', 'Add Latency')
    generate_matrix_csv(results, 3, 'lookup_latency_matrix.csv', 'Lookup Latency')
    generate_long_format_csv(results, 'latency_data.csv')
    
    print("\nSummary:")
    print(f"  Configurations tested: {len(results)}")
    print(f"  Node counts: {sorted(set(r[0] for r in results))}")
    print(f"  Machine counts: {sorted(set(r[1] for r in results))}")
    print("\nOutput files:")
    print("  - add_latency_matrix.csv      (matrix format for heatmaps)")
    print("  - lookup_latency_matrix.csv   (matrix format for heatmaps)")
    print("  - latency_data.csv            (long format for analysis)")

if __name__ == '__main__':
    main()