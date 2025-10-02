#!/usr/bin/env python3
"""
Convert DHT benchmark output to CSV format for gnuplot visualization.
Extracts timing data from add and lookup operations.
"""

import re
import sys
from datetime import datetime

def parse_benchmark_file(input_file, output_prefix):
    """Parse benchmark data and write to CSV files."""
    
    # Separate data by configuration
    data_1node = []
    data_2node = []
    data_8node = []
    data_lookup = []
    
    # Track current configuration
    current_nodes = 0
    current_machines = 0
    current_entries = 0
    
    request_num = 0
    
    with open(input_file, 'r') as f:
        for line in f:
            line = line.strip()
            
            # Parse configuration lines
            config_match = re.match(r'--> (\d+) Node\(s\), (\d+) Machine\(s\), (\d+) Entries', line)
            if config_match:
                current_nodes = int(config_match.group(1))
                current_machines = int(config_match.group(2))
                current_entries = int(config_match.group(3))
                continue
            
            # Parse operation lines: {op: add, count: 1, id: 4000, time: 27ms}
            op_match = re.match(r'\{op: (add|look), count: \d+, id: \d+, time: (\d+)ms', line)
            if op_match:
                operation = op_match.group(1)
                delay_ms = int(op_match.group(2))
                
                request_num += 1
                row = f"{request_num},{delay_ms}\n"
                
                if operation == "add":
                    if current_nodes == 1:
                        data_1node.append(row)
                    elif current_nodes == 2:
                        data_2node.append(row)
                    elif current_nodes == 8:
                        data_8node.append(row)
                else:  # lookup
                    data_lookup.append(row)
    
    # Write separate files
    files_written = []
    
    if data_1node:
        filename = f"{output_prefix}-1node.csv"
        with open(filename, 'w') as f:
            f.write("request,delay_ms\n")
            f.writelines(data_1node)
        files_written.append(filename)
        print(f"Created {filename} with {len(data_1node)} points")
    
    if data_2node:
        filename = f"{output_prefix}-2node.csv"
        with open(filename, 'w') as f:
            f.write("request,delay_ms\n")
            f.writelines(data_2node)
        files_written.append(filename)
        print(f"Created {filename} with {len(data_2node)} points")
    
    if data_8node:
        filename = f"{output_prefix}-8node.csv"
        with open(filename, 'w') as f:
            f.write("request,delay_ms\n")
            f.writelines(data_8node)
        files_written.append(filename)
        print(f"Created {filename} with {len(data_8node)} points")
    
    if data_lookup:
        filename = f"{output_prefix}-lookup.csv"
        with open(filename, 'w') as f:
            f.write("request,delay_ms\n")
            f.writelines(data_lookup)
        files_written.append(filename)
        print(f"Created {filename} with {len(data_lookup)} points")
    
    return files_written

def main():
    if len(sys.argv) < 2:
        print("Usage: python dht_to_csv.py <input_file> [output_prefix]")
        print("Example: python dht_to_csv.py benchmark.txt requests")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_prefix = sys.argv[2] if len(sys.argv) > 2 else "requests"
    
    try:
        parse_benchmark_file(input_file, output_prefix)
    except FileNotFoundError:
        print(f"Error: Input file '{input_file}' not found")
        sys.exit(1)
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)

if __name__ == "__main__":
    main()