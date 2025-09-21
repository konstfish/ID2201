#!/usr/bin/env python3
import glob
import csv

# Find all CSV files in current directory
csv_files = glob.glob("*.csv")

for filename in csv_files:
    print(f"Sorting {filename}...")

    # Read all rows
    rows = []
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        for row in reader:
            if row and len(row) > 0:  # Skip empty rows
                rows.append(row)

    # Sort by first column (convert to int for proper numeric sorting)
    rows.sort(key=lambda x: int(x[0]) if x[0].isdigit() else 0)

    # Write back to file
    with open(filename, 'w', newline='') as f:
        writer = csv.writer(f)
        writer.writerows(rows)

    print(f"✓ {filename} sorted by first column")

print("All CSV files have been sorted by their first column!")