#!/usr/bin/env python3
import glob
import csv

# Find all CSV files in current directory
csv_files = glob.glob("*.csv")

# Read first row from each file and store with filename
file_data = []
for filename in csv_files:
    with open(filename, 'r') as f:
        reader = csv.reader(f)
        first_row = next(reader)
        # Convert first column to int for proper sorting
        first_col_value = int(first_row[0])
        file_data.append((first_col_value, filename, first_row))

# Sort by first column value
file_data.sort(key=lambda x: x[0])

# Print results
print("CSV files ordered by first column:")
for value, filename, row in file_data:
    print(f"{filename}: {','.join(row)}")