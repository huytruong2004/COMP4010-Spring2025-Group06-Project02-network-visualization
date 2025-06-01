import pandas as pd
from collections import Counter

# Read the CSV file
df = pd.read_csv('output.csv')

# 1. Count unique destination ports
unique_ports = df['destination_port'].nunique()
print(f"1. Total unique destination ports: {unique_ports}")

# 2. Count unique source IPs
unique_ips = df['source_ip'].nunique()
print(f"2. Total unique source IPs: {unique_ips}")

# 3. Find top 10 ports by frequency
port_counts = df['destination_port'].value_counts().head(10)
print("\n3. Top 10 ports by frequency:")
for i, (port, count) in enumerate(port_counts.items(), 1):
    print(f"   {i}. Port {port}: {count} occurrences")