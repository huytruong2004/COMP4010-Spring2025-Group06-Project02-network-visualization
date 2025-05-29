import pyshark 
import time 
import geoip2.database
import os
import glob
import pandas as pd


def pcap_to_dict(pcap_file) -> dict:
    georeader = geoip2.database.Reader("./GeoLite2-Country.mmdb")
    
    def get_country(ip):
        try:
            response = georeader.country(ip)
            return response.country.name
        except geoip2.errors.AddressNotFoundError:
            return "Unknown"
    
    def get_dst_port(packet):
        if hasattr(packet, 'tcp'):
            return packet.tcp.dstport
        elif hasattr(packet, 'udp'):
            return packet.udp.dstport
        else:
            return 65537
        
    packets = []
    try:
        # Open capture file
        cap = pyshark.FileCapture(pcap_file)
        
        # Process packets
        for packet in cap: 
            if hasattr(packet, 'ip'):
                packets.append({
                    'timestamp': int(packet.sniff_time.timestamp()),
                    'source_ip': packet.ip.src,
                    'source_country': get_country(packet.ip.src),
                    'destination_port': get_dst_port(packet),
                    'protocol': packet.highest_layer,
                    'length': int(packet.length),
                })
                
        # Properly close capture
        cap.close()
        
    except Exception as e:
        print(f"Error processing {pcap_file}: {str(e)}")
        raise
        
    finally:
        # Ensure capture is closed even if error occurs
        if 'cap' in locals():
            cap.close()
            
    return packets

def dict_to_csv(packets, csv_file):
    with open(csv_file, 'w') as f:
        f.write("timestamp,source_ip,source_country,destination_port,protocol,length\n")
        for packet in packets:
            f.write(f"{packet['timestamp']},{packet['source_ip']},{packet['source_country']},{packet['destination_port']},{packet['protocol']},{packet['length']}\n")
            
def process_directory(pcap_dir: str, output_csv: str):
    """Process all pcap files in directory and combine into single CSV"""
    all_packets = []
    pcap_files = glob.glob(os.path.join(pcap_dir, "*.pcap"))
    
    header = not os.path.exists(output_csv)
    
    if not pcap_files:
        print(f"No pcap files found in {pcap_dir}")
        return
    
    total_start = time.time()
    
    for pcap_file in pcap_files:
        print(f"Processing {pcap_file}...")
        start_time = time.time()
        packets = pcap_to_dict(pcap_file)
        all_packets.extend(packets)
        end_time = time.time()
        print(f"- Completed in {end_time - start_time:.2f} seconds")
        print(f"- Found {len(packets)} packets")
    
    # Convert to DataFrame and sort by timestamp
    df = pd.DataFrame(all_packets)
    df = df.sort_values('timestamp')
    
    # Save combined CSV
    df.to_csv(output_csv, mode='a', index=False, header=header)
    
    total_end = time.time()
    print(f"\nAll processing completed in {total_end - total_start:.2f} seconds")
    print(f"Total packets processed: {len(all_packets)}")
    print(f"Output saved to: {output_csv}")
    time.sleep(2)  # Just to ensure the output is readable before exit

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Convert pcap files to single CSV.')
    parser.add_argument('pcap_dir', type=str, 
                       help='Directory containing pcap files')
    parser.add_argument('output_csv', type=str, 
                       help='Path to the output combined CSV file')
    args = parser.parse_args()
    
    process_directory(args.pcap_dir, args.output_csv)