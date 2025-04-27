import pyshark 
import time 
import geoip2.database


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
        
    cap = pyshark.FileCapture(pcap_file)
    packets = []
    import IPython
    IPython.embed()
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
    
    return packets

def dict_to_csv(packets, csv_file):
    with open(csv_file, 'w') as f:
        f.write("timestamp,source_ip,source_country,destination_port,protocol,length\n")
        for packet in packets:
            f.write(f"{packet['timestamp']},{packet['source_ip']},{packet['source_country']},{packet['destination_port']},{packet['protocol']},{packet['length']}\n")
            
def main(pcap_file, csv_file):
    start_time = time.time()
    packets = pcap_to_dict(pcap_file)
    dict_to_csv(packets, csv_file)
    end_time = time.time()
    print(f"Conversion completed in {end_time - start_time:.2f} seconds.")
    
if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description='Convert pcap file to CSV.')
    parser.add_argument('pcap_file', type=str, help='Path to the pcap file.')
    parser.add_argument('csv_file', type=str, help='Path to the output CSV file.')
    args = parser.parse_args()
    
    main(args.pcap_file, args.csv_file)