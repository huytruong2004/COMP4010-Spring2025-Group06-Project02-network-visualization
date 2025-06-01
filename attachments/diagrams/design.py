from diagrams import Cluster, Diagram, Edge, Node
from diagrams.onprem.network import Internet
from diagrams.onprem.compute import Server
from diagrams.onprem.inmemory import Redis
from diagrams.programming.language import Python, R
from diagrams.custom import Custom

with Diagram("Network Traffic Analysis System", show=False):
    with Cluster("Virtual Private Server"):
        # Network monitoring components
        wireshark = Custom("Wireshark", "./resources/wireshark.png")
        network = Internet("Internet Traffic")
        
        # Data processing components
        python_cleaner = Python("Data Cleaner")
        # redis_cache = Redis("Redis Cache")
        # fastapi = Custom("FastAPI", "./resources/fastapi.png")
        csv_files = Custom("CSV Files", "./resources/csv.png")
        
    # External visualization component
    r_vis = R("R Visualization")
    
    # Define the flow
    network << Edge(label="Monitor") << wireshark
    wireshark >> Edge(label="Export .pcap files") >> python_cleaner
    python_cleaner >> Edge(label="Clean Data") >> csv_files
    csv_files >> Edge(label="Load CSV") >> r_vis