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
        redis_cache = Redis("Redis Cache")
        fastapi = Custom("FastAPI", "./resources/fastapi.png")
        
    # External visualization component
    r_vis = R("R Visualization")
    
    # Define the flow
    network << Edge(label="Monitor") << wireshark
    wireshark >> Edge(label="Export JSON") >> python_cleaner
    python_cleaner >> Edge(label="Clean Data") >> redis_cache
    redis_cache >> Edge(label="Query Cache") >> fastapi
    fastapi >> Edge(label="Fetch Data") >> r_vis