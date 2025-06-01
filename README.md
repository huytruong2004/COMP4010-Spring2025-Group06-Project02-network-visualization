# COMP4010 Project 2 - Virtual Private Cloud Network Visualization 

This project showcases the ability of R to do active & network monitoring in virtual private server (VPS). 

## Members

+ Nguyen Tuan Anh
+ Tran Tat Hung
+ Truong Dang Gia Huy

## Introduction 

With the blooming of cloud services around the world, Virtual private server (VPS) are increasingly popular among developers or whoever interested in self-hosting something that help & comfort their daily digital lifes. Personal websites, game server, and even self-hosted virtual private network (VPN) are all hosted in personal cloud instance. This makes the life of digital nomads, developers, and even start-ups around the world much more simpler by not necessarily having an expensive server rack in their home or office. 

However, virtual private cloud is facing a crutial and also sadly fact: they are very rewardful targets to cybercriminal. With the VPS's public IP, which is easy to get from DNS records of the cloud services, attackers can easily scan for opening services by tools like [nmap](https://nmap.org/), and even deploying automatic exploits. For example, just opening a simple HTTP server, within 5 minutes, we got "visit" from a friend in... Lithuania? 

![](./attachments/scan.png)

From these actively running vulnerability scanners, cyber attacks could deploy thousands forms of attacks:

- Ransomware
- Cryptominer
- Botnet for future Distributed Denial-of-service attacks
- ... many many more :(

One of our teammate faced an [incident](https://h114mx001.netlify.app/posts/how-we-got-hacked-while-ctfing/) before, where his team got hacked by cybercriminal, got a cryptominer run intensively in their clusters, just because they did not secure their network, with assumption in mind is **only they can access the VPS**. 

From this inspiration, we selected this project: using open-source network monitoring tool [Wireshark](https://www.wireshark.org/), combining with the power of R in informative visualization, to provide a dashboard of active threats against VPS, from evil outsiders.

## Objectives

1. Demonstrating comprehensive network security visualization using `shiny` and advanced R visualization libraries.
    - **Threat monitoring**: Interactive dashboard overview with key security metrics, attack trends, and top attackers identification
    - **Temporal analysis**: Dynamic time-series charts showing traffic patterns across multiple time intervals (minute, hour, day) with configurable metrics (attack count, data volume, unique IPs, threat scores)
    - **Geographic threat intelligence**: Interactive world map choropleth visualization displaying attack origins by country with switchable metrics and detailed country statistics
    - **Network topology analysis**: Advanced network graph showing IP-to-port attack relationships with intelligent filtering by threat level, node type, and geographic origin

2. Raising the awareness of people on the current threat scenario, as well as putting security of the virtual private cloud in mind.

## Data Gathering & Setup 

We will use our teammate's virtual private server as the data source for this project. The design of the data retrieving system can be explained with this figure: 

![](./attachments/diagrams/network_traffic_analysis_system.png)

+ We will use Wireshark, an open-source network monitoring tool to monitor our network. 
+ Next, we will export the inbound traffic of our virtual private cloud into JSON. This will be the input for the Python data cleaner ultilities to clean the data & enrich the information about location of the income IP address. 
+ These information will then be written in a Redis key-value database store, for the FastAPI server to query.
+ Our FastAPI backend server will exposes the APIs for client R code.

Also, because this project is aimed to show how "reachable" our virtual private server to the attackers server, a list of IPs which we are using **will be excluded in the data gathering**.

## Data Dictionary 

For the visualization of the R code, we will use the following table schema, where each row is a single network packet. 

|variable                  |class     |description                           |
|:-------------------------|:---------|:-------------------------------------|
|timestamp                 |integer   |UNIX timestamp of the trafifc |
|source_ip                 |string    |IPv4 address of Source |
|source_country            |string    |The country where source_ip comes from, based on geolocation |
|destination_port          |integer   |The destination port that `source_ip` connects to. |
|protocol                  |string    |Protocol used by the packet | 
|length                    |integer   |Length in bytes of the packet |

The example of upcoming CSV is in [here](./src/parser/output.csv).

## Plan

### Infrastructure Setup

- [x] Set up Wireshark on the VPS for traffic capture
- [x] Set up Redis database to store processed data

### Visualization

- [x] Build comprehensive security dashboard with 4 main analysis views
- [x] Implement interactive timeline analysis with dygraphs
- [x] Create network graph visualization with advanced filtering
- [x] Build geographic analysis with interactive world map
- [x] Add threat scoring system and metrics
- [x] Implement advanced filtering and search capabilities

## Visualization Setup Guide

### Prerequisites

- **R** (version 4.0 or higher) - [Download from CRAN](https://cran.r-project.org/)
- **RStudio** (recommended) - [Download from Posit](https://posit.co/downloads/)

### Quick Start

1. **Navigate to the visualization directory:**
   ```bash
   cd src/network-security-viz
   ```

2. **Install required R packages:**
   ```bash
   Rscript install_packages.R
   ```
   
   This will automatically install all required packages including:
   - `shiny`, `shinydashboard` - Dashboard framework
   - `data.table`, `dplyr`, `ggplot2`, `scales` - Data manipulation & visualization
   - `visNetwork` - Interactive network graphs showing IP-to-port attack relationships
   - `dygraphs` - Time-series charts with zoom/pan for timeline analysis
   - `plotly` - Interactive world map choropleth for geographic analysis
   - `DT`, `viridis` - Tables and color palettes

3. **Run the application:**
   ```bash
   Rscript run_app.R
   ```
   
   The dashboard will start at `http://127.0.0.1:8080` and automatically open in your browser.

### Alternative Setup (Using RStudio)

1. Open RStudio
2. Set working directory to `src/network-security-viz`
3. Run the installation script:
   ```r
   source("install_packages.R")
   ```
4. Launch the app:
   ```r
   shiny::runApp("app.R")
   ```

### Dashboard Features

The visualization dashboard provides four comprehensive analysis views:

- **Dashboard Overview**: Metrics with 6 key performance indicators (total attacks, unique attackers, threat levels, top countries/ports, data volume), recent attack trends chart, and top attackers table
- **Timeline Analysis**: Interactive time-series visualization using dygraphs with configurable metrics (attack count, data volume, unique IPs, threat scores), time intervals (minute/hour/day), protocol filtering, and threat level thresholds
- **Network Graph**: Advanced network visualization using visNetwork showing IP-to-port attack patterns, with filtering by node type, threat level, country, and smart search functionality
- **Geographic Analysis**: Interactive world map using plotly choropleth displaying attack distribution by country, with metric switching and detailed country statistics table

Additional features include:
- Global time range filtering (24 hours, 7 days, 30 days, all time)
- Advanced threat scoring system (0-10 scale based on target ports, packet sizes, and protocols)
- Data loading with error handling and sample data fallback
- Enhanced security-themed dark UI with custom styling

### Data Requirements

The dashboard expects a CSV file with network traffic data following this schema:
- `timestamp`: UNIX timestamp
- `source_ip`: IPv4 address 
- `source_country`: Country of origin
- `destination_port`: Target port
- `protocol`: Network protocol
- `length`: Packet size in bytes

Data is available at `src/parser/output.csv`.

## Presentation and Report
