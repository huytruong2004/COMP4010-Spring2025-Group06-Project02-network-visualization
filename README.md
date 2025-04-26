# COMP4010 Project 2 - Virtual Private Cloud Network Visualization 

This project showcases the ability of R to do active & real-time network monitoring in virtual private server (VPS). 

## Members

+ Nguyen Tuan Anh
+ Tran Tat Hung
+ Truong Dang Gia Huy

## Introduction 

With the blooming of cloud services around the world, Virtual private server (VPS) are increasingly popular among developers or whoever interested in self-hosting something that help & comfort their daily digital lifes. Personal websites, game server, and even self-hosted virtual private network (VPN) are all hosted in personal cloud instance. This makes the life of digital nomads, developers, and even start-ups around the world much more simpler by not necessarily having an expensive server rack in their home or office. 

However, virtual private cloud is facing a crutial and also sadly fact: they are very rewardful targets to cybercriminal. With the VPS's public IP, which is easy to get from DNS records of the cloud services, attackers can easily scan for opening services by tools like [nmap](https://nmap.org/), and even deploying automatic exploits. For example, just opening a simple HTTP server in Python, we got "visit" from a friend in... Lithuania? 

![](./attachments/scan.png)


One of our teammate faced an [incident](https://h114mx001.netlify.app/posts/how-we-got-hacked-while-ctfing/) before, where his team got hacked by cybercriminal, got a cryptominer run intensively in their clusters, just because they did not secure their network, with assumption in mind is **only they can access the VPS**. From this inspiration, we selected this project: using open-source network monitoring tool [Wireshark](https://www.wireshark.org/), combining with the power of R in informative visualization, to provide a dashboard of active threats against VPS, from evil outsiders.

The target of this project is:
1. Showing a proof-of-concept on how visualization could help in network monitoring. 
2. Raising the awareness of people on the current threat scenario, as well as putting security of the virtual private cloud in mind.

## Data Gathering & Setup 

We will use our teammate's virtual private server as the data source for this project. The design of the data retrieving system can be explained with this figure: 

![](./attachments/diagrams/network_traffic_analysis_system.png)

+ We will use Wireshark, an open-source network monitoring tool to monitor our network. 
+ Next, we will export the inbound traffic of our virtual private cloud into JSON. This will be the input for the Python data cleaner ultilities to clean the data & enrich the information about location of the income IP address. 
+ These information will then be written in a Redis key-value database store, for the FastAPI server to query.
+ Our FastAPI backend server will exposes the APIs for client R code.

## Data Dictionary 

For the visualization of the R code, we will use the following table schema, where each row is a single network packet. 

|variable                  |class     |description                           |
|:-------------------------|:---------|:-------------------------------------|
|timestamp                 |integer   |UNIX timestamp of the trafifc |
|source_ip                 |string    |IPv4 address of Source |
|source_country            |string    |The country where source_ip comes from, based on geolocation |
|protocol                  |string    |Protocol used by the packet | 
|length                    |integer   |Length in bytes of the packet |
|information               |string    |First 100 bytes of the information of the packet |


## Weekly plan 

