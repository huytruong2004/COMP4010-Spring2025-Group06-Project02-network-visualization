import requests 

import pandas as pd 

df = pd.read_csv("output.csv")

# get list of "source_country" that is blank, show the unique IP
df_blank = df[df["source_country"].isnull()]
df_blank = df_blank[["source_ip", "source_country"]].drop_duplicates()
print("List of unique IPs with blank source_country:")
print(df_blank["source_ip"].unique())

def fill_ip_with_country(ip, country_name):
    """
    Fill the IP with the country name manually.
    """
    df.loc[df["source_ip"] == ip, "source_country"] = country_name
    print(f"Filled {ip} with {country_name}")
    
import IPython; IPython.embed()