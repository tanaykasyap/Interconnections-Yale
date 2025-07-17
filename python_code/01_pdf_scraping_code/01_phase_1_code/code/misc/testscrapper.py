 # this code is an example pdf scrapper for the following project
#C7PhI Appendix A - Q1012 Honeybee.pdf


import os
import re
import PyPDF2
import pdfplumber
import pandas as pd
import csv
import unicodedata

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/"
os.chdir(new_directory)

# Mapping Roman numerals to integers
roman_to_int = {
    "I": 1,
    "II": 2,
    "III": 3,
    "IV": 4,
    

}


 


def extract_pdf_data(pdf_path):
    # Open the PDF file using PyPDF2 for text extraction
    with open(pdf_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        text = ""
        for page in reader.pages:
            text += page.extract_text()
    
    # Extract Queue ID (without the "Q" prefix)
    # d means digits, d+ means one or more digits 
    # creating capture groups, group(0) whole string i.e Q1012, group(1) only digits i.e 1012
    queue_id = re.search(r" Q(\d+)", text)
    queue_id = queue_id.group(1) if queue_id else None
    
    # Extract Cluster Number
    cluster_number = re.search(r"Queue Cluster (\d+)", text)
    cluster_number = cluster_number.group(1) if cluster_number else None
    
    # Extract Phase
    #phase = re.search(r"Phase (\w+) Study", text)
    #phase = phase.group(1) if phase else None
    # Extract Phase (convert Roman numeral to integer)
    phase_match = re.search(r"Phase (\w+) Study", text)
    phase = roman_to_int.get(phase_match.group(1), None) if phase_match else None
    
    # Extract Deliverability Status
    #s is for whitespace, * means zero or more including none, \w means word character
    deliverability_status = re.search(r"(\w+)\s*Capacity Deliverability Status", text)
    deliverability_status = deliverability_status.group(1) if deliverability_status else None
    
    # Extract Plant Type, Fuel, and Generator Type
    
    plant_type = re.search(r"(\w+)\s+Storage\s+plant", text)
    if plant_type:
        fuel = plant_type.group(1)
        generator_type = "Storage"
    else:
        fuel = None
        generator_type = None
    
    # Extract Latitude and Longitude
    # + is for one or more of the preceding element
    # ([\d\.\-]+) matches and captures any sequence of digits, periods, and minus signs, which together represent a coordinate
    gps_coords = re.search(r"GPS Coordinates:\s*([\d\.\-]+),\s*([\d\.\-]+)", text )
    latitude, longitude = gps_coords.groups() if gps_coords else (None, None)
    
    # Extract Capacity
    capacity = re.search(r"total rated output of (\d+)\s*MW", text)
    capacity = int(capacity.group(1)) if capacity else None

    # Extract Network Maximum Cost Responsibility
    network_mcr_match = re.search(r"The maximum cost responsibility for the Network Upgrades.*? \$(\d+\.\d+) Million", text, re.IGNORECASE)
    network_mcr = network_mcr_match.group(1) if network_mcr_match else None
    
    
    # Initialize the data dictionary
    data = {
        "q_id": queue_id,
        "cluster": cluster_number,
        "req_deliverability": deliverability_status,
        "latitude": latitude,
        "longitude": longitude,
        "generator_type": generator_type,
        "fuel": fuel,
        "capacity": capacity,
        "phase": phase,  # Add phase to the data
        "Network_MCR": network_mcr  # Add Network_MCR to the data
    }
    
    # Extract Tables from Section 7 using pdfplumber
    df_tables = extract_tables_with_pdfplumber(pdf_path, data)
    
    return df_tables

def extract_tables_with_pdfplumber(pdf_path, base_data):
    all_rows = []
    
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            text = page.extract_text()
            if "Table 7-" in text:
                # Extract tables on the current page
                page_tables = page.extract_tables()
                for table in page_tables:
                    df = pd.DataFrame(table[1:], columns=table[0])  # assuming the first row is the header

                    # Clean the data: remove $ and % symbols and newline characters from column names
                    df.columns = df.columns.str.replace('\n', ' ')
                    df = df.replace({'\$': '', ',': '', '%': ''}, regex=True)

                    # Normalize and remove any weird characters
                    df = df.apply(lambda col: col.map(lambda x: unicodedata.normalize('NFKD', x).encode('ascii', 'ignore').decode('ascii') if isinstance(x, str) else x))


                    # Remove any newlines in the dataframe values
                    df = df.apply(lambda col: col.map(lambda x: str(x).replace('\n', ' ') if isinstance(x, str) else x))


                     # Forward fill the "Type of Upgrade" column to duplicate values across rows
                    df["Type of Upgrade"] = df["Type of Upgrade"].ffill()

                    # Simplify "Type of Upgrade" names- SEEMS TO BE NOT WORKING
                    #df["Type of Upgrade"] = df["Type of Upgrade"].replace({
                     #   "PTO’s Interconnection Facilities (Note 2)": "IFS",
                      #  "Reliability Network Upgrade": "RNU",
                       # "Local Delivery Network Upgrades": "LDNU",
                        #"Area Delivery Network Upgrades": "ADNU"
                    #})

                


                    # Handle the "Total" row based on "Cost Allocation Factor" column
                    if "Cost Allocation Factor" in df.columns:
                        if "Total" in df["Cost Allocation Factor"].values:
                            total_row = df[df["Cost Allocation Factor"] == "Total"]
                            estimated_total_cost = total_row.iloc[0, -3]  # estimated total costs column
                            escalated_total_cost = total_row.iloc[0, -2]  # escalated total costs column
                            df = df[df["Cost Allocation Factor"] != "Total"]

                            # Assign the total costs to each row corresponding to this table
                            df["estimated_total_costs"] = estimated_total_cost
                            df["escalated_total_costs"] = escalated_total_cost

                    # Duplicate base_data across all rows of this table
                    for _, row in df.iterrows():
                        row_data = {**base_data, "Type of Upgrade": row["Type of Upgrade"], **row.to_dict()}
                        all_rows.append(row_data)

    # Convert the list of all rows to a DataFrame
    df_final = pd.DataFrame(all_rows)

    # Make all column names lowercase
    df_final.columns = df_final.columns.str.lower()


    return df_final


 

def save_to_csv(df, output_path):
    # Save the DataFrame to a CSV file with UTF-8 encoding and semicolon delimiter
    df.to_csv(output_path, index=False, encoding='utf-8', sep=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)


def save_to_html(df, output_path):
    # Save the DataFrame to an HTML file
    df.to_html(output_path, index=False)    

 

# Usage
pdf_path = 'matched_pdfs_phase_i/C7PhI Appendix A - Q1012 Honeybee.pdf'  # Replace with your PDF file path
csv_output_path = '1012_data.csv'  # Replace with your desired output path
html_output_path = '1012_data.html'
 
# Extract data
df = extract_pdf_data(pdf_path)

# Save data to CSV
save_to_csv(df, csv_output_path)
save_to_html(df, html_output_path)

 

print(f"Data extracted and saved to {csv_output_path} and {html_output_path}")



 



 

