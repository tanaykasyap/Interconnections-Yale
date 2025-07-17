import os
import re
import PyPDF2
import pdfplumber
import pandas as pd
import csv
import unicodedata
#import warnings

# Directory containing the PDFs
directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/"
os.chdir(directory)

# Mapping Roman numerals to integers
roman_to_int = {
    "I": 1,
    "II": 2,
    "III": 3,
    "IV": 4,
}

def extract_pdf_data(pdf_path):
    with open(pdf_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        text = ""
        for page in reader.pages:
            text += page.extract_text()

    # Extract Queue ID (without the "Q" prefix)
    queue_id = re.search(r" Q(\d+)", text)
    queue_id = queue_id.group(1) if queue_id else None
    
    # Extract Cluster Number
    cluster_number = re.search(r"Queue Cluster (\d+)", text)
    cluster_number = cluster_number.group(1) if cluster_number else None
    
    # Extract Phase (convert Roman numeral to integer)
    phase_match = re.search(r"Phase (\w+) Study", text)
    phase = roman_to_int.get(phase_match.group(1), None) if phase_match else None
    
    # Extract Deliverability Status
    deliverability_status = re.search(r"(\w+)\s*Capacity Deliverability Status", text)
    deliverability_status = deliverability_status.group(1) if deliverability_status else None
    
    # Extract Plant Type, Fuel, and Generator Type
    plant_type = re.search(r"The Project is a (\w+) (\w+) plant", text)
    if plant_type:
        fuel = plant_type.group(1)
        generator_type = plant_type.group(2)
    else:
        fuel = None
        generator_type = None
    
    # Extract Latitude and Longitude
    gps_coords = re.search(r"GPS Coordinates:\s*([\d\.\-]+),\s*([\d\.\-]+)", text)
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
        "network_mcr": network_mcr  # Add Network_MCR to the data
    }
    
    # Extract Tables using pdfplumber
    df_tables = extract_tables_with_pdfplumber(pdf_path, data)
    
    return df_tables

def extract_tables_with_pdfplumber(pdf_path, base_data):
    all_rows = []
    
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            tables = page.extract_tables()
            for table in tables:
                df = pd.DataFrame(table[1:], columns=table[0])  # assuming the first row is the header

                # Clean the data: ensure all column names are strings and remove unwanted characters
                df.columns = df.columns.astype(str)
                df = df.map(lambda x: str(x) if pd.notnull(x) else "")
                df.columns = df.columns.str.replace('\n', ' ')
                df = df.replace({'\$': '', ',': '', '%': ''}, regex=True)

                # Check if the table has the required columns
                required_columns = {"Type of Upgrade", "Upgrade", "Description"}
                if required_columns.issubset(df.columns):
                    # Normalize and remove any weird characters
                    df = df.map(lambda x: unicodedata.normalize('NFKD', x).encode('ascii', 'ignore').decode('ascii') if isinstance(x, str) else x)

                    # Remove any newlines in the dataframe values
                    df = df.map(lambda x: str(x).replace('\n', ' ') if isinstance(x, str) else x)

                    # Forward fill the "Type of Upgrade" column to duplicate values across rows
                    df["Type of Upgrade"] = df["Type of Upgrade"].ffill()

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
                        row_data = {**base_data, "Type of Upgrade": row.get("Type of Upgrade", ""), **row.to_dict()}
                        all_rows.append(row_data)

    # Convert the list of all rows to a DataFrame
    df_final = pd.DataFrame(all_rows)

    # Make all column names lowercase
    df_final.columns = df_final.columns.str.lower()

    return df_final

def process_pdfs_in_folder(directory, output_csv_path):
    combined_df = pd.DataFrame()  # Initialize an empty DataFrame to combine results

    # Iterate over all PDF files in the directory
    for pdf_name in os.listdir(directory):
        if pdf_name.endswith(".pdf"):
            pdf_path = os.path.join(directory, pdf_name)
            print(f"Processing {pdf_name}")
            
            try:
                # Extract and process the data if the required table and columns are present
                df = extract_pdf_data(pdf_path)
                if not df.empty:
                    # Append the extracted data to the combined DataFrame
                    combined_df = pd.concat([combined_df, df], ignore_index=True)
                    print(f"Scraped data from {pdf_name}")
            
            except Exception as e:
                print(f"Skipping {pdf_name} due to an error: {e}")
    
    # Sort the combined DataFrame by 'q_id' in ascending order
    combined_df = combined_df.sort_values(by="q_id", ascending=True)

    # Save the sorted DataFrame to a CSV file
    combined_df.to_csv(output_csv_path, index=False, encoding='utf-8', sep=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

# Usage
output_csv_path = 'combined_data.csv'  # Replace with your desired output path
process_pdfs_in_folder(directory, output_csv_path)
print(f"Data extracted, sorted by q_id, and saved to {output_csv_path}")
 