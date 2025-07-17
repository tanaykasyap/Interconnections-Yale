import os
import re
import PyPDF2
import pdfplumber
import pandas as pd
import csv
import unicodedata

# Directory containing the PDFs
directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/"
os.chdir(directory)

# Patterns to identify Cluster 2 PDFs
cluster_2_patterns = [
    r"C[\s_-]*2", 
    r"c[\s_-]*2", 
    r"Cluster[\s_-]*2", 
    r"cluster[\s_-]*2", 
    r"C[\s_-]*II", 
    r"c[\s_-]*II", 
    r"Cluster[\s_-]*II", 
    r"cluster[\s_-]*II"
]

# Function to normalize and clean strings
def clean_string(text):
    return unicodedata.normalize('NFKD', text).encode('ascii', 'ignore').decode('ascii').replace('\n', ' ').lower().strip()

# Function to extract data from a PDF
def extract_pdf_data(pdf_path):
    with open(pdf_path, 'rb') as file:
        reader = PyPDF2.PdfReader(file)
        text = ""
        for page in reader.pages:
            text += page.extract_text()
    
    # Normalize text to handle unicode, spaces, and hyphens
    text = clean_string(text)

    # Extract Queue ID (without the "Q" prefix)
    queue_id = re.search(r"q[\s_-]*(\d+)", text, re)
    queue_id = queue_id.group(1) if queue_id else None
    
    # Extract Cluster Number, if not found, set it to 2
    cluster_number = re.search(r"queue[\s_-]*cluster[\s_-]*(\d+)", text)
    cluster_number = cluster_number.group(1) if cluster_number else "2"
    
    # Extract Deliverability Status
    deliverability_status = re.search(r"(\w+)\s*capacity deliverability status", text, re.IGNORECASE)
    deliverability_status = deliverability_status.group(1) if deliverability_status else None
    
    # Extract Plant Type, Fuel, and Generator Type
    plant_type = re.search(r"the project is a (\w+) (\w+) plant", text, re.IGNORECASE)
    if plant_type:
        fuel = plant_type.group(1)
        generator_type = plant_type.group(2)
    else:
        fuel = None
        generator_type = None
    
    # Extract Latitude and Longitude
    gps_coords = re.search(r"gps coordinates:\s*([\d\.\-]+),\s*([\d\.\-]+)", text)
    latitude, longitude = gps_coords.groups() if gps_coords else (None, None)
    
    # Extract Capacity
    capacity = re.search(r"total rated output of (\d+)\s*mw", text, re.IGNORECASE)
    if capacity:
        capacity = int(capacity.group(1))
    else:
        # Extract Capacity (first occurrence of number followed by MW)
        capacity2 = re.search(r"(\d+)\s*mw", text)
        capacity = int(capacity2.group(1)) if capacity2 else None

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
        "network_mcr": None
    }
    
    # Extract Tables from Section 10 using pdfplumber
    df_tables = extract_tables_with_pdfplumber(pdf_path, data)
    
    return df_tables

def extract_tables_with_pdfplumber(pdf_path, base_data):
    all_rows = []
    
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            text = page.extract_text()
            text = clean_string(text)  # Normalize the text
            
            # Look for tables starting with "Table 10"
            if re.search(r"table[\s_-]*10[\s_-]*[0-9]*", text):
                # Extract tables on the current page
                page_tables = page.extract_tables()
                for table in page_tables:
                    if table and len(table) > 1:
                        df = pd.DataFrame(table[1:], columns=table[0])  # assuming the first row is the header

                        # Clean the data: remove $ and % symbols and newline characters from column names
                        df.columns = df.columns.str.replace('\n', ' ')
                        df = df.replace({'\$': '', ',': '', '%': ''}, regex=True)

                        # Normalize and remove any weird characters
                        df = df.apply(lambda col: col.map(lambda x: unicodedata.normalize('NFKD', x).encode('ascii', 'ignore').decode('ascii') if isinstance(x, str) else x))

                        # Remove any newlines in the dataframe values
                        df = df.apply(lambda col: col.map(lambda x: str(x).replace('\n', ' ') if isinstance(x, str) else x))
                        
                        df.columns = df.columns.str.lower() 

                        # Forward fill the "Type of Upgrade" column to duplicate values across rows
                        if "type of upgrade" in df.columns:
                            df["type of upgrade"] = df["type of upgrade"].ffill()

                        # Check for the "Total" row and extract its value
                        total_row = df[df.apply(lambda row: row.str.contains('total', case=False).any(), axis=1)]
                        if not total_row.empty:
                            # Find the first numerical value in the total row
                            total_value = total_row.apply(lambda x: pd.to_numeric(x, errors='coerce')).dropna(axis=1).iloc[0]
                            if not total_value.empty:
                                base_data["network_mcr"] = total_value.iloc[0]  
                        # Drop the "Total" row from the dataframe
                            df = df.drop(total_row.index)
                        # Manually merge similar columns
                         # Merge similar or duplicate columns
                        df.columns = df.columns.str.lower()  # Convert all column names to lowercase
                       


                        # Drop the last column if it has no name or is a duplicate
                        if df.columns[-1] == '' or df.columns[-1] in df.columns[:-1]:
                            df.drop(columns=[df.columns[-1]], inplace=True)

                        # Duplicate base_data across all rows of this table
                        for _, row in df.iterrows():
                            row_data = {**base_data, "type of upgrade": row.get("type of upgrade", ""), **row.to_dict()}
                            all_rows.append(row_data)

    # Convert the list of all rows to a DataFrame
    df_final = pd.DataFrame(all_rows)

    # Make all column names lowercase
    df_final.columns = df_final.columns.str.lower()

    

    # Sort the DataFrame by q_id
    df_final = df_final.sort_values(by="q_id", ascending=True)

    return df_final

def process_pdfs_in_folder(directory, output_csv_path):
    combined_df = pd.DataFrame()  # Initialize an empty DataFrame to combine results
    pdf_count = 0
    scraped_count = 0

    # Iterate over all PDF files in the directory and subdirectories
    for root, dirs, files in os.walk(directory):
        for pdf_name in files:
            if pdf_name.endswith(".pdf"):
                pdf_path = os.path.join(root, pdf_name)

                # Check if the file is in cluster 2 by matching against the patterns
                if any(re.search(pattern, pdf_name, re.IGNORECASE) for pattern in cluster_2_patterns):
                    pdf_count += 1
                    print(f"Processing {pdf_name}")
                    
                    try:
                        # Extract and process the data if the required table and columns are present
                        df = extract_pdf_data(pdf_path)
                        if not df.empty:
                            # Append the extracted data to the combined DataFrame
                            combined_df = pd.concat([combined_df, df], ignore_index=True)
                            scraped_count += 1
                            print(f"Scraped data from {pdf_name}")
                    
                    except Exception as e:
                        print(f"Skipping {pdf_name} due to an error: {e}")
    # Merge similar or duplicate columns
    if not combined_df.empty:
        merge_columns = {
            "cost allocation factor": ["cost allocation", "cost allocation factor"],
            "estimated cost x 1000": ["estimated cost x 1000", "estimated cost x 1,000"],
            "estimated time to construct": ["estimated time to construct (note 1)", "estimated time to construct"]
        }
        
        for new_col, old_cols in merge_columns.items():
            if any(col in combined_df.columns for col in old_cols):
                combined_df[new_col] = combined_df[old_cols].bfill(axis=1).iloc[:, 0]
                combined_df.drop(columns=[col for col in old_cols if col in combined_df.columns and col != new_col], inplace=True)

    # Save the combined DataFrame to a CSV file
    combined_df.to_csv(output_csv_path, index=False, encoding='utf-8', sep=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

    print(f"Total PDFs accessed: {pdf_count}")
    print(f"Total PDFs successfully scraped: {scraped_count}")

# Usage
output_csv_path = '/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/combined_cluster_2_data.csv'  # Replace with your desired output path
process_pdfs_in_folder(directory, output_csv_path)
print(f"Data extracted and saved to {output_csv_path}")
