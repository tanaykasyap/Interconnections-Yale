import os
import re
import PyPDF2
import pdfplumber
import pandas as pd
import csv
import unicodedata
from pprint import pprint
from tabula import read_pdf
import tabulate

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/"
os.chdir(new_directory)

reader = PyPDF2.PdfReader("/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf")
'''  
print(len(reader.pages))
#page=reader.pages[14]
#print(page.extract_text())

for i in range(13, 15):
    page=reader.pages[i]
    print(page.extract_text())


with pdfplumber.open("/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf") as f:
    for i in f.pages:
         
        pprint(i.extract_tables())



table_settings={
    "vertical_strategy": "text",
    "horizontal_strategy": "text",
}

pdf=pdfplumber.open("/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf")

table= pdf.pages[14].extract_table(table_settings)
df= pd.DataFrame(table[1::], columns=table[0])
pprint('df:', df)

'''



 




 
 
 

 
 
'''
import pdfplumber
import csv

def extract_tables_with_headers(pdf_path, output_csv_path):
    with pdfplumber.open(pdf_path) as pdf:
        full_table_data = []
        table_started = False

        for page_number, page in enumerate(pdf.pages):
            # Extract tables from the page
            tables = page.extract_tables(table_settings={ #"snap_y_tolerance":5, "text_y_tolerance": 1,  "join_y_tolerance": 5,
    "horizontal_strategy": "lines", "vertical_strategy": "lines", "snap_y_tolerance": 10})

            for table in tables:
                if table:  # Ensure the table is not empty
                    # Check if this is the first part of a new table
                    if not table_started:
                        full_table_data.extend(table)  # Use the first row as the header
                        table_started = True  # Mark the start of a table
                    else:
                        # Append rows from continuation pages, excluding the header row
                        full_table_data.extend(table[1:])  # Skip header in continuation pages

        # Now save the combined table data to a single CSV file
        save_table_to_csv(full_table_data, output_csv_path)


     

def save_table_to_csv(table_data, output_csv_path):
    # Save the extracted table data into a CSV file
    with open(output_csv_path, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerows(table_data)

# Example usage:
pdf_path = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf"
output_csv_path = "combined_table_output.csv"
extract_tables_with_headers(pdf_path, output_csv_path)

print(f"Saved combined table as {output_csv_path}")

'''

'''
import pdfplumber
import csv

def extract_table7_tables(pdf_path, output_csv_path):
    with pdfplumber.open(pdf_path) as pdf:
        full_table_data = []
        table_started = False

        for page_number, page in enumerate(pdf.pages):
            # Extract all text from the page to check for "Table 7-"
            page_text = page.extract_text()

            # Only process tables if "Table 7-" is found in the page's text
            if "Table 7-" in page_text:
                print(f"Processing tables on page {page_number + 1} with 'Table 7-'...")

                # Extract tables using lines strategy for accurate table dimensions
                tables = page.extract_tables(table_settings={
                    "horizontal_strategy": "lines",  # Use lines for row boundaries
                    "vertical_strategy": "lines",  # Use lines for column boundaries
                })

                for table in tables:
                    if table:
                        # Only process tables once the Table 7- condition is met
                        if not table_started:
                            full_table_data.extend(table)  # Use the first row as the header
                            table_started = True  # Mark the start of a table
                        else:
                            # Append rows from continuation pages, excluding the header row
                            full_table_data.extend(table[1:])  # Skip header in continuation pages
            else:
                print(f"Skipping page {page_number + 1} (no 'Table 7-' found)...")

        # Now save the combined table data to a single CSV file
        if full_table_data:
            save_table_to_csv(full_table_data, output_csv_path)
            print(f"Saved combined 'Table 7-' tables as {output_csv_path}")
        else:
            print("No tables found with 'Table 7-'.")


            

def save_table_to_csv(table_data, output_csv_path):
    # Save the extracted table data into a CSV file
    with open(output_csv_path, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerows(table_data)

# Example usage:
pdf_path = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf"
output_csv_path = "table7_only_output.csv"
extract_table7_tables(pdf_path, output_csv_path)

print(f"Completed scraping 'Table 7-' tables.")

'''



'''
import pdfplumber
import csv
import re
import PyPDF2

def extract_pdf_data(pdf_path):
    with open(pdf_path, 'rb') as pdf:
        reader = PyPDF2.PdfReader(pdf)
        text = ""
        for page in reader.pages:
            text += page.extract_text()

    def clean_string(text):
        return unicodedata.normalize('NFKD', text).encode('ascii', 'ignore').decode('ascii').replace('\n', ' ').lower().strip()        
    
    # Normalize text to handle unicode, spaces, and hyphens
    text = clean_string(text)

    # Extract Queue ID (without the "Q" prefix)
    queue_id = re.search(r"q[\s_-]*(\d+)", text, re.IGNORECASE)
    queue_id = queue_id.group(1) if queue_id else None
    
    # Extract Cluster Number, if not found, set it to 7
    cluster_number = re.search(r"queue[\s_-]*cluster[\s_-]*(\d+)", text)
    cluster_number = cluster_number.group(1) if cluster_number else "7"
    
    # Extract Deliverability Status
    deliverability_status = re.search(r"(\w+)\s*capacity deliverability status", text, re.IGNORECASE)
    deliverability_status = deliverability_status.group(1) if deliverability_status else None

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

    # Extract Point of Interconnection
    point_of_interconnection = extract_point_of_interconnection(pdf_path)

    

    # Initialize the data dictionary
    base_data = {
        "q_id": queue_id,
        "cluster": cluster_number,
        "req_deliverability": deliverability_status,
        "latitude": latitude,
        "longitude": longitude,
        "capacity": capacity,
        "network_mcr": None,
        "point_of_interconnection": point_of_interconnection
    }
    
    return base_data


def extract_point_of_interconnection(pdf_path):
    """
    Extract the "Point of Interconnection" from Table 1-.
    """
    with pdfplumber.open(pdf_path) as pdf:
        for page in pdf.pages:
            # Extract the text of the page to identify Table 1-
            page_text = page.extract_text()

            # Look for "Table 1-" in the text to identify the page with Table 1-
            if "Table 1-" in page_text:
                # Extract the tables on the current page
                tables = page.extract_tables(table_settings={
                    "horizontal_strategy": "lines",
                    "vertical_strategy": "lines"
                })
                
                # Loop through the tables to find Table 1- and extract "Point of Interconnection"
                for table in tables:
                    for row in table:
                        # If we find "Point of Interconnection" in the first column, return its value in the second column
                        if row and "Point of Interconnection" in row[0]:
                            return row[1]  # Return the value from the second column
                        
    return None  # Return None if "Point of Interconnection" is not found



def extract_table7_and_replace_none(pdf_path, output_csv_path):
    # Extract base data from the PDF
    base_data = extract_pdf_data(pdf_path)
    
    
    with pdfplumber.open(pdf_path) as pdf:
        full_table_data = []
        table_started = False

        for page_number, page in enumerate(pdf.pages):
            # Extract all text from the page to check for "Table 7-"
            page_text = page.extract_text()

            # Search for the line containing "Table 7-"
            table_title = find_table_title(page_text, "Table 7-")
            
            # Only process tables if "Table 7-" is found in the page's text
            if table_title:
                print(f"Processing tables on page {page_number + 1} with '{table_title}'...")

                # Extract tables using lines strategy for accurate table dimensions
                tables = page.find_tables(table_settings={
                    "horizontal_strategy": "lines",  # Use lines for row boundaries
                    "vertical_strategy": "lines",  # Use lines for column boundaries
                })

                # Search for patterns like "PTO" or "Reliability Network Upgrades" in the table title
                title_pattern = search_in_table_title(table_title, ["PTO", "Reliability Network Upgrade"])
                
                for table_obj in tables:
                    table = table_obj.extract()  # Extract the table content as list of lists (rows -> cells)
                    processed_table = []

                    for row_index, row in enumerate(table):
                        # If the first column (Type of Upgrade) is None, replace it with the title pattern
                        if row[0] is None or row[0].strip() == "":
                            row[0] = title_pattern  # Replace None with the found pattern from the table title

                        # Clean up the row to remove unwanted characters before saving to CSV
                        cleaned_row = [clean_text(cell) for cell in row]
                        processed_table.append(cleaned_row)

                    # Append to the final table
                    if not table_started:
                        full_table_data.extend(processed_table)  # Use the first row as the header
                        table_started = True  # Mark the start of a table
                    else:
                        # Append rows from continuation pages, excluding the header row
                        full_table_data.extend(processed_table[1:])  # Skip header in continuation pages

        # Now carry forward any remaining None values in the Type of Upgrade column
        full_table_data = carry_forward_none(full_table_data)

        # Duplicate base data for each row in the table (excluding the header)
        duplicated_base_data = [list(base_data.values()) for _ in range(len(full_table_data) - 1)]

        # Combine base data with table data
        final_data = [list(base_data.keys()) + full_table_data[0]]  # Header row
        for base_row, table_row in zip(duplicated_base_data, full_table_data[1:]):
            final_data.append(base_row + table_row)

        # Now save the combined data to a single CSV file
        save_table_to_csv(final_data, output_csv_path)

        print(f"Saved combined 'Table 7-' tables with base data as {output_csv_path}")

def find_table_title(page_text, table_identifier):
    """
    Find the full table title (e.g., "Table 7-1: Whatever Whatever") in the page's text.
    Returns the line that contains the table title.
    """
    # Use a regular expression to find the full table title line
    table_title_match = re.search(rf"({table_identifier}[^\n]+)", page_text)
    if table_title_match:
        return table_title_match.group(1)  # Return the matching line containing the table title
    return None  # Return None if no match is found

def search_in_table_title(table_title, patterns):
    """
    Search for specific patterns like "PTO" or "Reliability Network Upgrade" in the table title.
    The patterns parameter accepts regular expressions.
    """
    # Search for any of the patterns (case-insensitive) in the table title
    for pattern in patterns:
        if re.search(pattern, table_title, re.IGNORECASE):
            return pattern  # Return the matching pattern if found
    return ""  # Return an empty string if no match is found

def clean_text(text):
    """
    Clean up the text by removing unwanted characters or patterns.
    This ensures that special characters from regex patterns like '\s+' are not saved in the CSV.
    """
    if text is not None:
        # Remove any unwanted characters, such as \s+ (which was mistakenly added to the output)
        text = re.sub(r'\\[a-zA-Z]+', '', text)  # Remove escape sequences like \s
        return text.strip()  # Return cleaned text
    return ""

def carry_forward_none(table_data):
    """
    Carry forward the last valid value in the Type of Upgrade column if the current value is None or empty.
    """
    last_valid_value = None
    for row in table_data[1:]:  # Skip the header
        if row[0] is None or row[0].strip() == "":
            row[0] = last_valid_value  # Replace with the last valid value
        else:
            last_valid_value = row[0]  # Update the last valid value
    return table_data

def save_table_to_csv(table_data, output_csv_path):
    # Save the extracted table data into a CSV file
    with open(output_csv_path, mode='w', newline='', encoding='utf-8') as file:
        writer = csv.writer(file)
        writer.writerows(table_data)

# Example usage:
pdf_path = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/matched_pdfs_phase_i/Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf"
output_csv_path = "combined_table_with_base_data_output.csv"
extract_table7_and_replace_none(pdf_path, output_csv_path)

print(f"Completed extracting tables and base data into {output_csv_path}")

'''


'''

import pdfplumber
import pandas as pd
from pprint import pprint

# Open the PDF file
with pdfplumber.open("Q1030/C7PhI - Appendix A - Q1030   South Lake Solar.pdf") as pdf:
    # Select the first page
    page = pdf.pages[15]
    
    # Find all tables on the page
    tables = page.find_tables(table_settings={
        "horizontal_strategy": "lines",
        "vertical_strategy": "lines",
    })
    
    # Iterate through each detected table
    for table_obj in tables:
        # Extract table data
        extracted_table = table_obj.extract()
        
        # Convert to pandas DataFrame
        df = pd.DataFrame(extracted_table[1:], columns=extracted_table[0])
        
        pprint(df)
'''


import re

appendix_pattern = appendix_pattern = re.compile(r'(?i)(?:appendix|app|ap)[\s_-]*(A|1|I)', re.IGNORECASE) 
phase_i_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:1|I)\b|(?:_Ph[\s_-]*I_)', re.IGNORECASE)

# Test strings
test_strings = [
    "-Appendix_A",
    "Appendix A",
    "Appendix_A",
    "Appendix-A",
    "Appendix A",
    "-Appendix A",
    "_Appendix_A",
    "Q1847-Dranem Energy Storage-Appendix_A-C14PhI.pdf"
]

for s in test_strings:
    if  phase_i_pattern.search(s) and appendix_pattern.search(s):
        print(f"Matched: {s}")
    else:
        print(f"Did not match: {s}")

 

