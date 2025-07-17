'''
import os
import pdfplumber
import pandas as pd
import re
import PyPDF2
from tabulate import tabulate

class Cluster7:
    def __init__(self, directory, output_csv_path):
        self.directory = directory
        self.output_csv_path = output_csv_path
        self.core = pd.DataFrame()  # Initialize an empty DataFrame to store all tables
        self.cluster_7_patterns = [
            r"C[\s_-]*7", r"c[\s_-]*7", r"Cluster[\s_-]*7", r"cluster[\s_-]*7", 
            r"C[\s_-]*VII", r"c[\s_-]*VII", r"Cluster[\s_-]*VII", r"cluster[\s_-]*VII"
        ]
        self.pdf_count = 0
        self.scraped_count = 0

    def clean_column_headers(self, headers):
        cleaned_headers = []
        for header in headers:
            if header is None:
                header = ""
            elif isinstance(header, str):
                header = header.lower()
                header = re.sub(r'\s+', ' ', header)
                header = re.sub(r'\(.*?\)', '', header)  # Remove anything in parentheses
                header = re.sub(r'[^a-zA-Z0-9\s]', '', header)
                header = header.strip()
            cleaned_headers.append(header)
        return cleaned_headers

    def clean_string_cell(self, value):
        if isinstance(value, str):
            return value.replace('\n', ' ').strip()  # Remove newlines and extra spaces
        return value

    def contains_phrase(self, row, phrase):
        regex_pattern = re.sub(r"\s+", r"\\s*", phrase)
        pattern = re.compile(regex_pattern, flags=re.IGNORECASE)
        return row.astype(str).apply(lambda cell: bool(pattern.search(cell))).any()
    
    
    def merge_columns(self, df):
        """Merge columns that have similar names or convey the same information."""
        merge_columns = {
            "escalated cost x 1000": ["escalated costs x 1000", "estimated cost x 1000 escalated"],
            "estimated time to construct": ["estimated time to construct", "estimated time  to construct"],
            "total estimated cost x 1000 escalalted": ["total estimated cost x 1000 escalalted", "total estimated cost x 1000 escalated"],
            "adnu cost rate x 1000": ["adnu cost rate x 1000", "cost rate x 1000"],
            "description": ["description"],
            "capacity": ["capacity", "project size"],
        }
        

        # Identify unnamed columns (empty or NaN header)
        unnamed_columns = [col for col in df.columns if pd.isna(col) or col.strip() == ""]
        
        if unnamed_columns:
            # Merge unnamed columns into 'description' if they exist
            merge_columns["description"].extend(unnamed_columns)
        
        for new_col, old_cols in merge_columns.items():
            if any(col in df.columns for col in old_cols):
                df[new_col] = df[old_cols].bfill(axis=1).iloc[:, 0]
                df.drop(columns=[col for col in old_cols if col in df.columns and col != new_col], inplace=True)
        
        return df
      

    def extract_point_of_interconnection(self, pdf_path):
        """Extract the 'Point of Interconnection' from either Table 1 or using regex."""
        with pdfplumber.open(pdf_path) as pdf:
            for page in pdf.pages:
                page_text = page.extract_text()
                if "Table 1-" in page_text:
                    tables = page.extract_tables(table_settings={
                        "horizontal_strategy": "lines",
                        "vertical_strategy": "lines"
                    })
                    for table in tables:
                        for row in table:
                            if row and "Point of Interconnection" in row[0]:
                                return row[1]

        # Fallback to regex search in the text
        with open(pdf_path, 'rb') as pdf_file:
            reader = PyPDF2.PdfReader(pdf_file)
            text = ""
            for page in reader.pages:
                text += page.extract_text()

        point_of_interconnection_match = re.search(r"point of interconnection[:\s]*(.+)", text, re.IGNORECASE)
        if point_of_interconnection_match:
            return point_of_interconnection_match.group(1).strip()

        return None

    def search_gps_coordinates(self, text):
        """Search for GPS coordinates using multiple patterns."""
        gps_coords = re.search(r"gps coordinates:\s*([\d\.\-]+),\s*([\d\.\-]+)", text, re.IGNORECASE)
        if gps_coords:
            return gps_coords.groups()

        project_coords = re.search(r"latitude[:\s]*([\d\.\-]+)[^\d]+longitude[:\s]*([\d\.\-]+)", text, re.IGNORECASE)
        if project_coords:
            return project_coords.groups()

        gps_coords_directional = re.search(
            r"gps coordinates:\s*([\d\.\-]+)\s*[nNsS],\s*([\d\.\-]+)\s*[eEwW]", text, re.IGNORECASE)
        if gps_coords_directional:
            lat, lon = gps_coords_directional.groups()
            latitude = lat if "N" in text.upper() else f"-{lat}"  # Adjust latitude sign
            longitude = lon if "E" in text.upper() else f"-{lon}"  # Adjust longitude sign
            return (latitude, longitude)

        return (None, None)

    def extract_pdf_data(self, pdf_path):
        """Extract base data from the PDF and return as a DataFrame."""
        with open(pdf_path, 'rb') as pdf:
            reader = PyPDF2.PdfReader(pdf)
            text = ""
            for page in reader.pages:
                text += page.extract_text()

        text = self.clean_string_cell(text)

        queue_id = re.search(r"q[\s_-]*(\d+)", text, re.IGNORECASE)
        queue_id = queue_id.group(1) if queue_id else None

        cluster_number = re.search(r"queue[\s_-]*cluster[\s_-]*(\d+)", text)
        cluster_number = cluster_number.group(1) if cluster_number else "7"

        deliverability_status = re.search(r"(\w+)\s*capacity deliverability status", text, re.IGNORECASE)
        deliverability_status = deliverability_status.group(1) if deliverability_status else None

        # Search for GPS coordinates
        latitude, longitude = self.search_gps_coordinates(text)

        capacity = re.search(r"total rated output of (\d+)\s*mw", text, re.IGNORECASE)
        if capacity:
            capacity = int(capacity.group(1))
        else:
            capacity2 = re.search(r"(\d+)\s*mw", text)
            capacity = int(capacity2.group(1)) if capacity2 else None

        # Extract Point of Interconnection
        point_of_interconnection = self.extract_point_of_interconnection(pdf_path)

        # Initialize base data dictionary
        base_data = {
            "q_id": [queue_id],
            "cluster": [cluster_number],
            "req_deliverability": [deliverability_status],
            "latitude": [latitude],
            "longitude": [longitude],
            "capacity": [capacity],
            "point_of_interconnection": [point_of_interconnection]
        }

        return pd.DataFrame(base_data)

   
    def extract_table7_and_replace_none(self, pdf_path):
        base_data = self.extract_pdf_data(pdf_path)
        pdf_core = pd.DataFrame()

        with pdfplumber.open(pdf_path) as pdf:
            table_in_progress = pd.DataFrame()
            headers = None
            expected_columns = None
            title_pattern = None
            last_known_upgrade = None 

            for page_number, page in enumerate(pdf.pages):
                text = page.extract_text()



                

                if re.search(r"Table 7", text):
                    tables = page.find_tables(table_settings={
                        "horizontal_strategy": "lines",
                        "vertical_strategy": "lines",
                    })

                    if not title_pattern:
                        table_title = self.find_table_title(text, "Table 7")
                        title_pattern = self.search_in_table_title(table_title, ["PTO", "Reliability Network Upgrade", "ADNU", "LDNU", "RNU"])

                    if not tables:
                        continue

                    for table_index, table in enumerate(tables):
                        tab = table.extract()
                        if len(tab) == 0:
                            continue

                        current_headers = tab[0]
                        cleaned_headers = self.clean_column_headers(current_headers)
                        data_rows = tab[1:]

                        replaced = False
                        for row in data_rows:
                            if any(cell and "Total" in cell for cell in row):
                               break
                            
                            if not replaced and (row[0] is None or row[0].strip() == ""):
                                row[0] = title_pattern
                                replaced = True

                        title_pattern = None        
                            
                    
                        

                            if row[0] is None or row[0].strip() == "":
                                # If no previous value, use the title_pattern as default
                                if last_known_upgrade is None:
                                    row[0] = title_pattern
                                else:
                                    row[0] = last_known_upgrade  # Use the last known non-None value
                            else:
                                # Update last_known_upgrade with the current non-None value
                                last_known_upgrade = row[0]
                          
                                


                                
                        if headers and len(current_headers) == expected_columns:
                            continuation_data = pd.DataFrame(tab, columns=headers)
                            table_in_progress = pd.concat([table_in_progress, continuation_data], ignore_index=True)
                        else:
                            if not table_in_progress.empty:
                                pdf_core = pd.concat([pdf_core, table_in_progress], ignore_index=True)
                                table_in_progress = pd.DataFrame()

                            headers = cleaned_headers
                            expected_columns = len(headers)
                            table_in_progress = pd.DataFrame(data_rows, columns=headers)
                             

            if not table_in_progress.empty:
                pdf_core = pd.concat([pdf_core, table_in_progress], ignore_index=True)

        overlapping_columns = base_data.columns.intersection(pdf_core.columns)
        pdf_core = pdf_core.drop(columns=overlapping_columns)
        merged_df = pd.concat([base_data] * len(pdf_core), ignore_index=True).join(pdf_core)

        return merged_df

   



    def extract_table7_and_replace_none(self, pdf_path):
        base_data = self.extract_pdf_data(pdf_path)
        pdf_core = pd.DataFrame()

        with pdfplumber.open(pdf_path) as pdf:
            table_in_progress = pd.DataFrame()
            headers = None
            expected_columns = None
            title_queue = []  # Queue to store titles that need tables extracted

            for page_number in range(len(pdf.pages)):
                page = pdf.pages[page_number]
                text = page.extract_text()
                if not text:
                    continue

                # Extract all instances of "Table 7" titles on the current page
                lines = text.split('\n')
                for line in lines:
                    if re.search(r"Table 7", line, re.IGNORECASE):
                        table_title = self.find_table_title(line, "Table 7")
                        title_pattern = self.search_in_table_title(table_title, ["PTO", "Reliability Network Upgrade", "ADNU", "LDNU", "RNU"])
                        title_queue.append(title_pattern)

                # Find all tables on the current page
                tables = page.find_tables(table_settings={
                    "horizontal_strategy": "lines",
                    "vertical_strategy": "lines",
                })

                # If there are no tables but a title is waiting, move to the next page
                if not tables and title_queue:
                    continue

                # Associate titles with tables on the same page or across pages
                for table_index, table in enumerate(tables):
                    if title_queue:
                        # Extract the current table and associate it with the current title in the queue
                        tab = table.extract()
                        if len(tab) > 0:
                            if headers and expected_columns == len(tab[0]):
                                # If headers match, this is a continuation of the previous table
                                data_rows = tab  # All rows are data rows in a continuation
                            else:
                                # New table with headers and data rows
                                current_headers = tab[0]
                                cleaned_headers = self.clean_column_headers(current_headers)
                                headers = cleaned_headers
                                expected_columns = len(headers)
                                data_rows = tab[1:]  # Skip the header row

                            # Assign the title to the table rows if missing
                            replaced = False
                            for row in data_rows:
                                if any(cell and "Total" in cell for cell in row):
                                    break

                                if not replaced and (row[0] is None or row[0].strip() == ""):
                                    row[0] = title_queue[0]  # Assign the current title in the queue to the table
                                    replaced = True

                            # Append rows to the ongoing table
                            continuation_data = pd.DataFrame(data_rows, columns=headers)
                            table_in_progress = pd.concat([table_in_progress, continuation_data], ignore_index=True)

                            # Remove the processed title from the queue
                            title_queue.pop(0)

                # If any titles remain in the queue without a corresponding table on the current page,
                # they will be processed on the next page.
                if title_queue and not tables:
                    continue

                # If there is still a table in progress at the end of the page, add it to the final DataFrame
                if not table_in_progress.empty:
                    pdf_core = pd.concat([pdf_core, table_in_progress], ignore_index=True)
                    table_in_progress = pd.DataFrame()

            # If there is still an unprocessed table at the end, add it to the final DataFrame
            if not table_in_progress.empty:
                pdf_core = pd.concat([pdf_core, table_in_progress], ignore_index=True)

        # Join the base data with the scraped table data
        overlapping_columns = base_data.columns.intersection(pdf_core.columns)
        pdf_core = pdf_core.drop(columns=overlapping_columns)
        merged_df = pd.concat([base_data] * len(pdf_core), ignore_index=True).join(pdf_core)

        return merged_df
  
         

    def process_pdfs_in_folder(self):
        for root, dirs, files in os.walk(self.directory):
            for pdf_name in files:
                if pdf_name.endswith(".pdf") and any(re.search(pattern, pdf_name, re.IGNORECASE) for pattern in self.cluster_7_patterns):
                    pdf_path = os.path.join(root, pdf_name)
                    self.pdf_count += 1
                    print(f"Processing {pdf_name}")

                    try:
                        df = self.extract_table7_and_replace_none(pdf_path)
                        if not df.empty:
                            self.core = pd.concat([self.core, df], ignore_index=True)
                            self.scraped_count += 1
                            print(f"Scraped data from {pdf_name}")
                    except Exception as e:
                        print(f"Skipping {pdf_name} due to an error: {e}")


    def save_to_csv(self):
        # Clean up the entire DataFrame by cleaning string cells
        self.core = self.core.map(self.clean_string_cell)

        # Drop rows that contain specific phrases (e.g., "Type of Upgrade")
        self.core = self.core[~self.core.apply(lambda row: self.contains_phrase(row, "Type of Upgrade"), axis=1)]

        # Merge similar or duplicate columns
        self.core = self.merge_columns(self.core)

        # Ensure q_id is numeric for sorting, replace missing values with None
        self.core['q_id'] = pd.to_numeric(self.core['q_id'], errors='coerce')

        # Save the unsorted DataFrame to CSV first
        self.core.to_csv(self.output_csv_path, index=False)

        # Sort by 'q_id' after saving and reload the file to avoid issues with sorting
        #sorted_data = self.core.sort_values(by="q_id", ascending=True, na_position='last')
        #sorted_data.to_csv(self.output_csv_path, index=False)

        print(f"Data extracted and saved to {self.output_csv_path}")
        print(f"Total PDFs accessed: {self.pdf_count}")
        print(f"Total PDFs successfully scraped: {self.scraped_count}")
                    
 
    @staticmethod
    def find_table_title(page_text, table_identifier):
        table_title_match = re.search(rf"({table_identifier}[^\n]+)", page_text)
        return table_title_match.group(1) if table_title_match else None

    @staticmethod
    def search_in_table_title(table_title, patterns):
        for pattern in patterns:
            if re.search(pattern, table_title, re.IGNORECASE):
                return pattern
        return ""

# Usage
directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/archive/matched_pdfs_phase_i/"
output_csv_path = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/Cluster 7/c7_data.csv"
cluster7 = Cluster7(directory, output_csv_path)
cluster7.process_pdfs_in_folder()
cluster7.save_to_csv()





import pandas as pd
import re
import unicodedata

# Load the CSV file
df = pd.read_csv('/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/Cluster 7/c7_data.csv', dtype={'estimated_time_to_construct': str})

def convert_to_snake_case(column_name):
    column_name = column_name.strip().lower()
    column_name = re.sub(r'[\s\-]+', '_', column_name)
    column_name = re.sub(r'[^\w]', '', column_name)
    return column_name


def clean_string_cell(value):
    if isinstance(value, str):
        # Normalize Unicode to NFKD and remove non-ASCII characters
        value = unicodedata.normalize('NFKD', value).encode('ascii', 'ignore').decode('ascii')
        # Remove newlines and extra spaces
        value = value.replace('\n', ' ').strip()
    return value


df = df.map(clean_string_cell)



# Apply the function to all columns
df.columns = [convert_to_snake_case(col) for col in df.columns]

 
 






# Step 1: Create the 'item' column based on whether the 'type_of_upgrade' row or 'cost_allocation_factor' contains 'Total'
df['item'] = df.apply(
    lambda row: 'no' if (pd.notna(row['type_of_upgrade']) and 'Total' in str(row['type_of_upgrade'])) or 
                         (pd.notna(row['cost_allocation_factor']) and 'Total' in str(row['cost_allocation_factor']))
               else 'yes', axis=1)

# Step 2: Move 'item' column next to 'type_of_upgrade' column
cols = df.columns.tolist()
item_index = cols.index('item')
cols.insert(cols.index('type_of_upgrade') + 1, cols.pop(item_index))
df = df[cols]

# Step 3: Remove the 'Total' values from the 'cost_allocation_factor' column if they are already in the 'type_of_upgrade' column
df['cost_allocation_factor'] = df.apply(
    lambda row: None if pd.notna(row['type_of_upgrade']) and 'Total' in str(row['type_of_upgrade']) else row['cost_allocation_factor'], axis=1
)

# Step 4: For each `q_id` and `type_of_upgrade`, if it has only one row and no 'Total' is present in 'cost_allocation_factor', create a new `Total` row
new_rows = []
for q_id, group in df.groupby('q_id'):
    unique_upgrades = group['type_of_upgrade'].unique()

    # Check if there's already a "Total" in 'cost_allocation_factor' for this q_id
    if any('Total' in str(x) for x in group['cost_allocation_factor']):
        continue  # If Total exists under 'cost_allocation_factor', skip creating new total row for this q_id
    
    for upgrade in unique_upgrades:
        # Skip if "Total" is already present for this upgrade or if the upgrade is NaN
        if pd.isna(upgrade) or 'Total' in str(upgrade):
            continue
        
        # Get rows corresponding to this specific upgrade
        rows = group[group['type_of_upgrade'] == upgrade]
        
        if len(rows) == 1:
            # Duplicate the row and create a new row with "Total" under 'type_of_upgrade'
            original_row = rows.iloc[0].copy()
            total_row = original_row.copy()

            # Modify the total_row to reflect "Total"
            total_row['type_of_upgrade'] = 'Total'
            total_row['item'] = 'no'

            # Append the Total row immediately after the original row
            original_index = df[(df['q_id'] == q_id) & (df['type_of_upgrade'] == upgrade)].index[0]
            new_rows.append((original_index + 1, total_row))

# Step 5: Insert the new rows into the DataFrame in the correct order
for idx, row in sorted(new_rows, reverse=True):
    df = pd.concat([df.iloc[:idx], pd.DataFrame([row]), df.iloc[idx:]]).reset_index(drop=True)

# Step 6: Move "Total" from 'cost_allocation_factor' column to 'type_of_upgrade' column
df['type_of_upgrade'] = df.apply(
    lambda row: 'Total' if 'Total' in str(row['cost_allocation_factor']) and 'Total' not in str(row['type_of_upgrade']) else row['type_of_upgrade'],
    axis=1
)

# Step 7: After moving "Total" to 'type_of_upgrade', clear it from 'cost_allocation_factor'
df['cost_allocation_factor'] = df.apply(
    lambda row: None if 'Total' in str(row['cost_allocation_factor']) else row['cost_allocation_factor'],
    axis=1
)


def clean_estimated_time(value):
    if isinstance(value, str):
        value = re.sub(r'\s*months?\s*', '', value, flags=re.IGNORECASE).strip()
    return value
df['estimated_time_to_construct'] = df['estimated_time_to_construct'].apply(clean_estimated_time)



df['type_of_upgrade'] = df['type_of_upgrade'].apply(lambda x: re.sub(r'\s*\(note \d+\)', '', x, flags=re.IGNORECASE).strip() if isinstance(x, str) else x)




mappings = {
     'Reliability Network Upgrade': 'RNU',
     'Reliability Network upgrade': 'RNU',
     'Local Delivery Network Upgrade': 'LDNU',
     "PTOs Interconnection Facilities":'PTO_IF',
     'Area Delivery Network Upgrade':'ADNU',
     
}

# Apply transformations using a lambda function
df['type_of_upgrade'] = df['type_of_upgrade'].apply(lambda x: re.sub(r'\bupgrades\b', 'upgrade', x, flags=re.IGNORECASE).strip() if isinstance(x, str) else x)
df['type_of_upgrade'] = df['type_of_upgrade'].apply(lambda x: mappings.get(x, x) if isinstance(x, str) else x)
#df['type_of_upgrade'] = df['type_of_upgrade'].str.title()
# Step 8: Forward fill the non-empty values in 'type_of_upgrade' to replace empty cells with the previous value
df['type_of_upgrade'] = df['type_of_upgrade'].ffill()


df['upgrade'] = df.groupby(['q_id','type_of_upgrade'])['upgrade'].ffill()

 


 

 

# Step 9: Ensure 'Total' is correctly replaced in 'type_of_upgrade' when present
previous_type_of_upgrade = None

for i in range(len(df)):
    if df.at[i, 'type_of_upgrade'] == 'Total':
        if previous_type_of_upgrade is not None:
            df.at[i, 'type_of_upgrade'] = previous_type_of_upgrade
    else:
        previous_type_of_upgrade = df.at[i, 'type_of_upgrade']

# Step 10: Fill NaN in descriptive columns with None and NaN in numeric columns with 0
# List of numeric columns and non-numeric columns based on dataset structure
numeric_columns = ['cost_allocation_factor',  'estimated_cost_x_1000', 'estimated_time_to_construct', 'total_estimated_cost_x_1000_escalalted', 'adnu_cost_rate_x_1000', 'escalated_cost_x_1000', ]
non_numeric_columns = ['type_of_upgrade', 'upgrade', 'description']

for col in non_numeric_columns:
     df[col] = df[col].apply(lambda x: 'None' if pd.isna(x) else x)

# Replace '-' with NaN in numeric columns (so they can be converted to 0)
df[numeric_columns] = df[numeric_columns].replace('-', pd.NA)     

# Replace NaN in numeric columns with 0
df[numeric_columns] = df[numeric_columns].fillna(0)

# Step 11: Sort the DataFrame by q_id and type_of_upgrade
df['original_order'] = df.index
df = df.sort_values(by=['q_id', 'original_order'], ascending=[True, True])

# Optional: Drop the original_order column if not needed anymore
df = df.drop(columns=['original_order'])


#df = df.sort_values(by=['q_id', 'type_of_upgrade', 'upgrade', 'description'], ascending=[True, False, False, False])

# Step 12: Create the itemized dataset (rows where item == 'yes') and save to CSV
itemized_df = df[df['item'] == 'yes']
itemized_df.to_csv('/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/Cluster 7/c7_itemized.csv', index=False)

# Step 13: Create the totals dataset (rows where item == 'no') and drop unwanted columns
totals_df = df[df['item'] == 'no'].drop(columns=['upgrade', 'description', 'cost_allocation_factor', 'estimated_time_to_construct'])
totals_df.to_csv('/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/Cluster 7/c7_totals.csv', index=False)

print(f"Itemized rows saved to 'itemized.csv'.")
print(f"Filtered Total rows saved to 'totals.csv'.")

print(df['type_of_upgrade'].unique())
 
'''





import pandas as pd
import re
import unicodedata

# Load the CSV file
df = pd.read_csv('/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/C_6_7_8_data.csv', dtype={'estimated_time_to_construct': str})


def merge_columns(df):
    merge_columns_dict = {
            "escalated cost x 1000": ["escalated costs x 1000", "estimated cost x 1000 escalated" , "estimated cost x 1000 escalated with itcca"],
            "estimated time to construct": ["estimated time to construct", "estimated time  to construct", "estimated time"],
            "total estimated cost x 1000 escalalted": ["total estimated cost x 1000 escalalted", "total estimated cost x 1000 escalated"],
            "adnu cost rate x 1000": ["adnu cost rate x 1000", "cost rate x 1000"],
            "description": ["description"],
            "capacity": ["capacity", "project size"],
            "cost allocation factor": ["cost allocation factor", "cost allocatio n factor"],


        }

    # Identify unnamed columns (columns with no name or those starting with "Unnamed")
    unnamed_columns = [col for col in df.columns if pd.isna(col) or col.strip() == "" or col.startswith("Unnamed")]

    if unnamed_columns:
        # Merge unnamed columns into 'description' if they exist
        merge_columns_dict["description"].extend(unnamed_columns)

    # Iterate over the dictionary to merge columns
    for new_col, old_cols in merge_columns_dict.items():
        if any(col in df.columns for col in old_cols):
            # Create a new column by backfilling data from the old columns
            df[new_col] = df[old_cols].bfill(axis=1).iloc[:, 0]
            # Drop the old columns, except for the new one we just created
            df.drop(columns=[col for col in old_cols if col in df.columns and col != new_col], inplace=True)

    return df


     

# Apply the merge columns function to the DataFrame
df = merge_columns(df) 
 



def convert_to_snake_case(column_name):
    column_name = column_name.strip().lower()
    column_name = re.sub(r'[\s\-]+', '_', column_name)
    column_name = re.sub(r'[^\w]', '', column_name)
    return column_name


def clean_string_cell(value):
    if isinstance(value, str):
        # Normalize Unicode to NFKD and remove non-ASCII characters
        value = unicodedata.normalize('NFKD', value).encode('ascii', 'ignore').decode('ascii')
        # Remove newlines and extra spaces
        value = value.replace('\n', ' ').strip()
    return value


df = df.map(clean_string_cell)



# Apply the function to all columns
df.columns = [convert_to_snake_case(col) for col in df.columns]

 
 






# Step 1: Create the 'item' column based on whether the 'type_of_upgrade' row or 'cost_allocation_factor' contains 'Total'
df['item'] = df.apply(
    lambda row: 'no' if (pd.notna(row['type_of_upgrade']) and 'Total' in str(row['type_of_upgrade'])) or 
                         (pd.notna(row['cost_allocation_factor']) and 'Total' in str(row['cost_allocation_factor']))
               else 'yes', axis=1)

# Step 2: Move 'item' column next to 'type_of_upgrade' column
cols = df.columns.tolist()
item_index = cols.index('item')
cols.insert(cols.index('type_of_upgrade') + 1, cols.pop(item_index))
df = df[cols]

# Step 3: Remove the 'Total' values from the 'cost_allocation_factor' column if they are already in the 'type_of_upgrade' column
df['cost_allocation_factor'] = df.apply(
    lambda row: None if pd.notna(row['type_of_upgrade']) and 'Total' in str(row['type_of_upgrade']) else row['cost_allocation_factor'], axis=1
)

# Step 4: For each `q_id` and `type_of_upgrade`, if it has only one row and no 'Total' is present in 'cost_allocation_factor', create a new `Total` row
new_rows = []
for q_id, group in df.groupby('q_id'):
    unique_upgrades = group['type_of_upgrade'].unique()

    # Check if there's already a "Total" in 'cost_allocation_factor' for this q_id
    if any('Total' in str(x) for x in group['cost_allocation_factor']):
        continue  # If Total exists under 'cost_allocation_factor', skip creating new total row for this q_id
    
    for upgrade in unique_upgrades:
        # Skip if "Total" is already present for this upgrade or if the upgrade is NaN
        if pd.isna(upgrade) or 'Total' in str(upgrade):
            continue
        
        # Get rows corresponding to this specific upgrade
        rows = group[group['type_of_upgrade'] == upgrade]
        
        if len(rows) == 1:
            # Duplicate the row and create a new row with "Total" under 'type_of_upgrade'
            original_row = rows.iloc[0].copy()
            total_row = original_row.copy()

            # Modify the total_row to reflect "Total"
            total_row['type_of_upgrade'] = 'Total'
            total_row['item'] = 'no'

            # Append the Total row immediately after the original row
            original_index = df[(df['q_id'] == q_id) & (df['type_of_upgrade'] == upgrade)].index[0]
            new_rows.append((original_index + 1, total_row))

# Step 5: Insert the new rows into the DataFrame in the correct order
for idx, row in sorted(new_rows, reverse=True):
    df = pd.concat([df.iloc[:idx], pd.DataFrame([row]), df.iloc[idx:]]).reset_index(drop=True)

# Step 6: Move "Total" from 'cost_allocation_factor' column to 'type_of_upgrade' column
df['type_of_upgrade'] = df.apply(
    lambda row: 'Total' if 'Total' in str(row['cost_allocation_factor']) and 'Total' not in str(row['type_of_upgrade']) else row['type_of_upgrade'],
    axis=1
)

# Step 7: After moving "Total" to 'type_of_upgrade', clear it from 'cost_allocation_factor'
df['cost_allocation_factor'] = df.apply(
    lambda row: None if 'Total' in str(row['cost_allocation_factor']) else row['cost_allocation_factor'],
    axis=1
)


def clean_estimated_time(value):
    if isinstance(value, str):
        value = re.sub(r'\s*months?\s*', '', value, flags=re.IGNORECASE).strip()
    return value
df['estimated_time_to_construct'] = df['estimated_time_to_construct'].apply(clean_estimated_time)



df['type_of_upgrade'] = df['type_of_upgrade'].apply(lambda x: re.sub(r'\s*\(note \d+\)', '', x, flags=re.IGNORECASE).strip() if isinstance(x, str) else x)




mappings = {
     'Reliability Network Upgrade': 'RNU',
     'Reliability Network upgrade': 'RNU',
     'Local Delivery Network Upgrade': 'LDNU',
     "PTOs Interconnection Facilities":'PTO_IF',
     'Area Delivery Network Upgrade':'ADNU',
     'Reliability Network upgrade to Physically Interconnect': 'RNU',
     'PTO' : 'PTO_IF',

     
}

# Apply transformations using a lambda function
df['type_of_upgrade'] = df['type_of_upgrade'].apply(lambda x: re.sub(r'\bupgrades\b', 'upgrade', x, flags=re.IGNORECASE).strip() if isinstance(x, str) else x)
df['type_of_upgrade'] = df['type_of_upgrade'].apply(lambda x: mappings.get(x, x) if isinstance(x, str) else x)
#df['type_of_upgrade'] = df['type_of_upgrade'].str.title()
# Step 8: Forward fill the non-empty values in 'type_of_upgrade' to replace empty cells with the previous value
df['type_of_upgrade'] = df['type_of_upgrade'].ffill()


df['upgrade'] = df.groupby(['q_id','type_of_upgrade'])['upgrade'].ffill()

 


 

 

# Step 9: Ensure 'Total' is correctly replaced in 'type_of_upgrade' when present
previous_type_of_upgrade = None

for i in range(len(df)):
    if df.at[i, 'type_of_upgrade'] == 'Total':
        if previous_type_of_upgrade is not None:
            df.at[i, 'type_of_upgrade'] = previous_type_of_upgrade
    else:
        previous_type_of_upgrade = df.at[i, 'type_of_upgrade']

# Step 10: Fill NaN in descriptive columns with None and NaN in numeric columns with 0
# List of numeric columns and non-numeric columns based on dataset structure
numeric_columns = ['cost_allocation_factor',  'estimated_cost_x_1000', 'estimated_time_to_construct', 'total_estimated_cost_x_1000_escalalted', 'adnu_cost_rate_x_1000', 'escalated_cost_x_1000',
                    'estimated_cost_x_1000_escalated_without_itcca', 'adnu_cost_rate_x_1000_escalated']
non_numeric_columns = ['type_of_upgrade', 'upgrade', 'description']

for col in non_numeric_columns:
     df[col] = df[col].apply(lambda x: 'None' if pd.isna(x) else x)

# Replace '-' with NaN in numeric columns (so they can be converted to 0)
df[numeric_columns] = df[numeric_columns].replace('-', pd.NA)     

# Replace NaN in numeric columns with 0
df[numeric_columns] = df[numeric_columns].fillna(0)

# Step 11: Sort the DataFrame by q_id and type_of_upgrade
df['original_order'] = df.index
df = df.sort_values(by=['q_id', 'original_order'], ascending=[True, True])

# Optional: Drop the original_order column if not needed anymore
df = df.drop(columns=['original_order'])

df = df.drop(columns=['project_parameters', 'project_specific_data'])

#df = df.sort_values(by=['q_id', 'type_of_upgrade', 'upgrade', 'description'], ascending=[True, False, False, False])

# Step 12: Create the itemized dataset (rows where item == 'yes') and save to CSV
itemized_df = df[df['item'] == 'yes']
itemized_df.to_csv('/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/Cluster 7/c678_itemized.csv', index=False)

# Step 13: Create the totals dataset (rows where item == 'no') and drop unwanted columns
totals_df = df[df['item'] == 'no'].drop(columns=['upgrade', 'description', 'cost_allocation_factor', 'estimated_time_to_construct'])
totals_df.to_csv('/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/output/Cluster 7/c678_totals.csv', index=False)

print(f"Itemized rows saved to 'itemized.csv'.")
print(f"Filtered Total rows saved to 'totals.csv'.")

print(df['type_of_upgrade'].unique())
