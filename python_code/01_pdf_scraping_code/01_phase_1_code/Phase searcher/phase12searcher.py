#this code searches for pdfs that contain the words "Phase I" and 
# "Appendix A" in the file name, creates an csv file with the list of names
# and then creates a new folder which contains the matched pdfs.

import os
import re
import csv
import shutil
import PyPDF2
from PyPDF2 import PdfReader
new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/"
os.chdir(new_directory)
 
'''
# Directory where the cleaned PDFs are stored
pdf_dir = "cleaned_pdfs"  # Replace with your actual directory

# Regex pattern to match variations of "Phase I" (e.g., Phase I, Ph I, etc.)
#this picked up both phase I and phase II
#phase_pattern = re.compile(r'(?i)phase[\s_-]*I|ph[\s_-]*I', re.IGNORECASE)
#(?i) means case insensitive, \b is a word boundary i.e ensures Phase I is treated as a sep
# word and not part of a larger word, (?:) is a non-capturing group allowing to match either or
# of the two words, [\s_-]* means zero or more whitespace or hyphen, | is an OR operator
#  (?![\s_-]*I means negative lookahead, i.e. match only if the word is not followed by I
# so this pattern will match Phase I, Ph I, Phase-I, Ph-I, Phase I- etc. but not Phase II 
#and its variations
# \s is whitespace, * means zero or more, - is a literal hyphen


#phase_pattern = re.compile(r'(?i)\b(?:phase|ph)[\s_-]*I\b(?![\s_-]*I)', re.IGNORECASE)

 
# New directories to copy matching PDFs for Phase I and II
output_pdf_dir_phase_i = "matched_pdfs_phase_i"
output_pdf_dir_phase_ii = "matched_pdfs_phase_ii"

# If they exist, remove and recreate them
for directory in [output_pdf_dir_phase_i, output_pdf_dir_phase_ii]:
    if os.path.exists(directory):
        shutil.rmtree(directory)
    os.makedirs(directory, exist_ok=True)

# Regex pattern to match variations of "Phase I" and avoid "Phase II"
#phase_i_pattern = re.compile(r'\b[Pp][Hh]ase[\s_-]*[I1](?![I1])\b|\b[Pp][Hh][\s_-]*[I1](?![I1])\b', re.IGNORECASE)
# Updated Phase I pattern that excludes "Phase II"
phase_i_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:1|I)\b|(?:_Ph[\s_-]*I_)', re.IGNORECASE)


 




# Regex pattern for Phase II
phase_ii_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:II|2)\b|(?:_Ph[\s_-]*II_)', re.IGNORECASE)







# Regex pattern to match "Appendix A"
appendix_pattern = appendix_pattern = re.compile(r'(?i)(?:appendix|app|ap)[\s_-]*(A|1)', re.IGNORECASE) 
#re.compile(r'Appendix A', re.IGNORECASE)


# Counters for files matching the Phase I and Phase II patterns
matching_files_count_phase_i = 0
matching_files_count_phase_ii = 0

# Lists to store the names of matching files
matching_files_phase_i = []
matching_files_phase_ii = []



 


for root, dirs, files in os.walk(pdf_dir):
    for filename in files:
        if filename.endswith(".pdf"):
            file_path = os.path.join(root, filename)
            relative_path = os.path.relpath(file_path, pdf_dir).strip()   # Get the relative path

            # Check for Phase I pattern
            if phase_i_pattern.search(filename) and appendix_pattern.search(filename):
                # Copy matching PDF to the Phase I output directory
                shutil.copy(file_path, output_pdf_dir_phase_i)
                matching_files_phase_i.append(relative_path)  # Store relative path
                matching_files_count_phase_i += 1

            # Check for Phase II pattern
            elif phase_ii_pattern.search(filename) and appendix_pattern.search(filename):
                # Copy matching PDF to the Phase II output directory
                shutil.copy(file_path, output_pdf_dir_phase_ii)
                matching_files_phase_ii.append(relative_path)  # Store relative path
                matching_files_count_phase_ii += 1
# Save matching file names to CSV files
# Define CSV output paths for Phase I and Phase II
# Define CSV output paths for Phase I and Phase II
output_csv_file_phase_i = os.path.join(output_pdf_dir_phase_i, "phase_i_matching_files.csv")
output_csv_file_phase_ii = os.path.join(output_pdf_dir_phase_ii, "phase_ii_matching_files.csv")

# Phase I: Write matching Phase I PDFs to CSV with quoting
with open(output_csv_file_phase_i, mode='w', newline='', encoding='utf-8') as file_i:
    writer_i = csv.writer(file_i, quoting=csv.QUOTE_ALL)  # Ensure all fields are quoted
    writer_i.writerow(["File Path"])  # Write header
    for pdf_file in matching_files_phase_i:
        writer_i.writerow([pdf_file])  # Write each cleaned relative file path

# Phase II: Write matching Phase II PDFs to CSV with quoting
with open(output_csv_file_phase_ii, mode='w', newline='', encoding='utf-8') as file_ii:
    writer_ii = csv.writer(file_ii, quoting=csv.QUOTE_ALL)  # Ensure all fields are quoted
    writer_ii.writerow(["File Path"])  # Write header
    for pdf_file in matching_files_phase_ii:
        writer_ii.writerow([pdf_file])  # Write each cleaned relative file path


# Report the results
print(f"Total number of PDF files matching 'Phase I': {matching_files_count_phase_i}")
print(f"Matching PDF files for Phase I have been saved in: {output_pdf_dir_phase_i}")
print(f"CSV file has been saved in: {output_csv_file_phase_i}")

print(f"Total number of PDF files matching 'Phase II': {matching_files_count_phase_ii}")
print(f"Matching PDF files for Phase II have been saved in: {output_pdf_dir_phase_ii}")
print(f"CSV file has been saved in: {output_csv_file_phase_ii}")
'''

'''
import os
import re
import csv
import shutil
import pdfplumber

# Set the directory
new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/"
os.chdir(new_directory)

pdf_dir = "clean_pdfs"  # Replace with your actual directory
output_pdf_dir_phase_i = "matched_pdfs_phase_i"
output_pdf_dir_phase_ii = "matched_pdfs_phase_ii"

# Cleanup and create output directories
for directory in [output_pdf_dir_phase_i, output_pdf_dir_phase_ii]:
    #if os.path.exists(directory):
    #    shutil.rmtree(directory)
    os.makedirs(directory, exist_ok=True)

# Regex patterns for filename-based matching
phase_i_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:1|I)\b|(?:_Ph[\s_-]*I_)', re.IGNORECASE)
# Regex pattern for Phase II
phase_ii_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:II|2)\b|(?:_Ph[\s_-]*II_)', re.IGNORECASE)
# Regex pattern to match "Appendix A"
appendix_pattern = appendix_pattern = re.compile(r'(?i)(?:appendix|app|ap)[\s_-]*(A|1|I)', re.IGNORECASE) 
q_id_pattern = re.compile(r'Q\d+', re.IGNORECASE)  # Pattern to identify Q_IDs

# Counters for files matching the Phase I and Phase II patterns
matching_files_count_phase_i = 0
matching_files_count_phase_ii = 0

# Lists to store the names of matching files
matching_files_phase_i = []
matching_files_phase_ii = []

# Step 1: Run the original pattern matching and organize files into subfolders by q_id
for root, dirs, files in os.walk(pdf_dir):
    for filename in files:
        if filename.endswith(".pdf"):
            file_path = os.path.join(root, filename)
            relative_path = os.path.relpath(file_path, pdf_dir).strip()

            # Check for Phase I pattern
            if phase_i_pattern.search(filename) and appendix_pattern.search(filename):
                # Extract Q_ID and create subfolder
                q_id_match = q_id_pattern.search(filename)
                if q_id_match:
                    q_id = q_id_match.group(0)
                    q_id_folder = os.path.join(output_pdf_dir_phase_i, q_id)
                    os.makedirs(q_id_folder, exist_ok=True)
                    shutil.copy(file_path, q_id_folder)
                else:
                    shutil.copy(file_path, output_pdf_dir_phase_i)

                matching_files_phase_i.append(relative_path)
                matching_files_count_phase_i += 1

            # Check for Phase II pattern
            elif phase_ii_pattern.search(filename) and appendix_pattern.search(filename):
                # Extract Q_ID and create subfolder
                q_id_match = q_id_pattern.search(filename)
                if q_id_match:
                    q_id = q_id_match.group(0)
                    q_id_folder = os.path.join(output_pdf_dir_phase_ii, q_id)
                    os.makedirs(q_id_folder, exist_ok=True)
                    shutil.copy(file_path, q_id_folder)
                else:
                    shutil.copy(file_path, output_pdf_dir_phase_ii)
                matching_files_phase_ii.append(relative_path)
                matching_files_count_phase_ii += 1

# Save matching file names to CSV files
output_csv_file_phase_i = os.path.join(output_pdf_dir_phase_i, "phase_i_matching_files.csv")
output_csv_file_phase_ii = os.path.join(output_pdf_dir_phase_ii, "phase_ii_matching_files.csv")

with open(output_csv_file_phase_i, mode='w', newline='', encoding='utf-8') as file_i:
    writer_i = csv.writer(file_i, quoting=csv.QUOTE_ALL)
    writer_i.writerow(["File Path"])
    for pdf_file in matching_files_phase_i:
        writer_i.writerow([pdf_file])

with open(output_csv_file_phase_ii, mode='w', newline='', encoding='utf-8') as file_ii:
    writer_ii = csv.writer(file_ii, quoting=csv.QUOTE_ALL)
    writer_ii.writerow(["File Path"])
    for pdf_file in matching_files_phase_ii:
        writer_ii.writerow([pdf_file])

# Report the results of Step 1
print(f"Step 1 Complete: Matching based on filename patterns")
print(f"Total number of PDF files matching 'Phase I': {matching_files_count_phase_i}")
print(f"Total number of PDF files matching 'Phase II': {matching_files_count_phase_ii}")

'''


import os
import re
import shutil

# Set the directory
new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/"
os.chdir(new_directory)

pdf_dir = "data"  # Replace with your actual directory

# Regex patterns for filename-based matching
phase_i_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:1|I)\b|(?:_Ph[\s_-]*I_)', re.IGNORECASE)
# Regex pattern for Phase II
phase_ii_pattern = re.compile(r'(?i)(?:phase|ph)[\s_-]*(?:II|2)\b|(?:_Ph[\s_-]*II_)', re.IGNORECASE)
# Regex pattern to match "Appendix A"
appendix_pattern = re.compile(r'(?i)(?:appendix|app|ap)[\s_-]*(A|1|I)', re.IGNORECASE)

# Counters for non-empty phase 1 and phase 2 subfolders
non_empty_phase_1_count = 0
non_empty_phase_2_count = 0

# Iterate through each project folder inside clean_pdfs
for project_folder in os.listdir(pdf_dir):
    project_path = os.path.join(pdf_dir, project_folder)

    # Ensure we're working with directories only (skip any files)
    if os.path.isdir(project_path):
        # Create subfolders (Phase 1, Phase 2, GIA, dump) inside each project folder
        subfolder_names = ['01_interconnection_request', '02_phase_1_study', '03_phase_2_study', '04_GIA', '05_reassesment', 'document_dump']
        for folder in subfolder_names:
            subfolder_path = os.path.join(project_path, folder)
            os.makedirs(subfolder_path, exist_ok=True)

        # Track if any files are moved to phase 1 or phase 2 subfolders
        phase_1_has_files = False
        phase_2_has_files = False

        for root, dirs, files in os.walk(project_path):
            # Process other files and subfolders
            for filename in files:
                file_path = os.path.join(root, filename)

                if filename.endswith(".pdf"):  # Process only PDF files for Phase matching
                    if phase_i_pattern.search(filename) and appendix_pattern.search(filename):
                        # Move Phase 1 + Appendix A files to the Phase 1 subfolder
                        shutil.move(file_path, os.path.join(project_path, '02_phase_1_study', filename))
                        phase_1_has_files = True
                    elif appendix_pattern.search(filename) and not phase_ii_pattern.search(filename):
                        # Move Appendix A files (without Phase II) to the Phase 1 subfolder
                        shutil.move(file_path, os.path.join(project_path, '02_phase_1_study', filename))
                        phase_1_has_files = True
                    elif phase_ii_pattern.search(filename):
                        # Move Phase II + Appendix A files to the Phase 2 subfolder
                        shutil.move(file_path, os.path.join(project_path, '03_phase_2_study', filename))
                        phase_2_has_files = True
                    else:
                        # Move other PDF files to dump
                        shutil.move(file_path, os.path.join(project_path, 'document_dump', filename))
                else:
                    # Move all other files (non-PDFs) to dump
                    shutil.move(file_path, os.path.join(project_path, 'document_dump', filename))

        # After processing files, move any unexpected subfolders to the document dump
        for subfolder in os.listdir(project_path):
            subfolder_path = os.path.join(project_path, subfolder)

            if os.path.isdir(subfolder_path) and subfolder not in subfolder_names:
                # Move unexpected subfolder to document dump 
                shutil.move(subfolder_path, os.path.join(project_path, 'document_dump', subfolder))
                print(f"Moved unexpected subfolder {subfolder} to document_dump")

        # Increment counters if files were moved to Phase 1 or Phase 2
        if phase_1_has_files:
            non_empty_phase_1_count += 1
        if phase_2_has_files:
            non_empty_phase_2_count += 1

# Output the count of non-empty Phase 1 and Phase 2 subfolders
print(f"Total number of non-empty 'Phase 1' subfolders: {non_empty_phase_1_count}")
print(f"Total number of non-empty 'Phase 2' subfolders: {non_empty_phase_2_count}")
 
