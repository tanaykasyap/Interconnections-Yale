 


'''
import os
import re
import shutil
import pdfplumber
import logging
from concurrent.futures import ProcessPoolExecutor, as_completed
from pprint import pprint

def process_project(project, pdf_dir, output_pdf_dir_phase_i):
    total_subfolders_checked = 0
    new_subfolders_created = 0
    project_folder_path = os.path.join(pdf_dir, project)
    project_subfolder_count = 0

    for root, dirs, files in os.walk(project_folder_path):
        project_subfolder_count += 1  # Count each subfolder (root itself counts as a folder)

        for filename in files:
            if filename.endswith(".pdf"):
                file_path = os.path.join(root, filename)

                try:
                    # Try to open the PDF file and check if "Phase I Report" exists on the first page
                    with pdfplumber.open(file_path) as pdf:
                        if len(pdf.pages) > 0:  # Check if the PDF has at least one page
                            first_page = pdf.pages[0].extract_text()
                            if first_page and re.search(r'phase i report', first_page, re.IGNORECASE):
                                print(f"'Phase I Report' found in {filename}")

                                # Create the Q-prefixed subfolder in matched_pdfs_phase_i
                                q_id_folder = os.path.join(output_pdf_dir_phase_i, f"Q{project}")
                                if not os.path.exists(q_id_folder):
                                    os.makedirs(q_id_folder)
                                    new_subfolders_created += 1  # Increment the counter for new subfolders

                                # Copy the PDF to the Q-prefixed folder
                                shutil.copy(file_path, q_id_folder)
                                print(f"Copied {filename} to {q_id_folder}")

                except Exception as e:
                    # Log the error and file path
                    logging.error(f"Error processing file {file_path}: {str(e)}")
                    print(f"Skipping file {filename} due to an error: {e}")

    return project_subfolder_count, new_subfolders_created

if __name__ == '__main__':
    # Set the working directory
    new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/"
    os.chdir(new_directory)

    pdf_dir = "clean_pdfs"
    output_pdf_dir_phase_i = "matched_pdfs_phase_i"

    # Create the output directory if it doesn't exist
    if not os.path.exists(output_pdf_dir_phase_i):
        os.makedirs(output_pdf_dir_phase_i)

    # Regex pattern to extract project number from cleaned_pdfs subfolders and Q-prefixed project number from matched_pdfs_phase_i
    project_number_pattern = re.compile(r'(\d+[A-Z]*)', re.IGNORECASE)
    q_id_pattern = re.compile(r'Q(\d+)', re.IGNORECASE)

    # Setup logging for errors
    logging.basicConfig(filename='phase12_errors.log', level=logging.ERROR)

    # Get the project numbers from cleaned_pdfs (subfolder names without prefix 'Q')
    cleaned_pdfs_projects = set()
    for folder_name in os.listdir(pdf_dir):
        match = project_number_pattern.match(folder_name)
        if match:
            cleaned_pdfs_projects.add(match.group(1))

    # Get the Q-prefixed project numbers from matched_pdfs_phase_i (strip off 'Q' prefix)
    matched_pdfs_projects = set()
    for folder_name in os.listdir(output_pdf_dir_phase_i):
        match = q_id_pattern.match(folder_name)
        if match:
            matched_pdfs_projects.add(match.group(1))

    # Find projects in cleaned_pdfs that are not in matched_pdfs_phase_i
    missing_projects = cleaned_pdfs_projects - matched_pdfs_projects
    pprint(f"Missing projects: {missing_projects}")

    total_subfolders_checked = 0
    new_subfolders_created = 0

    # Use ProcessPoolExecutor for parallel processing
    with ProcessPoolExecutor() as executor:
        futures = {executor.submit(process_project, project, pdf_dir, output_pdf_dir_phase_i): project for project in missing_projects}

        for future in as_completed(futures):
            project = futures[future]
            try:
                subfolder_count, new_subfolder_count = future.result()
                total_subfolders_checked += subfolder_count
                new_subfolders_created += new_subfolder_count
                print(f"Processed project {project} with {subfolder_count} subfolders.")
            except Exception as exc:
                logging.error(f"Project {project} generated an exception: {exc}")

    # Final Debug Output
    print(f"Total number of subfolders checked in missing projects: {total_subfolders_checked}")
    print(f"Total number of new subfolders created in matched_pdfs_phase_i: {new_subfolders_created}")
 
'''


import os
import re
import shutil
import pdfplumber
import logging
from concurrent.futures import ProcessPoolExecutor, as_completed

# Function to check if the '02_phase_1_study' folder is empty
def is_phase_1_study_empty(project_path):
    phase_1_study_path = os.path.join(project_path, '02_phase_1_study')
    return not os.listdir(phase_1_study_path)  # Returns True if empty

# Function to process the project folder and look for Phase I PDFs in document_dump
def process_project(project, pdf_dir):
    project_folder_path = os.path.join(pdf_dir, project)
    document_dump_path = os.path.join(project_folder_path, 'document_dump')
    phase_1_study_path = os.path.join(project_folder_path, '02_phase_1_study')

    total_files_checked = 0
    files_moved_to_phase_1 = 0

    # Regex pattern for Phase I study
    phase_i_study_pattern = re.compile(r'(?i)(?:phase)[\s]*(?:I)\b.*report', re.IGNORECASE)
    # This pattern matches "Phase I" (with or without spaces) followed by "study".

    # Traverse the document_dump folder and its subfolders
    for root, dirs, files in os.walk(document_dump_path):
        for filename in files:
            if filename.endswith(".pdf"):
                file_path = os.path.join(root, filename)

                try:
                    # Open PDF and check if "Phase I study" exists on the first page
                    with pdfplumber.open(file_path) as pdf:
                        if len(pdf.pages) > 0:  # Ensure the PDF has at least one page
                            first_page = pdf.pages[0].extract_text()

                            # Check for "Phase I study"
                            if first_page and phase_i_study_pattern.search(first_page):
                                print(f"'Phase I study' found in {filename}")

                                # Move the PDF to the 02_phase_1_study subfolder
                                shutil.move(file_path, os.path.join(phase_1_study_path, filename))
                                files_moved_to_phase_1 += 1

                except Exception as e:
                    # Log the error and skip the file
                    logging.error(f"Error processing file {file_path}: {str(e)}")
                    print(f"Skipping file {filename} due to error: {e}")

                total_files_checked += 1

    return total_files_checked, files_moved_to_phase_1

if __name__ == '__main__':
    # Set the working directory
    new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper/"
    os.chdir(new_directory)

    pdf_dir = "data"

    # Setup logging for errors
    logging.basicConfig(filename='phase_i_search_errors.log', level=logging.ERROR)

    # Gather all projects to process (where 02_phase_1_study is empty)
    projects_to_process = []
    for project_folder in os.listdir(pdf_dir):
        project_path = os.path.join(pdf_dir, project_folder)
        phase_1_study_path = os.path.join(project_path, '02_phase_1_study')

        # Ensure it's a directory and '02_phase_1_study' exists
        if os.path.isdir(project_path) and os.path.exists(phase_1_study_path):
            if is_phase_1_study_empty(project_path):
                projects_to_process.append(project_folder)

    total_files_checked = 0
    files_moved_to_phase_1 = 0

    # Use ProcessPoolExecutor for parallel processing
    with ProcessPoolExecutor() as executor:
        futures = {executor.submit(process_project, project, pdf_dir): project for project in projects_to_process}

        for future in as_completed(futures):
            project = futures[future]
            try:
                files_checked, files_moved = future.result()
                total_files_checked += files_checked
                files_moved_to_phase_1 += files_moved
                print(f"Processed project {project}: {files_checked} files checked, {files_moved} moved to Phase 1.")
            except Exception as exc:
                logging.error(f"Project {project} generated an exception: {exc}")

    # Final summary
    print(f"Total number of files checked: {total_files_checked}")
    print(f"Total number of files moved to '02_phase_1_study': {files_moved_to_phase_1}")


