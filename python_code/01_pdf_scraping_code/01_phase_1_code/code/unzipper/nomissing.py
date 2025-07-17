
# this is a script that creates a folder which only contains projects which have a pdf document
'''
Total number of subfolders processed: 1251
Total number of non-empty subfolders: 1237
Total number of subfolders with only PDFs: 1233
Subfolders without any PDFs:
417 # excel file says this one has a pdf, but we just have some msg.file
zOEpxM # just some word document 
2058 # excel file says we have it, but there is no pdf in the zip file
2059  # same
Empty subfolders:
PREC0064 # this zip file seems to be empty
2082 # excel says yes, but zip file is empty
0089CONV # not in excel file, zip file emptu
2077 # ok this is correct, excel file says no
GFPX28 # zip file is emptu
490 # excel says yes, zip file is empty 
1871 # excel says yes, zip file is empty
785 # excel says yes, zip file is empty
2089 # excel says yes, zip file is empty
WDT2496 this zip file seems to be empty
2162 # excel says yes, zip file is empty
643AL # excel says yes, zip file is empty
PREC0366 this zip file seems to be empty
WDT0003  this zip file seems to be empty



Total number of subfolders processed: 1310
Total number of non-empty subfolders: 1299
Total number of subfolders with only PDFs: 1295
Subfolders without any PDFs:
417
zOEpxM
2058
2059
Empty subfolders:
PREC0064
2082
0089CONV
2077
GFPX28
490
785
WDT2496
643AL
PREC0366
WDT0003
Cleanup and copy process completed.
 '''


'''
import os
import shutil

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper"
os.chdir(new_directory)

def clean_subfolders(source_folder, destination_folder):
    """
    Cleans subfolders by removing empty ones and keeping only the ones that contain PDFs.
    Copies cleaned subfolders to a destination folder.

    Parameters:
    source_folder (str): The source folder containing subfolders.
    destination_folder (str): The folder to store the cleaned subfolders.
    """
    total_subfolders = 0
    non_empty_subfolders = 0
    subfolders_with_only_pdfs = 0
    subfolders_without_pdfs = []  # List to store names of subfolders without PDFs
    empty_folders = []  # List to store names of empty folders

    # Create the destination folder if it doesn't exist
    if not os.path.exists(destination_folder):
        os.makedirs(destination_folder)

    # Debugging: Print out all the subfolders in the source directory
    print(f"Contents of {source_folder}:")
    for subfolder in os.listdir(source_folder):
        print(subfolder)  # Print every folder and file name for inspection

    # Iterate through all top-level subfolders
    for subfolder in os.listdir(source_folder):
        subfolder_path = os.path.join(source_folder, subfolder)

        if os.path.isdir(subfolder_path):
            total_subfolders += 1
            files_in_subfolder = os.listdir(subfolder_path)

            # Remove empty subfolders
            if not files_in_subfolder:
                print(f"Removing empty subfolder: {subfolder_path}")
                empty_folders.append(subfolder)  # Add to the list of empty folders
                continue

            non_empty_subfolders += 1

            # Check if all files in the subfolder are PDFs
            pdf_files = [f for f in files_in_subfolder if f.endswith('.pdf')]
            non_pdf_files = [f for f in files_in_subfolder if not f.endswith('.pdf')]

            if non_pdf_files:
                print(f"Subfolder {subfolder_path} contains non-PDF files. Removing non-PDF files...")

            # If it has PDFs, copy to the destination folder and keep only PDFs
            if pdf_files:
                subfolders_with_only_pdfs += 1
                new_subfolder_path = os.path.join(destination_folder, subfolder)
                if not os.path.exists(new_subfolder_path):
                    os.makedirs(new_subfolder_path)

                for pdf_file in pdf_files:
                    source_file = os.path.join(subfolder_path, pdf_file)
                    destination_file = os.path.join(new_subfolder_path, pdf_file)
                    shutil.copy2(source_file, destination_file)
                    print(f"Copied PDF file: {source_file} to {destination_file}")
            else:
                print(f"Subfolder {subfolder_path} contains no PDFs and will be ignored.")
                subfolders_without_pdfs.append(subfolder)  # Add subfolder to the list
    
    # Print summary
    print(f"Total number of subfolders processed: {total_subfolders}")
    print(f"Total number of non-empty subfolders: {non_empty_subfolders}")
    print(f"Total number of subfolders with only PDFs: {subfolders_with_only_pdfs}")
    
    # Print the names of subfolders without PDFs
    if subfolders_without_pdfs:
        print(f"Subfolders without any PDFs:")
        for subfolder in subfolders_without_pdfs:
            print(subfolder)
    
    # Print the names of empty folders
    if empty_folders:
        print(f"Empty subfolders:")
        for subfolder in empty_folders:
            print(subfolder)

if __name__ == "__main__":
    source_folder = "unzipped"  # Replace with your source folder path containing subfolders
    destination_folder = "clean_pdfs"  # Replace with the destination folder path

    clean_subfolders(source_folder, destination_folder)
    print("Cleanup and copy process completed.")
'''






import os
import shutil

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper"
os.chdir(new_directory)

def clean_subfolders(source_folder, destination_folder):
    """
    Cleans subfolders by removing empty ones and keeping only the ones that contain PDFs.
    Copies cleaned subfolders to a destination folder.

    Parameters:
    source_folder (str): The source folder containing subfolders.
    destination_folder (str): The folder to store the cleaned subfolders.
    """
    total_subfolders = 0
    non_empty_subfolders = 0
    subfolders_with_only_pdfs = 0
    subfolders_without_pdfs = []  # List to store names of subfolders without PDFs
    empty_folders = []  # List to store names of empty folders

    # Create the destination folder if it doesn't exist
    if not os.path.exists(destination_folder):
        os.makedirs(destination_folder)

    # Walk through the entire directory structure recursively
    for root, dirs, files in os.walk(source_folder):
        if files:
            total_subfolders += 1
            pdf_files = [f for f in files if f.endswith('.pdf')]
            non_pdf_files = [f for f in files if not f.endswith('.pdf')]

            # If the folder contains PDFs, copy them to the destination folder
            if pdf_files:
                subfolders_with_only_pdfs += 1
                relative_path = os.path.relpath(root, source_folder)
                new_subfolder_path = os.path.join(destination_folder, relative_path)

                if not os.path.exists(new_subfolder_path):
                    os.makedirs(new_subfolder_path)

                for pdf_file in pdf_files:
                    source_file = os.path.join(root, pdf_file)
                    destination_file = os.path.join(new_subfolder_path, pdf_file)
                    shutil.copy2(source_file, destination_file)
                    print(f"Copied PDF file: {source_file} to {destination_file}")
            
            # If the folder has non-PDF files, print a message
            if non_pdf_files:
                print(f"Subfolder {root} contains non-PDF files, but only PDFs will be copied.")
            
            # If there are no PDFs in this subfolder
            if not pdf_files:
                subfolders_without_pdfs.append(root)

        # If the folder is empty, add to the empty_folders list
        if not files and not dirs:
            empty_folders.append(root)

    # Print summary
    print(f"Total number of subfolders processed: {total_subfolders}")
    print(f"Total number of subfolders with only PDFs: {subfolders_with_only_pdfs}")
    
    # Print the names of subfolders without PDFs
    if subfolders_without_pdfs:
        print(f"Subfolders without any PDFs:")
        for subfolder in subfolders_without_pdfs:
            print(subfolder)
    
    # Print the names of empty folders
    if empty_folders:
        print(f"Empty subfolders:")
        for subfolder in empty_folders:
            print(subfolder)

if __name__ == "__main__":
    source_folder = "unzipped"  # Replace with your source folder path containing subfolders
    destination_folder = "data"  # Replace with the destination folder path

    clean_subfolders(source_folder, destination_folder)
    print("Cleanup and copy process completed.")





'''    



import os
import shutil

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper"
os.chdir(new_directory)

def clean_subfolders(source_folder):
    """
    Cleans subfolders by removing empty ones and keeping only the ones that contain PDFs.
    The original source folder's structure is modified in place.

    Parameters:
    source_folder (str): The source folder containing subfolders to clean.
    """
    total_subfolders = 0
    non_empty_subfolders = 0
    subfolders_with_only_pdfs = 0
    subfolders_without_pdfs = []  # List to store names of subfolders without PDFs
    empty_folders = []  # List to store names of empty folders

    # Walk through the entire directory structure recursively
    for root, dirs, files in os.walk(source_folder):
        if files:
            total_subfolders += 1
            pdf_files = [f for f in files if f.endswith('.pdf')]
            non_pdf_files = [f for f in files if not f.endswith('.pdf')]

            # If the folder contains non-PDF files, remove them
            for non_pdf_file in non_pdf_files:
                non_pdf_file_path = os.path.join(root, non_pdf_file)
                os.remove(non_pdf_file_path)
                print(f"Removed non-PDF file: {non_pdf_file_path}")

            # If the folder contains no PDFs, mark it for deletion later
            if not pdf_files:
                subfolders_without_pdfs.append(root)
            else:
                subfolders_with_only_pdfs += 1
                print(f"Subfolder {root} contains PDFs only.")

        # If the folder is empty (no files and no subdirectories), mark it for deletion
        if not files and not dirs:
            empty_folders.append(root)

    # Remove subfolders that contain no PDFs
    for subfolder in subfolders_without_pdfs:
        if os.path.exists(subfolder):
            shutil.rmtree(subfolder)
            print(f"Removed subfolder without PDFs: {subfolder}")

    # Remove any empty folders
    for subfolder in empty_folders:
        if os.path.exists(subfolder):
            shutil.rmtree(subfolder)
            print(f"Removed empty folder: {subfolder}")

    # Print summary
    print(f"Total number of subfolders processed: {total_subfolders}")
    print(f"Total number of subfolders with only PDFs: {subfolders_with_only_pdfs}")
    
    # Print the names of subfolders without PDFs
    if subfolders_without_pdfs:
        print(f"Subfolders without any PDFs were deleted.")

    # Print the names of empty folders
    if empty_folders:
        print(f"Empty subfolders were deleted.")

if __name__ == "__main__":
    source_folder = "unzipped"  # Replace with your source folder path

    clean_subfolders(source_folder)
    print("Cleanup process completed.")

'''