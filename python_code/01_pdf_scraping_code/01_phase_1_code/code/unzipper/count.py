#just a simple code to count the number of projects
#  


'''
import os

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper"
os.chdir(new_directory)
def count_subfolders(folder_path):
    """
    Counts the number of subfolders in the given folder.

    Parameters:
    folder_path (str): The path to the folder where you want to count subfolders.

    Returns:
    int: The total number of subfolders.
    """
    subfolder_count = 0

    # Loop through items in the folder and check if they are directories (subfolders)
    for item in os.listdir(folder_path):
        item_path = os.path.join(folder_path, item)
        if os.path.isdir(item_path):  # Check if the item is a directory
            subfolder_count += 1

    return subfolder_count

if __name__ == "__main__":
    folder_path = "data"  # Replace with your folder path
    total_subfolders = count_subfolders(folder_path)
    print(f"Total number of subfolders: {total_subfolders}")

'''

import os

def count_non_empty_phase_1_study_folders(data_dir):
    non_empty_count = 0

    # Iterate through each project folder in the data directory
    for project_folder in os.listdir(data_dir):
        project_path = os.path.join(data_dir, project_folder)
        
        # Check if it's a directory
        if os.path.isdir(project_path):
            # Define the path for the '02_phase_1_study' subfolder
            phase_1_study_path = os.path.join(project_path, '02_phase_1_study')
            
            # Check if '02_phase_1_study' exists and is non-empty
            if os.path.exists(phase_1_study_path) and os.listdir(phase_1_study_path):
                non_empty_count += 1

    return non_empty_count

if __name__ == '__main__':
    data_dir = "/Users/vk365/Dropbox/interconnections_data/data/pdf_scraper/data"  # Replace with your actual data folder path
    non_empty_folders = count_non_empty_phase_1_study_folders(data_dir)
    print(f"Total number of non-empty '02_phase_1_study' folders: {non_empty_folders}")
