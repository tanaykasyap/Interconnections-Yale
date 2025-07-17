#this code unzips the files into subfolders, with the subfolder name being the queue number combo
'''
import os
import zipfile
import shutil
import patoolib

new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper"
os.chdir(new_directory)

#Outer ZIP files processed: 1778
#nner ZIP files processed: 3099
#Total ZIP files processed: 4877


#With data provided by michael: 
#Outer ZIP files processed: 1246 ( this should have been 1280 from the excel sheet, michael provided 1267, however there are like 15 files which end with zip.crdownload, th)
#Inner ZIP files processed: 5184
#Total ZIP files processed: 6430

# Counters to track ZIP files
outer_zip_count = 0
inner_zip_count = 0
total_zip_count = 0

def unzip_file(zip_file_path, output_dir, processed_files, outer_zip_name=None):
    global outer_zip_count, inner_zip_count, total_zip_count

    try:
        with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
            for file in zip_ref.namelist():
                file_path = os.path.join(output_dir, file)

                # If the file already exists, we do not replace it
                if os.path.exists(file_path):
                    print(f"Skipping existing file: {file_path}")
                else:
                    print(f"Adding new file: {file_path}")
                    zip_ref.extract(file, output_dir)

            #print(f"Unzipped {zip_file_path} to {output_dir}")
            processed_files.append(zip_file_path)
            total_zip_count += 1

            if not outer_zip_name:
                outer_zip_count += 1
                outer_zip_name = os.path.basename(zip_file_path).split('.')[0]
            else:
                inner_zip_count += 1

            # Process inner ZIP files
            for file in zip_ref.namelist():
                inner_file_path = os.path.join(output_dir, file)
                if inner_file_path.endswith('.zip'):
                    # Extract inner zip into the same output folder as the outer zip
                    unzip_file(inner_file_path, output_dir, processed_files, outer_zip_name)

    except zipfile.BadZipFile:
        print(f"Error: {zip_file_path} is not a valid zip file. Trying patoolib...")
        try:
            patoolib.extract_archive(zip_file_path, outdir=output_dir)
            print(f"Unzipped {zip_file_path} with patoolib to {output_dir}")
            processed_files.append(zip_file_path)
            total_zip_count += 1

            if not outer_zip_name:
                outer_zip_count += 1
                outer_zip_name = os.path.basename(zip_file_path).split('.')[0]
            else:
                inner_zip_count += 1

        except Exception as e:
            print(f"Error: {zip_file_path} could not be processed even with patoolib. {str(e)}")

    except UnicodeDecodeError:
        print(f"Error: {zip_file_path} contains invalid UTF-8 data and cannot be processed.")
    except Exception as e:
        print(f"An unexpected error occurred while processing {zip_file_path}: {str(e)}")


def process_zip_files(zip_folder, output_root):
    processed_files = []

    # Loop through all files in the directory
    for file_name in os.listdir(zip_folder):
        if file_name.endswith('.zip'):
            zip_file_path = os.path.join(zip_folder, file_name)

            # Extract the subfolder name from the pattern between _ and _
            subfolder_name = file_name.split('_')[1] if len(file_name.split('_')) > 1 else file_name.split('.')[0]
            output_subfolder = os.path.join(output_root, subfolder_name)

            # Create the output subfolder if it doesn't exist
            if not os.path.exists(output_subfolder):
                os.makedirs(output_subfolder)

            # Unzip the file
            unzip_file(zip_file_path, output_subfolder, processed_files)

    print(f"Processed {len(processed_files)} ZIP files in total.")
    print(f"Outer ZIP files processed: {outer_zip_count}")
    print(f"Inner ZIP files processed: {inner_zip_count}")
    print(f"Total ZIP files processed: {total_zip_count}")


if __name__ == "__main__":
    zip_folder = "MichaelData/pdfs"  # Replace with the folder where your ZIP files are located
    output_root = "unzipped2"  # Replace with the output folder path

    # Start processing
    process_zip_files(zip_folder, output_root)
'''

import os
import zipfile
import shutil
import patoolib


'''
 === Processing Summary ===
Total ZIP files accessed: 1251
Total ZIP files processed successfully: 6444
 - Outer ZIP files processed: 1246
 - Inner ZIP files processed: 5198
Total ZIP files failed to unzip: 29
 - Failed outer ZIP files: 5
 - Failed inner ZIP files: 24
Duplicate files skipped (outer ZIPs only): 38445

List of Failed Outer ZIP Files:
 - MichaelData/pdfs/DownloadDocs_1871_MOFODILEX27074.zip  # our excel file suggests this has a phase 1 study
 - MichaelData/pdfs/DownloadDocs_2089_MOFODILEX27074.zip  # same
 - MichaelData/pdfs/DownloadDocs_2162_MOFODILEX27074.zip  # same
 - MichaelData/pdfs/DownloadDocs_2077_MOFODILEX27074.zip  # ok this is not in our list 
 - MichaelData/pdfs/DownloadDocs_2110_MOFODILEX27074.zip  # this is in our excel file.

List of Failed Inner ZIP Files:
 - unzipped2/1170/15AS885461-QC8PhII_Q1170_Gateway_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1347/16AS0139-Q1347C9P1Crescent_Peak_Wind_Project__Revision01.zip
 - unzipped2/1165/15AS882813-QC8PhII_Q1165_Bancroft/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1749/P2RPT-Q1749Whale_Rock_EnergyC13PhII.zip
 - unzipped2/1175/15AS882626-QC8PhII_Q1175_La_Conte_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1341/16AS0055-Q1341C9PH1Yellow_Pine_2_Project__Revision01.zip  #  this is in our unzipped folder, suspicious
 - unzipped2/1739/P2RPT-Q1739_Pecho_Energy_Storage_C13PhII.zip
 - unzipped2/1166/15AS882546-QC8PhII_Q1166_Big_Rock_Solar_Farm/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1688/P2RPT-Q1688_Crossroads_C13PhII.zip
 - unzipped2/1718/P2RPT-Q1718_Gonzaga_Hybrid_C13PhII.zip
 - unzipped2/1191/15AS883200-QC8PhII_Q1191_Valley_Center_Renewable/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1695/P2RPT-Q1695_Meadows_Energy_StorageC13PhII.zip
 - unzipped2/1225/P2RPT-Q1225_Cinco_C13PhII_OneTime_OPDS_C9PhII.zip
 - unzipped2/1728/P2RPT-Q1728_Zeta_C13PhII.zip
 - unzipped2/1174/15AS881376-QC8PhII_Q1174_Jacumba_Ranch/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1736/P2RPT-Q1736_Hawkins_Solar_Hybrid_C13PhII.zip
 - unzipped2/1169/15AS882736-QC8PhII_Q1169_Fallbrook_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1189/15AS883123-QC8PhII_Q1189_Sun_Streams_Solar/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1308/16AS0082-C9PIQ1308_NOL_Barstow_Solar.zip # already had in our subfolder but was empty, so manually placed it.
 - unzipped2/1733/P2RPT-Q1733_Bluff_Trail_C13PhII.zip
 - unzipped2/1171/15AS881633-QC8PhII_Q1171_McFarland_Solar/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1740/P2RPT-Q1740_Puff_Hybrid_Solar_C13PhII.zip
 - unzipped2/1187/15AS880547-QC8PhII_Q1187_Stingray_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped2/1345/16AS0028-Q1345C9PH1South_Ridge_Project__Revision01.zip
'''


'''
Total ZIP files accessed: 1314
Total ZIP files processed successfully: 6799
 - Outer ZIP files processed: 1310
 - Inner ZIP files processed: 5489
Total ZIP files failed to unzip: 35
 - Failed outer ZIP files: 4
 - Failed inner ZIP files: 31
Duplicate files skipped (outer ZIPs only): 0

List of Failed Outer ZIP Files:
 - MichaelData/newpdfs/DownloadDocs_1871_MOFODILEX27074.zip
 - MichaelData/newpdfs/DownloadDocs_2089_MOFODILEX27074.zip
 - MichaelData/newpdfs/DownloadDocs_2162_MOFODILEX27074.zip
 - MichaelData/newpdfs/DownloadDocs_2077_MOFODILEX27074.zip

List of Failed Inner ZIP Files:
 - unzipped/1170/15AS885461-QC8PhII_Q1170_Gateway_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1287/16AS0141-QC9PhI_Q1287_Mt_Laguna_Wind/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1347/16AS0139-Q1347C9P1Crescent_Peak_Wind_Project__Revision01.zip
 - unzipped/1294/16AS0100-QC9PhI_Q1294_Vista_Energy_Storage_2/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1165/15AS882813-QC8PhII_Q1165_Bancroft/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1749/P2RPT-Q1749Whale_Rock_EnergyC13PhII.zip
 - unzipped/1292/16AS0153-QC9PhI_Q1292_Otaybat/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1175/15AS882626-QC8PhII_Q1175_La_Conte_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1341/16AS0055-Q1341C9PH1Yellow_Pine_2_Project__Revision01.zip
 - unzipped/1739/P2RPT-Q1739_Pecho_Energy_Storage_C13PhII.zip
 - unzipped/1166/15AS882546-QC8PhII_Q1166_Big_Rock_Solar_Farm/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1688/P2RPT-Q1688_Crossroads_C13PhII.zip
 - unzipped/1718/P2RPT-Q1718_Gonzaga_Hybrid_C13PhII.zip
 - unzipped/1191/15AS883200-QC8PhII_Q1191_Valley_Center_Renewable/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1695/P2RPT-Q1695_Meadows_Energy_StorageC13PhII.zip
 - unzipped/1225/P2RPT-Q1225_Cinco_C13PhII_OneTime_OPDS_C9PhII.zip
 - unzipped/1728/P2RPT-Q1728_Zeta_C13PhII.zip
 - unzipped/1293/16AS0083-QC9PhI_Q1293_Signal_Peak/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1174/15AS881376-QC8PhII_Q1174_Jacumba_Ranch/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1736/P2RPT-Q1736_Hawkins_Solar_Hybrid_C13PhII.zip
 - unzipped/1169/15AS882736-QC8PhII_Q1169_Fallbrook_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1189/15AS883123-QC8PhII_Q1189_Sun_Streams_Solar/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1308/16AS0082-C9PIQ1308_NOL_Barstow_Solar.zip
 - unzipped/1285/16AS0042-QC9PhI_Q1285_Blue_Wing_Ranch_Solar/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1288/16AS0030-QC9PhI_Q1288_Red_Wing_Ranch_Solar/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1733/P2RPT-Q1733_Bluff_Trail_C13PhII.zip
 - unzipped/1286/16AS0023-QC9PhI_Q1286_Gigantes_Solar/QC9PhI_SDGE Area Report and Appendices B-K.zip
 - unzipped/1171/15AS881633-QC8PhII_Q1171_McFarland_Solar/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1740/P2RPT-Q1740_Puff_Hybrid_Solar_C13PhII.zip
 - unzipped/1187/15AS880547-QC8PhII_Q1187_Stingray_Energy_Storage/C8PhII_SDGE Area Report and Appendices B-K.zip
 - unzipped/1345/16AS0028-Q1345C9PH1South_Ridge_Project__Revision01.zip

Processed ZIP files details:
 - Total processed ZIPs (including inner): 6799

'''



# Change to the desired directory
new_directory = "/Users/vk365/Dropbox/Interconnections_data/data/pdf_scraper"
os.chdir(new_directory)

# Initialize counters and lists
outer_zip_count = 0
inner_zip_count = 0
total_zip_count = 0
accessed_files = 0
failed_files = 0
duplicate_skips = 0

# Lists to track failed zips
failed_outer_zips = []
failed_inner_zips = []

def unzip_file(zip_file_path, output_dir, processed_files, outer_zip_name=None):
    global outer_zip_count, inner_zip_count, total_zip_count, failed_files, duplicate_skips

    try:
        with zipfile.ZipFile(zip_file_path, 'r') as zip_ref:
            print(f"Processing {'Outer' if outer_zip_name is None else 'Inner'} ZIP: {zip_file_path}")

            for file in zip_ref.namelist():
                file_path = os.path.join(output_dir, file)

                # Handle directories and empty subfolders
                if file.endswith('/'):  # This is a directory
                    if not os.path.exists(file_path):
                        os.makedirs(file_path)
                        print(f"Created subfolder: {file_path}")
                else:
                    # Handle files
                    if os.path.exists(file_path):
                        # If the file already exists, check if it is empty
                        if os.path.getsize(file_path) == 0:
                            print(f"Replacing empty file: {file_path}")
                            zip_ref.extract(file, output_dir)
                        else:
                            print(f"File already exists and is not empty: {file_path}")
                    else:
                        # File doesn't exist, extract it
                        print(f"Adding new file: {file_path}")
                        zip_ref.extract(file, output_dir)

            # Successfully processed this ZIP file
            processed_files.append(zip_file_path)
            total_zip_count += 1

            # Determine if it's an outer or inner ZIP
            if not outer_zip_name:
                outer_zip_count += 1
                outer_zip_name = os.path.basename(zip_file_path).split('.')[0]
            else:
                inner_zip_count += 1

            # Process inner ZIP files recursively
            for file in zip_ref.namelist():
                inner_file_path = os.path.join(output_dir, file)
                if inner_file_path.endswith('.zip'):
                    inner_subfolder = os.path.splitext(os.path.basename(inner_file_path))[0]
                    inner_output_dir = os.path.join(output_dir, inner_subfolder)

                    if not os.path.exists(inner_output_dir):
                        os.makedirs(inner_output_dir)
                    unzip_file(inner_file_path, inner_output_dir, processed_files, outer_zip_name)

    except zipfile.BadZipFile:
        print(f"Error: {zip_file_path} is not a valid zip file.")
        failed_files += 1
        if outer_zip_name is None:
            failed_outer_zips.append(zip_file_path)
        else:
            failed_inner_zips.append(zip_file_path)

    except UnicodeDecodeError:
        print(f"Error: {zip_file_path} contains invalid UTF-8 data and cannot be processed.")
        failed_files += 1
        if outer_zip_name is None:
            failed_outer_zips.append(zip_file_path)
        else:
            failed_inner_zips.append(zip_file_path)

    except Exception as e:
        print(f"An unexpected error occurred while processing {zip_file_path}: {str(e)}")
        failed_files += 1
        if outer_zip_name is None:
            failed_outer_zips.append(zip_file_path)
        else:
            failed_inner_zips.append(zip_file_path)

def process_zip_files(zip_folder, output_root):
    global accessed_files
    processed_files = []

    # Loop through all files in the directory
    for file_name in os.listdir(zip_folder):
        if file_name.endswith('.zip'):
            accessed_files += 1  # Increment accessed files counter
            zip_file_path = os.path.join(zip_folder, file_name)

            print(f"\nStarting to process outer ZIP: {zip_file_path}")

            # Extract the subfolder name from the pattern between _ and _
            subfolder_name = file_name.split('_')[1] if len(file_name.split('_')) > 1 else os.path.splitext(file_name)[0]
            output_subfolder = os.path.join(output_root, subfolder_name)

            # Create the output subfolder if it doesn't exist
            if not os.path.exists(output_subfolder):
                os.makedirs(output_subfolder)
                print(f"Created subfolder: {output_subfolder}")
            else:
                print(f"Subfolder already exists: {output_subfolder}")

            # Unzip the file
            unzip_file(zip_file_path, output_subfolder, processed_files)

    # Summary of processing
    print("\n=== Processing Summary ===")
    print(f"Total ZIP files accessed: {accessed_files}")
    print(f"Total ZIP files processed successfully: {total_zip_count}")
    print(f" - Outer ZIP files processed: {outer_zip_count}")
    print(f" - Inner ZIP files processed: {inner_zip_count}")
    print(f"Total ZIP files failed to unzip: {failed_files}")
    print(f" - Failed outer ZIP files: {len(failed_outer_zips)}")
    print(f" - Failed inner ZIP files: {len(failed_inner_zips)}")
    print(f"Duplicate files skipped (outer ZIPs only): {duplicate_skips}")

    if failed_outer_zips:
        print("\nList of Failed Outer ZIP Files:")
        for zip_path in failed_outer_zips:
            print(f" - {zip_path}")

    if failed_inner_zips:
        print("\nList of Failed Inner ZIP Files:")
        for zip_path in failed_inner_zips:
            print(f" - {zip_path}")

    print("\nProcessed ZIP files details:")
    print(f" - Total processed ZIPs (including inner): {len(processed_files)}")

if __name__ == "__main__":
    zip_folder = "RawData/newpdfs"  # Replace with the folder where your ZIP files are located
    output_root = "unzipped"        # Replace with the output folder path

    # Start processing
    process_zip_files(zip_folder, output_root)

