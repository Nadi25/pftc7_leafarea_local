
# PFTC7 leaf area
# Get leaf area from different folders

source("PFTC7_functions_image_prep_and_process_09.05.24.R")

folder_path <- "test/"
output_folder <- "test/output_otsu_new/"
leaf_areas <- leaf_area_calculation(folder_path = folder_path, output_folder = output_folder)
leaf_areas


# folder 5 ----------------------------------------------------------------

folder_path <- "raw_scans/5/"
output_folder <- "output/output_leaf_area/5/"
leaf_areas <- leaf_area_calculation(folder_path = folder_path, output_folder = output_folder)
leaf_areas
