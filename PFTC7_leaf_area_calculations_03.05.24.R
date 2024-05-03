
# PFTC7 leaf area
# Get leaf area from different folders

source("Leaf_area_EBI_function_02.05.24.R")

# Now you can use the leaf_area_EBImage function as before
folder_path <- "raw_scans/6/"
output_folder <- "output/output_EBI/6/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2_6 <- read.csv("output/output_EBI/6/leaf_areas_mm2.csv")
leaf_areas_mm2_6
