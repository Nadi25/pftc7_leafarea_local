
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

# Write the leaf areas to a CSV file
write.csv(leaf_areas, file = "raw_data/EBImage/PFTC7_leaf_area_5.csv", row.names = FALSE)

leaf_area_5 <- read.csv("raw_data/EBImage/PFTC7_leaf_area_5.csv")
leaf_area_5

# folder 5 minimum ----------------------------------------------------------------

folder_path <- "raw_scans/5/"
output_folder <- "output/output_leaf_area/5_minimum/"
leaf_areas <- leaf_area_calculation(folder_path = folder_path, output_folder = output_folder, method = "Minimum")
leaf_areas

# Write the leaf areas to a CSV file
write.csv(leaf_areas, file = "raw_data/EBImage/PFTC7_leaf_area_5_minimum.csv", row.names = FALSE)

leaf_area_5_minimum <- read.csv("raw_data/EBImage/PFTC7_leaf_area_5_minimum.csv")
leaf_area_5_minimum




# folder 6 ----------------------------------------------------------------

folder_path <- "raw_scans/6/"
output_folder <- "output/output_leaf_area/6/"
leaf_areas <- leaf_area_calculation(folder_path = folder_path, output_folder = output_folder)
leaf_areas

# Write the leaf areas to a CSV file
write.csv(leaf_areas, file = "raw_data/EBImage/PFTC7_leaf_area_6.csv", row.names = FALSE)

leaf_area_6 <- read.csv("raw_data/EBImage/PFTC7_leaf_area_6.csv")
leaf_area_6

# folder 6 minimum ----------------------------------------------------------------

folder_path <- "raw_scans/6/"
output_folder <- "output/output_leaf_area/6_minimum/"
leaf_areas <- leaf_area_calculation(folder_path = folder_path, output_folder = output_folder, method = "Minimum")
leaf_areas

# Write the leaf areas to a CSV file
write.csv(leaf_areas, file = "raw_data/EBImage/PFTC7_leaf_area_6_minimum.csv", row.names = FALSE)

leaf_area_6_minimum <- read.csv("raw_data/EBImage/PFTC7_leaf_area_6_minimum.csv")
leaf_area_6_minimum


# compare the methods -----------------------------------------------------

# Assuming you have data frames 'df_otsu' and 'df_minimum' containing the results from the Otsu and Minimum methods respectively.

# Merge the data frames for both methods based on the image filename
merged_df <- merge(leaf_area_6, leaf_area_6_minimum, by = "Image", suffixes = c("_otsu", "_minimum"))

# Calculate the difference in leaf area between the two methods for each image
merged_df$leaf_area_difference_cm2 <- merged_df$total_leaf_area_cm2_otsu - merged_df$total_leaf_area_cm2_minimum






