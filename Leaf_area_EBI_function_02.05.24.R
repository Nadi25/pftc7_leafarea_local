
# Function to calculate leaf area with EBImage and autothreshold method -----------------------------------

# load library
library(EBImage)
library(purrr)

# function to calculate leaf area
leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Process each image in the folder
  leaf_areas <- map_dbl(list.of.files, function(file_path) {
    # Read the image
    img <- readImage(file_path)

    # Get the file name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Define the output file path for the segmented image with original name
    output_path <- file.path(output_folder, paste0(file_name, ".jpeg"))

    # Get the dimensions of the image
    img_dims <- dim(img)

    # Define the cropping dimensions
    crop_top <- 10
    crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
    crop_left <- 1750
    crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

    # Crop the image
    img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, , drop = FALSE]  # Include 'drop = FALSE' to handle multiple frames

    # Convert the image to grayscale
    blue_img <- channel(img_crop, "blue")

    # Set threshold using Otsu's method
    threshold <- otsu(blue_img)
    threshold_img <- blue_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)
    # by using ! the background is black and the area of the leaf can be calculated instead of the area of the background minus the leaf

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    writeImage(threshold_img, output_path) # save the image with original name

    # Return the leaf area in mm^2
    return(leaf_area_mm2)
  })

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), Leaf_Area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas
  return(leaf_areas_df)
}

# # # Usage example:
# file_path <- "test/"
# output_folder <- "test/output/"
# leaf_area <- leaf_area_EBImage(file_path, output_folder)
# print(leaf_area)

