
library(EBImage)
library(purrr)

# Function to prepare images (crop) and return list of prepared file paths
prepare_images <- function(folder_path, output_folder_prep) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg", full.names = TRUE)

  # Create the output folder if it doesn't exist
  dir.create(output_folder_prep, showWarnings = FALSE)

  # Process each image in the folder
  prepared_files <- lapply(list.of.files, function(file_path) {
    # Read the image
    img <- readImage(file_path)

    # Get the file name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Define the output file path for the cropped image with original name
    output_path <- file.path(output_folder_prep, paste0(file_name, ".jpeg"))

    # Get the dimensions of the image
    img_dims <- dim(img)

    # Define the cropping dimensions
    crop_top <- 10
    crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
    crop_left <- 1750
    crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

    # Crop the image
    img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, , drop = FALSE]  # Include 'drop = FALSE' to handle multiple frames

    # Save the cropped image
    writeImage(img_crop, output_path) # save the image with original name

    # Return the path of the prepared image
    return(output_path)
  })

  # Return the list of prepared file paths
  return(prepared_files)
}

# Function to process cropped images and calculate leaf area
process_image_leaf_area <- function(list_of_files, output_folder_proc){
  # Initialize an empty data frame to store results
  results_df <- data.frame(Image = character(), leaf_area_mm2 = numeric())

  # Process each cropped image
  map(list_of_files, ~ {
    # Read the cropped image
    img <- readImage(.x)

    # Convert the image to grayscale
    blue_img <- channel(img, "blue")

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using autothresh method
    threshold <- autothresholdr::auto_thresh(data, method = "Moments") / 256
    threshold_img <- blue_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    output_path <- file.path(output_folder_proc, basename(.x))
    writeImage(threshold_img, output_path) # save the image with original name

    # Add results to the data frame
    results_df <<- rbind(results_df, data.frame(Image = basename(.x), leaf_area_mm2 = leaf_area_mm2))

  # Print leaf area
  cat("Leaf area for", basename(.x), ":", leaf_area_mm2, "mm^2\n")
  })

# Write results to a CSV file
output_csv <- file.path(output_folder_proc, "leaf_areas_mm2.csv")
write.csv(results_df, file = output_csv, row.names = FALSE)

# Return the results data frame
return(results_df)
}

# Usage:
folder_path <- "test/"
output_folder_prep <- "test/prepared_images/"
output_folder_proc <- "test/processed_images/"

# Prepare images (crop) and get list of prepared file paths
list_of_files <- prepare_images(folder_path, output_folder_prep)

# Process cropped images (calculate leaf area)
results_df <- process_image_leaf_area(list_of_files, output_folder_proc)

# Read the CSV file containing results
leaf_area_test <- read.csv(file.path(output_folder_proc, "leaf_areas_mm2.csv"))
print(leaf_area_test)





