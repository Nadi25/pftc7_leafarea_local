

# load library ------------------------------------------------------------
library(EBImage)
library(purrr)


# function to calculate leaf area from a folder of jpeg images ------------

leaf_area_calculation <- function(folder_path, output_folder, method = "Otsu") {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg", full.names = TRUE)

  # Process each image in the folder
  leaf_areas <- map(list.of.files, ~ {
    # Read the image
    img <- readImage(.x)

    # crop image
    img_crop <- crop_image(img = img)

    # process image to get leaf area
    process_image_leaf_area(img = img_crop, output_folder = output_folder, img_file = .x, method = method)

  }) |>
    list_rbind()

  # Return the results of leaf area
  leaf_areas
}


# function to crop images -------------------------------------------------

crop_image <- function(img, crop_top = 20, crop_bottom = 2440,
                       crop_left = 1588, crop_right = 3513){
  # Get the dimensions of the image
  img_dims <- dim(img)

  # Define the cropping dimensions
  crop_top <- crop_top
  crop_bottom <- min(crop_bottom, img_dims[1])  # Ensure the bottom crop does not exceed image height
  crop_left <- crop_left
  crop_right <- min(crop_right, img_dims[2])  # Ensure the right crop does not exceed image width

  # Crop the image
  img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, , drop = FALSE ]

}

# function to calculate leaf area ----------------------------------------------
# method can be chosen
# exclude small particles

process_image_leaf_area <- function(img, method = "Otsu", dpi = 300, min_area_mm2 = 1, output_folder, img_file) {

  # Process the cropped image

    # Convert the image to blue scale
    blue_img <- channel(img, "blue")

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using auto thresh method
    threshold <- autothresholdr::auto_thresh(data, method = method) / 256
    threshold_img <- blue_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)

    # Calculate leaf area in pixels
    leaf_areas <- computeFeatures.shape(segmented_img)[, "s.area"]

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- dpi
    leaf_areas_mm2 <- (leaf_areas / dpi^2) * 25.4^2

    # Filter out small particles
    filtered_leaf_areas_mm2 <- leaf_areas_mm2[leaf_areas_mm2 >= min_area_mm2]

    # Count the number of patches detected (particles larger than min_area_mm2)
    num_patches <- length(filtered_leaf_areas_mm2)

    # Calculate total leaf area
    total_leaf_area_mm2 <- sum(filtered_leaf_areas_mm2)

    # convert to cm2 and round to 3 digits
    total_leaf_area_cm2 <- round(total_leaf_area_mm2/100, 3)

    # Save the segmented image
    output_path <- file.path(output_folder, basename(img_file)) # save the image with original name
    writeImage(threshold_img, output_path)

    # Add results to the data frame
    data.frame(Image = basename(img_file),
               num_patches = num_patches,
               total_leaf_area_mm2 = total_leaf_area_mm2,
               total_leaf_area_cm2 = total_leaf_area_cm2)

}


