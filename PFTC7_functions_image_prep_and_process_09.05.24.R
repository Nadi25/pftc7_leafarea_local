
library(EBImage)
library(purrr)


prep_image <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg", full.names = TRUE)

  # Process each image in the folder
  list_prep_images <- map(list.of.files, ~ {
    # Read the image
    img <- readImage(.x)

    # Get the dimensions of the image
    img_dims <- dim(img)

    # Define the cropping dimensions
    crop_top <- 10
    crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
    crop_left <- 1750
    crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

    # Crop the image
    img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, , drop = FALSE ]

    # Define the output file path for the cropped image with original name
    output_path <- file.path(output_folder,basename(.x))

    # Save the cropped image (if needed)
    writeImage(img_crop, output_path) # save the image with original name

    # Return the path of the prepared image
    return(output_path)
  })

  # Return the list of prepared file paths
  return(list_prep_images)
}


process_image_leaf_area <- function(list_prep_images, output_folder) {
  # Initialize an empty data frame to store results
  results_df <- data.frame(Image = character(), leaf_area_mm2 = numeric())

  # Process each cropped image
  map(list_prep_images, ~ {
    # Read the cropped image
    img <- readImage(.x)

    # Convert the image to bluescale
    blue_img <- channel(img, "blue")

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using autothresh method
    threshold <- autothresholdr::auto_thresh(data, method = "Otsu") / 256
    threshold_img <- blue_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    output_path <- file.path(output_folder, basename(.x))
    writeImage(threshold_img, output_path) # save the image with original name

    # Add results to the data frame
    results_df <<- rbind(results_df, data.frame(Image = basename(.x), leaf_area_mm2 = leaf_area_mm2))

    # Print leaf area
    cat("Leaf area for", basename(.x), ":", leaf_area_mm2, "mm^2\n")
  })

  # Write results to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(results_df, file = output_csv, row.names = FALSE)

  # Return the results data frame
  return(results_df)
}


folder_path <- "test/"
output_folder <- "test/output_otsu_new/"
list_prep_images <- prep_image(folder_path, output_folder)
list_prep_images
result <- process_image_leaf_area(list_prep_images, output_folder)
result


