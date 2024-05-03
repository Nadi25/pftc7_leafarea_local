
# Calculate leaf areas with EBImage ---------------------------------------
# author: Nadine Arzt
# date: 29.04.24

# install.packages("BiocManager")
# BiocManager::install("EBImage")

# Load the EBImage library
library(EBImage)

# Read the image into R using readImage()
img <- readImage("Testing/DME0274.jpeg")
display(img)

# understand the dimensions of the image
dim(img)
# 2556 3513    3
# the 3 means: color image with three channels(RGB)
# cropping needs to be adjusted accordinlgy and can't just be 2
# first cut off ruler on the right side, width
# then cut off envelope from top
img_crop <- img[100:2400, 1550:3500, ]
display(img_crop)

# Change into a black and white image
grey_img <- channel(img_crop, "grey")
display(grey_img)

# set threshold manually
threshold_img <- grey_img > 0.6

segmented_img <- bwlabel(threshold_img)
display(segmented_img)

## s.area: area size (in pixels)
leaf_area <- computeFeatures.shape(segmented_img)[, "s.area"]

print(leaf_area)


## thresholding not manually
threshold <- otsu(grey_img)
threshold
# threshold_2 <- thresh(grey_img, method = "minimum")
# threshold_2 <- huang(grey_img)

image_thres <- combine( mapply(function(frame, th) frame > th, getFrames(grey_img), threshold, SIMPLIFY=FALSE) )

display(image_thres, all=TRUE)

segmented_img_2 <- bwlabel(image_thres)
display(segmented_img)

leaf_area_otsu <- computeFeatures.shape(segmented_img_2)[, "s.area"]

print(leaf_area)

## do we have to find out what the size of the smallest possible leaf is and then
## ignore the rest that is presumably dirt?


# # adaptive threshold ------------------------------------------------------
#
disc <- makeBrush(100, "disc")
disc <- disc / sum(disc)
offset <- 0.00005
grey_img_bg <- filter2( grey_img, disc )
grey_img_th <- grey_img > grey_img_bg + offset
display(grey_img_th, all=TRUE)
#
display( thresh(grey_img, w=15, h=15, offset=0.05), all=TRUE )
#
# ## not what we want, but looks like an old book print




# Get leaf area from a whole folder with images-----------------------------------

# Directory containing JPEG files
folder_path <- "test/"

# List JPEG files in the folder
list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

# Initialize a list to store leaf areas
leaf_areas <- numeric(length(list.of.files))

# Output folder for segmented images
output_folder <- "test/output"
dir.create(output_folder, showWarnings = FALSE)  # Create output folder if it doesn't exist

# Loop through each JPEG file
for (i in seq_along(list.of.files)) {
  # Read the image
  img <- readImage(list.of.files[i])

  # Get the dimensions of the image
  img_dims <- dim(img)

  # Define the cropping dimensions
  crop_top <- 100
  crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
  crop_left <- 1550
  crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

  # Crop the image
  img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, ]

  # Convert the image to grayscale
  grey_img <- channel(img_crop, "grey")

  # Set threshold manually or using an automatic method
  threshold_img <- grey_img > 0.68

  # Perform segmentation
  segmented_img <- bwlabel(threshold_img)

  # Calculate leaf area
  leaf_area <- computeFeatures.shape(segmented_img)[, "s.area"]

  # Store the leaf area
  leaf_areas[i] <- sum(leaf_area)

  # Save the segmented image with leaf area highlighted
  output_path <- file.path(output_folder, paste0("segmented_image_", i, ".jpeg"))
  writeImage(segmented_img, output_path)
}

print(leaf_areas)


# Get leaf area for whole folder using automated threshold otsu -----------

# Directory containing JPEG files
folder_path <- "test/"

# List JPEG files in the folder
list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

# Initialize a list to store leaf areas
leaf_areas <- numeric(length(list.of.files))

# Output folder for segmented images
output_folder <- "test/output"
dir.create(output_folder, showWarnings = FALSE)

# Loop through each JPEG file
for (i in seq_along(list.of.files)) {
  # Read the image
  img <- readImage(list.of.files[i])

  # Get the dimensions of the image
  img_dims <- dim(img)

  # Define the cropping dimensions
  crop_top <- 100
  crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
  crop_left <- 1550
  crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

  # Crop the image
  img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, ]

  # Convert the image to grayscale
  grey_img <- channel(img_crop, "grey")

  # Set threshold manually or using an automatic method
  threshold <- otsu(grey_img)
  threshold_img <- combine( mapply(function(frame, th) frame > th, getFrames(grey_img), threshold, SIMPLIFY=FALSE) )



  # Perform segmentation
  segmented_img <- bwlabel(threshold_img)

  # Calculate leaf area
  leaf_area <- computeFeatures.shape(segmented_img)[, "s.area"]

  # Store the leaf area
  leaf_areas[i] <- sum(leaf_area)

  # Save the segmented image with leaf area highlighted
  output_path <- file.path(output_folder, paste0("segmented_image_", i, ".jpeg"))
  writeImage(segmented_img, output_path)
}

print(leaf_areas)


# Folder 5 set manual threshold on 0.7 ----------------------------------------

library(EBImage)

# Directory containing JPEG files
folder_path <- "raw_scans/5/"

# List JPEG files in the folder
list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

# Initialize a list to store leaf areas
leaf_areas <- numeric(length(list.of.files))

# Output folder for segmented images
output_folder <- "output/output_EBI/5/"
dir.create(output_folder, showWarnings = FALSE)

# Loop through each JPEG file
for (file_path in list.of.files) {
  # Read the image
  img <- readImage(file_path)

  # Get the file name without the extension
  file_name <- tools::file_path_sans_ext(basename(file_path))

  # Define the output file path
  output_path <- file.path(output_folder, basename(file_path))

  # Get the dimensions of the image
  img_dims <- dim(img)

  # Define the cropping dimensions
  crop_top <- 10
  crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
  crop_left <- 1550
  crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

  # Crop the image
  img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, ]

  # Convert the image to grayscale
  grey_img <- channel(img_crop, "grey")

  # Set threshold manually or using an automatic method
  threshold_img <- grey_img > 0.7

  # Perform segmentation
  segmented_img <- bwlabel(threshold_img)

  # Calculate leaf area
  leaf_area <- computeFeatures.shape(segmented_img)[, "s.area"]

  # Store the leaf area
  leaf_areas[i] <- sum(leaf_area)

  # Save the segmented image
  writeImage(segmented_img, output_path)
}

print(leaf_areas)

## crop it a bit more on the left side


# Folder 5 set otsu autothreshold -------------------------------------------------

# Directory containing JPEG files
folder_path <- "raw_scans/5/"

# List JPEG files in the folder
list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

# Initialize a list to store leaf areas
leaf_areas <- numeric(length(list.of.files))

# Output folder for segmented images
output_folder <- "output/output_EBI/5_otsu/"
dir.create(output_folder, showWarnings = FALSE)

# Loop through each JPEG file
for (i in seq_along(list.of.files)) {
  # Read the image
  img <- readImage(list.of.files[i])

  # Get the file name without the extension
  file_name <- tools::file_path_sans_ext(basename(list.of.files[i]))

  # Define the output file path
  output_path <- file.path(output_folder, basename(list.of.files[i]))

  # Get the dimensions of the image
  img_dims <- dim(img)

  # Define the cropping dimensions
  crop_top <- 10
  crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
  crop_left <- 1550
  crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

  # Crop the image
  img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, ]

  # Convert the image to grayscale
  grey_img <- channel(img_crop, "grey")

  # Set threshold using Otsu's method
  threshold <- otsu(grey_img)
  threshold_img <- combine(mapply(function(frame, th) frame > th, getFrames(grey_img), threshold, SIMPLIFY=FALSE))

  # Perform segmentation
  segmented_img <- bwlabel(threshold_img)

  # Calculate leaf area
  leaf_area <- computeFeatures.shape(segmented_img)[, "s.area"]

  # Store the leaf area
  leaf_areas[i] <- sum(leaf_area)

  # Save the segmented image
  writeImage(segmented_img, output_path)
}

print(leaf_areas)

## save leaf area in same unit as leaf_area package did
## with the image name

## figure out what to do with the weird scans


# Initialize a data frame to store image file names and their corresponding leaf areas
output_data <- data.frame(Image = basename(list.of.files), Leaf_Area = leaf_areas)

# Define the output CSV file path
output_csv <- file.path(output_folder, "leaf_areas_5.csv")

# Write the data frame to a CSV file
write.csv(output_data, file = output_csv, row.names = FALSE)




# folder 6 ----------------------------------------------------------------

# Directory containing JPEG files
folder_path <- "raw_scans/6/"

# List JPEG files in the folder
list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

# Initialize a list to store leaf areas
leaf_areas <- numeric(length(list.of.files))

# Output folder for segmented images
output_folder <- "output/output_EBI/6_otsu/"
dir.create(output_folder, showWarnings = FALSE)  # Create output folder if it doesn't exist

# Loop through each JPEG file
for (i in seq_along(list.of.files)) {
  # Read the image
  img <- readImage(list.of.files[i])

  # Get the file name without the extension
  file_name <- tools::file_path_sans_ext(basename(list.of.files[i]))

  # Define the output file path
  output_path <- file.path(output_folder, basename(list.of.files[i]))

  # Get the dimensions of the image
  img_dims <- dim(img)

  # Define the cropping dimensions
  crop_top <- 10
  crop_bottom <- min(2400, img_dims[1])  # Ensure the bottom crop does not exceed image height
  crop_left <- 1550
  crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

  # Crop the image
  img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, ]

  # Convert the image to grayscale
  grey_img <- channel(img_crop, "grey")

  # Set threshold using Otsu's method
  threshold <- otsu(grey_img)
  threshold_img <- combine(mapply(function(frame, th) frame > th, getFrames(grey_img), threshold, SIMPLIFY=FALSE))

  # Perform segmentation
  segmented_img <- bwlabel(threshold_img)

  # Calculate leaf area
  leaf_area <- computeFeatures.shape(segmented_img)[, "s.area"]

  # Store the leaf area
  leaf_areas[i] <- sum(leaf_area)

  # Save the segmented image
  writeImage(segmented_img, output_path)
}

print(leaf_areas)

## save area in csv file

# Define the resolution of the images in dots per inch (dpi)
dpi <- 300

# Convert the leaf areas from pixels to square millimeters
leaf_areas_mm2 <- (leaf_areas / dpi^2) * 25.4^2

# Update the 'Leaf_Area' column in the output data frame
output_data$Leaf_Area_mm2 <- leaf_areas_mm2

# Define the output CSV file path with the updated name
output_csv_mm2 <- file.path(output_folder, "leaf_areas_6_mm2.csv")

# Write the updated data frame to a new CSV file
write.csv(output_data, file = output_csv_mm2, row.names = FALSE)

leaf_area_6 <- read.csv("output/output_EBI/6_otsu/leaf_areas_6_mm2.csv")
leaf_area_6


# function to calculate leaf area in mm2 and save table with image name ------------------------------

library(EBImage)
library(purrr)

leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Initialize a list to store leaf areas
  leaf_areas <- numeric(length(list.of.files))

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Function to process each image
  process_image <- function(file_path) {
    # Read the image
    img <- readImage(file_path)

    # Get the file name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Define the output file path
    output_path <- file.path(output_folder, basename(file_path))

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
    grey_img <- channel(img_crop, "grey")

    # Set threshold using Otsu's method
    threshold <- otsu(grey_img)
    threshold_img <- grey_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)
    # by using ! the background is black and the area of the leaf can be calculated instead of the area of the background minus the leaf

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    writeImage(threshold_img, output_path) # save the image with white background (threshold_img)

    # Return the leaf area in mm^2
    return(leaf_area_mm2)
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), Leaf_Area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas
  return(leaf_areas)
}

# Usage example:
folder_path <- "test/"
output_folder <- "test/output/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2 <- read.csv("test/output/leaf_areas_mm2.csv")
leaf_areas_mm2



# folder 5 ----------------------------------------------------------------

library(EBImage)
library(purrr)

leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Initialize a list to store leaf areas
  leaf_areas <- numeric(length(list.of.files))

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Function to process each image
  process_image <- function(file_path) {
    # Read the image
    img <- readImage(file_path)

    # Get the file name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Define the output file path
    output_path <- file.path(output_folder, basename(file_path))

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
    grey_img <- channel(img_crop, "grey")

    # Set threshold using Otsu's method
    threshold <- otsu(grey_img)
    threshold_img <- grey_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)
    # by using ! the background is black and the area of the leaf can be calculated instead of the area of the background minus the leaf

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    writeImage(threshold_img, output_path) # save the image with white background (threshold_img)

    # Return the leaf area in mm^2
    return(leaf_area_mm2)
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), Leaf_Area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas
  return(leaf_areas)
}

# Usage example:
folder_path <- "raw_scans/5/"
output_folder <- "output/output_EBI/5_images/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2_5 <- read.csv("output/output_EBI/5_images/leaf_areas_mm2.csv")
leaf_areas_mm2_5



# test with blue channel --------------------------------------------------
leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Initialize a list to store leaf areas
  leaf_areas <- numeric(length(list.of.files))

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Function to process each image
  process_image <- function(file_path) {
    # Read the image
    img <- readImage(file_path)

    # Get the file name
    file_name <- tools::file_path_sans_ext(basename(file_path))

    # Define the output file path
    output_path <- file.path(output_folder, basename(file_path))

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
    grey_img <- channel(img_crop, "blue")

    # Set threshold using Otsu's method
    threshold <- otsu(grey_img)
    threshold_img <- grey_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)
    # by using ! the background is black and the area of the leaf can be calculated instead of the area of the background minus the leaf

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    writeImage(threshold_img, output_path) # save the image with white background (threshold_img)

    # Return the leaf area in mm^2
    return(leaf_area_mm2)
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), Leaf_Area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas
  return(leaf_areas)
}

# Usage example:
folder_path <- "test/"
output_folder <- "test/output/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2 <- read.csv("test/output/leaf_areas_mm2.csv")
leaf_areas_mm2



# just write one function instead of 2 -----------------------------------------------------------

library(EBImage)
library(purrr)

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
    crop_left <- 1600
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

# Usage example:
folder_path <- "test/"
output_folder <- "test/output/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2 <- read.csv("test/output/leaf_areas_mm2.csv")
leaf_areas_mm2




# use manual threshold ----------------------------------------------------

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
    crop_left <- 1600
    crop_right <- min(3513, img_dims[2])  # Ensure the right crop does not exceed image width

    # Crop the image
    img_crop <- img[crop_top:crop_bottom, crop_left:crop_right, , drop = FALSE]  # Include 'drop = FALSE' to handle multiple frames

    # Convert the image to grayscale
    blue_img <- channel(img_crop, "blue")

    # Set threshold
    threshold_img <- blue_img > 0.68

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

# Usage example:
folder_path <- "test/"
output_folder <- "test/output_manual/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2 <- read.csv("test/output/leaf_areas_mm2.csv")
leaf_areas_mm2



# autothresh otsu --------------------------------------------------------------

leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Define a function to process each image
  process_image <- function(file_path) {
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

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using autothresh method
    threshold <- autothresholdr::auto_thresh(data, method = "otsu") / 256
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
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- purrr::map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), leaf_area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas data frame
  return(leaf_areas_df)
}

folder_path <- "test/"
output_folder <- "test/output_otsu/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2_otsu <- read.csv("test/output_otsu/leaf_areas_mm2.csv")
leaf_areas_mm2_otsu



# autoshresh minimum ------------------------------------------------------
leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Define a function to process each image
  process_image <- function(file_path) {
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

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using autothresh method
    threshold <- autothresholdr::auto_thresh(data, method = "Minimum") / 256
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
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- purrr::map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), leaf_area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas data frame
  return(leaf_areas_df)
}

folder_path <- "test/"
output_folder <- "test/output_minimum/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2_minimum <- read.csv("test/output_minimum/leaf_areas_mm2.csv")
leaf_areas_mm2_minimum

# autoshresh Moments ------------------------------------------------------
leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Define a function to process each image
  process_image <- function(file_path) {
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

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using autothresh method
    threshold <- autothresholdr::auto_thresh(data, method = "Moments") / 256
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
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- purrr::map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), leaf_area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas data frame
  return(leaf_areas_df)
}

folder_path <- "test/"
output_folder <- "test/output_moments/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2 <- read.csv("test/output_minimum/leaf_areas_mm2.csv")
leaf_areas_mm2

# autoshresh Moments and exclude small particles -------------------------------
leaf_area_EBImage <- function(folder_path, output_folder) {
  # List JPEG files in the folder
  list.of.files <- dir(path = folder_path, pattern = "jpeg|jpg", full.names = TRUE)

  # Create the output folder if it doesn't exist
  dir.create(output_folder, showWarnings = FALSE)

  # Define a function to process each image
  process_image <- function(file_path) {
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

    # Convert to 8-bits (hopefully)
    data <- round(blue_img * 256)

    # Set threshold using autothresh method
    threshold <- autothresholdr::auto_thresh(data, method = "Moments") / 256
    threshold_img <- blue_img >= threshold

    # Perform segmentation
    segmented_img <- bwlabel(!threshold_img)
    # by using ! the background is black and the area of the leaf can be calculated instead of the area of the background minus the leaf

    # Calculate leaf area in pixels
    leaf_area_pixels <- sum(computeFeatures.shape(segmented_img)[, "s.area"], na.rm = TRUE)

    # Define the minimum area threshold for particles (in mm^2)
    min_particle_area_mm2 <- 500

    # Filter out small objects (particles) based on the area threshold
    filtered_segmented_img <- segmented_img
    object_areas <- computeFeatures.shape(segmented_img)[, "s.area"]
    filtered_segmented_img[object_areas < min_particle_area_mm2] <- 0

    # Convert leaf area to mm^2 (assuming known pixel-to-mm conversion factor)
    dpi <- 300
    leaf_area_mm2 <- (leaf_area_pixels / dpi^2) * 25.4^2

    # Save the segmented image
    writeImage(threshold_img, output_path) # save the image with original name

    # Return the leaf area in mm^2
    return(leaf_area_mm2)
  }

  # Process each image in the folder and store the leaf areas
  leaf_areas <- purrr::map_dbl(list.of.files, process_image)

  # Create a data frame with image names and corresponding leaf areas in mm^2
  leaf_areas_df <- data.frame(Image = basename(list.of.files), leaf_area_mm2 = leaf_areas)

  # Write the leaf areas to a CSV file
  output_csv <- file.path(output_folder, "leaf_areas_mm2.csv")
  write.csv(leaf_areas_df, file = output_csv, row.names = FALSE)

  # Return the leaf areas data frame
  return(leaf_areas_df)
}

folder_path <- "test/"
output_folder <- "test/output_moments_filter/"
leaf_areas <- leaf_area_EBImage(folder_path, output_folder)
print(leaf_areas)

leaf_areas_mm2 <- read.csv("test/output_minimum/leaf_areas_mm2.csv")
leaf_areas_mm2

# check time with comparing two methods
bench::mark(m1, m2)


## keep number of leaves as information in table
## split in 2 functions (one for managing files and one for processing leaves)
## delete the part of delete jpeg and add it again
