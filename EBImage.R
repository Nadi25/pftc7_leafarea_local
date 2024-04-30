
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
# disc <- makeBrush(100, "disc")
# disc <- disc / sum(disc)
# offset <- 0.00005
# grey_img_bg <- filter2( grey_img, disc )
# grey_img_th <- grey_img > grey_img_bg + offset
# display(grey_img_th, all=TRUE)
#
# display( thresh(grey_img, w=15, h=15, offset=0.05), all=TRUE )
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




