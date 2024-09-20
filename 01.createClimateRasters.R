# create raster files / raster stack input files
# katherine carbeck
# 12 august 2024

## OUTLINE:
# 1. crop NA DEM to study area for input into climate NA
# (1a. then put DEM into climateNA for each year)
# 2. create raster stacks for each year
# 3. extract all values to txt file for GF


library(terra)
library(raster)
library(tidyverse)
setwd("C:/Users/kcarbeck.stu/Desktop/ClimateNA")


####*  1. crop raster file for input into climateNA to get    *####

dem <- raster("InputFiles/na800.asc")
crop_extent <- extent(-126, -118, 46.5, 50.5)
cropped_dem <- crop(dem, crop_extent)
plot(cropped_dem)
writeRaster(cropped_dem, "InputFiles/WestDEM800m.asc", format="ascii", overwrite=TRUE)


####* 2. create rasters stacks from downloaded rasters from climateNA     *#####

folder_path <- "C:/Users/kcarbeck.stu/Desktop/ClimateNA/OutFilesWest800m"
# list environmental variables
variables <- c('DD_0', 'DD18', 'EMT', 'MAT', 'MSP', 'SHM', 'TD', 'RH')


# read in by looping through each year
for (year in 1971:2023) {
  # create a list to store raster objects for each variable
  rasters <- list()
  
  # then loop through each environmental variable
  for (variable in variables) {
    # construct file name
    file_name <- paste0("Year_", year, "Y", "/", variable, ".asc")
    
    # read raster data
    raster_obj <- raster(file.path(folder_path, file_name))
    
    # assign name to the raster object
    assign(paste0(variable, "_", year), raster_obj, envir = .GlobalEnv)
    
    # store raster object in the list
    rasters[[variable]] <- raster_obj
  }
  # create raster stacks from list of rasters
  stack_obj <- stack(rasters)
  
  # assign names to the raster stacks 
  assign(paste0("stack_", year), stack_obj, envir = .GlobalEnv)
}

rm(stack_obj) # remove last object


## save raster stacks
out_path <- "C:/Users/kcarbeck.stu/Desktop/ClimateNA/RasterStacksWest800m"

for (year in 1971:2023) {
  # Get the stack object
  stack_obj <- get(paste0("stack_", year))
  
  # Define the output file name
  output_file <- paste0(out_path, "/stack_", year, ".tif")
  
  # Save the raster stack as a GeoTIFF file
  writeRaster(stack_obj, output_file, format = "GTiff", overwrite = TRUE)
}



###*   3. extract values to txt file   *###
txt_out_path <- "C:/Users/kcarbeck.stu/Desktop/ClimateNA/RasterStackTxtFiles"

# Loop through each year to extract the values and save them to a text file
for (year in 1971:2023) {
  
  # Get the raster stack object
  stack_obj <- get(paste0("stack_", year))
  
  # Extract the values from the raster stack
  values_matrix <- values(stack_obj)
  
  # Get the coordinates
  coords <- xyFromCell(stack_obj, 1:ncell(stack_obj))
  
  # Combine coordinates with the raster values
  combined_data <- cbind(coords, values_matrix)
  
  # Extract layer names from the raster stack
  layer_names <- names(stack_obj)
  
  # Define the column names
  colnames(combined_data) <- c("lon", "lat", layer_names)
  
  # Define the output file name
  txt_file <- paste0(txt_out_path, "/raster_data_", year, ".txt")
  
  # Write the data to a text file
  write.table(combined_data, file = txt_file, sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)
}

head(combined_data)



### output txt files will be used for GF

