# extract offset for mandarte for each year
#katherine carbeck
library(raster)

# mandarte coords
coord <- c(-123.28684967295878, 48.63370792160427)

# offset dir
tif_dir <- "/workdir/kcarbeck/data/offsets" 

#list of all .tif files in the directory
tif_files <- list.files(tif_dir, pattern = "\\.tif$", full.names = TRUE)

#data frame to store the results
results <- data.frame(Year = character(), GenomicOffset = numeric())

#now loop through each tif file and extract genomic offset for mandarte
for (tif_file in tif_files) {
  #have to extract both years (e.g.,"offset_1971_1972_genomic_offset.tif")
  year <- sub(".*offset_([0-9]{4}_[0-9]{4})_genomic_offset\\.tif$", "\\1", basename(tif_file))
  
  raster_layer <- raster(tif_file)

  #get offset
  offset_value <- extract(raster_layer, coord)

  #add to df
  results <- rbind(results, data.frame(Year = year, GenomicOffset = offset_value))
}

#remove na rows - i dont know how to fix the loop to avoid duplicating rows 
results <- results[!is.na(results$GenomicOffset), ]
# save
write.csv(results, "/workdir/kcarbeck/data/mandarteOffsetOutput/Mandarte_genomic_offset_output.csv", row.names = FALSE)

#### take results to personal computer to visualize them

