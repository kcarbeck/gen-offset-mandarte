#--------------------------------------------------------------------------------------------------------#
# Calculate genomic offset between predictions for two consecutive years.
#--------------------------------------------------------------------------------------------------------#

library(data.table)
library(hash)
library(gradientForest)
library(raster)
library(rgeos)
library(tidyr)

options(datatable.fread.datatable=FALSE)

args = commandArgs(trailingOnly=TRUE)
pred_file_year1 <- args[1]
pred_file_year2 <- args[2]
range_file <- args[3]
offset_basename <- args[4]
save_dir <- args[5]
imports_path <- args[6]

#load custom R functions from the imports path
source(paste0(imports_path, '/imports.R'))

#load predictions for the two years
cat(sprintf('

Loading predictions for two consecutive years ...

'))
pred_year1 <- readRDS(pred_file_year1)
pred_year2 <- readRDS(pred_file_year2)


range_data <- read.delim(range_file, sep = "\t")


#calculate genomic offset (Euclidean distance between predictions)
cat(sprintf('

Calculating genomic offset ...

'))
#genomic_offset <- sqrt(rowSums((pred_year2 - pred_year1)^2, na.rm = TRUE))
genomic_offset <- sqrt(apply((pred_year2 - pred_year1)^2, 1, function(x) {
  if (any(is.na(x))) {
    return(NA)
  } else {
    return(sum(x))
  }
}))


# add genomic offset results to range data
range_data$genomic_offset <- genomic_offset

# create a georeferenced raster from the range data (lat, lon, and genomic offset)
coords <- range_data[, c("lon", "lat")]
r <- rasterFromXYZ(cbind(coords, range_data$genomic_offset), crs = "+proj=longlat +datum=WGS84")

# save the raster as a GeoTIFF
tiff_file <- paste0(save_dir, sprintf('/%s_genomic_offset.tif', offset_basename))
cat(sprintf('Saving GeoTIFF: %s', tiff_file))
writeRaster(r, filename = tiff_file, format = "GTiff", overwrite = TRUE)


# save the raster as a netCDF file
netcdf_file <- paste0(save_dir, sprintf('/%s_genomic_offset.nc', offset_basename))
cat(sprintf('\nSaving netCDF: %s\n', netcdf_file))
writeRaster(r, filename = netcdf_file, format = "CDF", overwrite = TRUE)


#save the genomic offset results
offset_file <- paste0(save_dir, sprintf('/%s_genomic_offset.RDS', offset_basename))
saveRDS(genomic_offset, offset_file)
print(offset_file)

### END
cat(sprintf('
DONE!
'))
print(format(Sys.time(), format="%B %d %Y %T"))
