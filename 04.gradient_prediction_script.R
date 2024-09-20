#--------------------------------------------------------------------------------------------------------#
# Load the trained Gradient Forest model and predict environmental conditions for a given year.
#--------------------------------------------------------------------------------------------------------#

library(data.table)
library(hash)
library(gradientForest)
library(raster)
library(rgeos)
library(tidyr)

options(datatable.fread.datatable=FALSE)

args = commandArgs(trailingOnly=TRUE)
range_file <- args[1]
basename <- args[2]
save_dir <- args[3]
model_dir <- args[4]
imports_path <- args[5]

# Load custom R functions from the imports path
source(paste0(imports_path, '/imports.R'))

# Load the trained Gradient Forest model
cat(sprintf('

Loading trained Gradient Forest model ...
'))
gfOut <- readRDS(paste0(model_dir, '/trained_gradient_forest_model.RDS'))

range_data <- read.delim(range_file, sep="\t")
envs <- colnames(gfOut$X)

# Subset the data to exclude NAs in the relevant environmental variables
valid_data <- range_data[complete.cases(range_data[, envs]), ]

cat(sprintf('

Interpolating gradient forests model ...
'))
ptm <- proc.time()
predOut_valid <- predict(gfOut, valid_data[, envs])
print(proc.time() - ptm)

# Initialize predOut.gf as a data frame with the same structure as predOut_valid
predOut.gf <- as.data.frame(matrix(NA, nrow = nrow(range_data), ncol = ncol(predOut_valid)))
names(predOut.gf) <- names(predOut_valid)

# Reinsert the predictions into their corresponding positions
valid_indices <- which(complete.cases(range_data[, envs]))
predOut.gf[valid_indices, ] <- predOut_valid

# Now, predOut.gf is a data frame with predictions where possible and NAs elsewhere

### SAVE
cat(sprintf('

Saving files ...
'))
predwoNA <- paste0(save_dir, sprintf('/%s_gradient_forest_predOut_noNA.RDS', basename))
saveRDS(predOut_valid, predwoNA)

predfile <- paste0(save_dir, sprintf('/%s_gradient_forest_predOut.RDS', basename))
saveRDS(predOut.gf, predfile)

print(predwoNA)
print(predfile)

### END
cat(sprintf('
DONE!
'))
print(format(Sys.time(), format="%B %d %Y %T"))
