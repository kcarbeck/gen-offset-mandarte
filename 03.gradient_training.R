
#--------------------------------------------------------------------------------------------------------#
# Given a set of populations, allele freqs, and environmental data, train gradient forests.
#
# Usage
# -----
# Rscript gradient_training.R snpfile envfile range_file basename save_dir imports_path
#
# Notes
# -----
# - snpfile and envfile must already be subset for training populations
#
# Parameters
# ----------
# snpfile - SNPs that you want to use to train gradient forests (row.names=pop_ID, cols=loci)
# envfile - lat/long, environmental data for each pop
# range_file - tab delimited .txt file path with columns for `lat`, `lon`, and each env used in training
#    - this will be used to interpolate model on pops' lat/long to a larger area
#      - specifically this should the range-wide environmental data that is from same 
#        timeframe as in `envfile` (eg 1961-1990)
#    - generally, these are the envdata extracted from ClimateNA GIS data, and then clipped
#        to the range map
#    - these can be current, future, or uniform (eg for common garden) climate
#    - see eg yeaman03:/notebooks/007_JP_gea/07_gradient_forest/01_prep_spatial_envdata.ipynb
# basename - the basename prefix wanted for saving files in `save_dir`
# save_dir - the directory where files should be saved
# imports_path - path to directory with my homebrew R functions
#--------------------------------------------------------------------------------------------------------#

library(data.table)
library(hash)
library(gradientForest)
library(raster)
library(rgeos)
library(tidyr)

options(datatable.fread.datatable=FALSE)

args = commandArgs(trailingOnly=TRUE)
snpfile <- args[1]
envfile <- args[2]
save_dir <- args[3]
imports_path <- args[4]

source(paste0(imports_path, '/imports.R'))  # https://github.com/brandonlind/r_imports

print(format(Sys.time(), format="%B %d %Y %T"))

prep_snps <- function(snpfile, exclude=c(), sep='\t'){
  #----------------------------------------------------------------#
  # READ IN FREQUENCYxPOP TABLE, REMOVE POPS IN exclude
  #
  # Notes
  # -----
  # - assumes snpfile has colname 'index' which has pop names
  # - the rest of the colnames are names for each locus in dataset
  #----------------------------------------------------------------#
  # load data
  snps <- read.delim(snpfile, sep=sep)
  # set row names
  #row.names(snps) <- snps[ ,'index']
  # remove column so all cols are loci
 # snps <- subset(snps, select = -c(index))
  # get popnames, exclude irrelevant columns
  pops <- rownames(snps)[!rownames(snps) %in% c('CHROM', 'POS')]
  if (len(exclude) > 0){
    # exclude any other pops
    pops <- pops[!pops %in% exclude]
  }
  # reframe datatable
  snps <- snps[pops, ]
  return(snps)
}



prep_envdata <- function(envfile, pops){
    #-------------------------------------------------------------------------#
    # COLOR GEOGRAPHIC GROUPS AND REORDER POPS IN ENVIRONMENT TABLE 
    #
    # Notes
    # -----
    # - places `envs` (names of environments) in global namespace
    #-------------------------------------------------------------------------#
    
    # read in the environmental data (previously centered/standardized)
    envdata <- read.csv(envfile)

    envs <- c('DD_0', 'DD18', 'EMT', 'MAT', 'MSP','RH', 'SHM', 'TD')

    stopifnot(all(envs %in% colnames(envdata)))
    cat(sprintf('There are %s environmental attributes in the envdata table.', len(envs)))
    
    #change colnames
    colnames(envdata)[colnames(envdata) == "Population"] <- "pops"
    colnames(envdata)[colnames(envdata) == "LONG"] <- "lon"
    colnames(envdata)[colnames(envdata) == "LAT"] <- "lat"

    # set row names and remove pops column 
    rownames(envdata) <- envdata$pops
    envdata <- envdata[, -1]
    
    # order pops same as snp table
    envdata <- envdata[pops, ]
    
    # add to global namespace so I don't have to return a stupid list, stupid R
    envs <<- envs
    
    return(envdata[, envs])
}




### SET UP NAMESPACE

# get allele freqs for each pop
cat(sprintf('
Prepping snp data ...
'))
cand_freqs <- prep_snps(snpfile) #, exclude=test_pops)

# get population names
pops <- rownames(cand_freqs)
cat(sprintf('
Pops ...
'))
cat(sprintf(pops))

# prep training data (climate data at location of populations)
cat(sprintf('

Prepping envdata
 ...'))
envdata <- prep_envdata(envfile, pops)




### TRAIN GRADIENT FORESTS

cat(sprintf('

Training gradient forests ...
'))

# double check that pops are in same order
print(rownames(envdata))
print(rownames(cand_freqs))
stopifnot(all(rownames(envdata)==rownames(cand_freqs)))

# NOTE: envdata below must have been filtered for env-only columns
# calc of maxLevel is different but not effectively from Ellis 2012 Supp 1 https://doi.org/10.6084/m9.figshare.c.3304350.v1
# recommended by Stephen Keller
maxLevel <- floor(log2(0.368*nrow(envdata)/2))
ptm <- proc.time()
gfOut <- gradientForest(cbind(envdata, cand_freqs),
                        predictor.vars=colnames(envdata[ ,envs]),
                        response.vars=colnames(cand_freqs),
                        ntree=500,
                        transform=NULL,
                        #compact = TRUE,
                        nbin=201,  # doesn't do anything if not running compact = TRUE
                        maxLevel=maxLevel,
                        corr.threshold=0.5)
print(proc.time() - ptm)


### SAVE TRAINED MODEL
cat(sprintf('

Saving trained model ...
'))
gfOut_trainingfile <- paste0(save_dir, '/trained_gradient_forest_model.RDS')
saveRDS(gfOut, gfOut_trainingfile)
print(gfOut_trainingfile)


### END
cat(sprintf('
DONE!
'))
print(format(Sys.time(), format="%B %d %Y %T"))


