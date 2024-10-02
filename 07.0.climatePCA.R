# climate pca for mandarte
# katherine carbeck
# did this on terminal

library(raster)
library(geosphere) # want to extract climate closest to mandarte

#coords
mandarte_coord <- c(-123.28684967295878, 48.63370792160427)

#file directory
txt_dir <- "/workdir/kcarbeck/rasterdir/RasterStackTxtFiles"

# get list of all the txt files in my dir
txt_files <- list.files(txt_dir, pattern = "^raster_data_[0-9]{4}\\.txt$", full.names = TRUE)

# df to store the results for each year
climate_data <- data.frame()

# loop thru each txt file
for (txt_file in txt_files) {
  # extract year from file name (e.g., raster_data_1971.txt)
  year <- sub("raster_data_([0-9]{4})\\.txt$", "\\1", basename(txt_file))
  
  # txt file to df
  climate_df <- read.delim(txt_file)
  
  # calculate distance between each lat/lon and Mandarte to get nearest location
  distances <- distHaversine(cbind(climate_df$lon, climate_df$lat), mandarte_coord)
  
  #find the index of the minimum distance (closest point)
  closest_index <- which.min(distances)
  
  #extract the row of climate data for the closest point
  closest_climate <- climate_df[closest_index, ]
  
  # add year to the climate data
  closest_climate$Year <- year
  
  # finally append the result to the main df
  climate_data <- rbind(climate_data, closest_climate)
}


head(climate_data)

#save
write.csv(climate_data, "/workdir/kcarbeck/Mandarte_multi_climate_variable_output.csv", row.names = FALSE)


#bash 
# cp Mandarte_multi_climate_variable_output.csv /home/lc736_0001/song_sparrow/AmNat/data/MandarteMultiYearClimate



#### PART 2 ####
#make PCA of climate in each year
# done one personal computer


library(ggplot2)

setwd("/Users/katherine/Documents/Projects/SOSP_AmNat/data/MandarteClimate")

climate_data <- read.csv("Mandarte_multi_climate_variable_output.csv")

#remove any non-climatic columns for PCA
climate_vars <- climate_data[, -which(names(climate_data) %in% c("Year", "lon", "lat", "EMT"))]
climate_vars

# standardize the climate variables (mean = 0, sd = 1)
climate_vars_scaled <- scale(climate_vars)

# perform the PCA
pca_result <- prcomp(climate_vars_scaled, center = TRUE, scale. = TRUE)

# extract the first two pcs
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]

#put output to df year, PC1, and PC2 to plot
pca_data <- data.frame(Year = climate_data$Year, PC1 = pc1, PC2 = pc2)

write.csv(pca_data, "Climate_PCA_output.csv")




### make biplot

# extract pca scores (PC1 and PC2 for each year)
pca_scores <- as.data.frame(pca_result$x[, 1:2])
pca_scores$Year <- climate_data$Year  # Add the year to the scores

# extract the pca loadings - how much each variable contributes to PC1 and PC2
pca_loadings <- as.data.frame(pca_result$rotation[, 1:2])
pca_loadings$Variable <- rownames(pca_loadings)

# scale loadings to make them more visible on the biplot (multiply them by a factor that looks nice)
loadings_scaling_factor <- 5
pca_loadings$PC1 <- pca_loadings$PC1 * loadings_scaling_factor
pca_loadings$PC2 <- pca_loadings$PC2 * loadings_scaling_factor

# create biplot - color by loadings & years?
climate_biplot <- ggplot() +
  # plot pca scores - points for each year
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = Year), size = 3) +
  
  # plot pca loadings 
  geom_segment(data = pca_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")), color = "grey30", size = 1) +
  
  # label vars at end of each arrow with a lil offset
  geom_text(data = pca_loadings, aes(x = PC1, y = PC2, label = Variable),
            hjust = 1.2, vjust = -0.5, color = "grey30", size = 4) +
  
  # labels and theme
  labs(title = "Climate PCA Biplot", x = "PC1", y = "PC2") +
  scale_color_gradient(low = "blue", high = "red") +  # Keeping gradient for year coloring
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "white", size = 0.0))  # Subtle grid for clarity

climate_biplot

ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/ClimateFigs/climate_biplot.svg", 
       plot = climate_biplot, width = 6, height = 5)



# ------
# get the variance explained by each PC
pca_var <- pca_result$sdev^2  # eigenvalues (variance explained by each PC)

# calculate the proportion of variance explained by each PC
pca_var_explained <- pca_var / sum(pca_var)
pca_var_explained[1] * 100  
# 37.05297
pca_var_explained[2] * 100  
# 30.21551



#------- loadings
pca_loadings <- as.data.frame(pca_result$rotation[, 1:2])
pca_loadings$Variable <- rownames(pca_loadings)
pca_loadings <- pca_loadings[, c("Variable", "PC1", "PC2")]
pca_loadings

write.csv(pca_loadings, "/Users/katherine/Documents/Projects/SOSP_AmNat/output_tables/Climate_PCA_loadings.csv", row.names = FALSE)




