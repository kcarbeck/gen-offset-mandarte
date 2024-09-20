# did this on terminal

library(raster)
library(geosphere) # want to extract climate closest to mandarte

# Mandarte coordinates
mandarte_coord <- c(-123.28684967295878, 48.63370792160427)

# Text file directory
txt_dir <- "/workdir/kcarbeck/rasterdir/RasterStackTxtFiles"

# List all .txt files in the directory
txt_files <- list.files(txt_dir, pattern = "^raster_data_[0-9]{4}\\.txt$", full.names = TRUE)

# Initialize a data frame to store the results for each year
climate_data <- data.frame()

# Loop through each text file
for (txt_file in txt_files) {
  # Extract the year from the file name (e.g., raster_data_1971.txt)
  year <- sub("raster_data_([0-9]{4})\\.txt$", "\\1", basename(txt_file))
  
  # Read the text file into a data frame
  climate_df <- read.delim(txt_file)
  
  # Calculate the distance between each lat/lon in the file and the Mandarte coordinate
  distances <- distHaversine(cbind(climate_df$lon, climate_df$lat), mandarte_coord)
  
  # Find the index of the minimum distance (closest point)
  closest_index <- which.min(distances)
  
  # Extract the row of climate data for the closest point
  closest_climate <- climate_df[closest_index, ]
  
  # Add the year to the climate data
  closest_climate$Year <- year
  
  # Append the result to the main data frame
  climate_data <- rbind(climate_data, closest_climate)
}


# View the final result
head(climate_data)

# Save the results to CSV
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

# Standardize the climate variables (mean = 0, sd = 1)
climate_vars_scaled <- scale(climate_vars)

# Perform PCA
pca_result <- prcomp(climate_vars_scaled, center = TRUE, scale. = TRUE)

# Extract the first two principal components
pc1 <- pca_result$x[, 1]
pc2 <- pca_result$x[, 2]

# Create a data frame with Year, PC1, and PC2 for plotting
pca_data <- data.frame(Year = climate_data$Year, PC1 = pc1, PC2 = pc2)

write.csv(pca_data, "Climate_PCA_output.csv")






### make biplot
# Extract the PCA scores (PC1 and PC2 for each year)
pca_scores <- as.data.frame(pca_result$x[, 1:2])
pca_scores$Year <- climate_data$Year  # Add the year to the scores

# Extract the PCA loadings (how much each variable contributes to PC1 and PC2)
pca_loadings <- as.data.frame(pca_result$rotation[, 1:2])
pca_loadings$Variable <- rownames(pca_loadings)

# Scale the loadings to make them more visible on the biplot (multiplying by a factor, e.g., 5)
loadings_scaling_factor <- 5
pca_loadings$PC1 <- pca_loadings$PC1 * loadings_scaling_factor
pca_loadings$PC2 <- pca_loadings$PC2 * loadings_scaling_factor

# Create the biplot with distinct color for loadings and years
climate_biplot <- ggplot() +
  # Plot the PCA scores (points for each year)
  geom_point(data = pca_scores, aes(x = PC1, y = PC2, color = Year), size = 3) +
  
  # Plot the PCA loadings (arrows for each climate variable in a different color)
  geom_segment(data = pca_loadings, aes(x = 0, y = 0, xend = PC1, yend = PC2),
               arrow = arrow(length = unit(0.3, "cm")), color = "grey30", size = 1) +
  
  # Label the variables at the end of each arrow with some offset
  geom_text(data = pca_loadings, aes(x = PC1, y = PC2, label = Variable),
            hjust = 1.2, vjust = -0.5, color = "grey30", size = 4) +
  
  # Labels and theme
  labs(title = "Climate PCA Biplot", x = "PC1", y = "PC2") +
  scale_color_gradient(low = "blue", high = "red") +  # Keeping gradient for year coloring
  theme_minimal() +
  theme(panel.grid.major = element_line(color = "white", size = 0.0))  # Subtle grid for clarity


# Display the plot
print(climate_biplot)

# Save the plot if needed
ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/ClimateFigs/climate_biplot.svg", 
       plot = climate_biplot, width = 6, height = 5)









## time series plot

head(pca_data)
pca_data_sub <- pca_data[pca_data$Year >= 1975 & dat$Year2 <= 2023, ]


# Define common breaks for the x-axis based on years
common_breaks <- seq(min(pca_data_sub$Year), max(pca_data_sub$Year), by = 5)

# Define a common theme for all plots to ensure consistency
common_theme <- theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_line(color = "grey90", size = 0.5),
    panel.border = element_rect(color = "grey20", fill = NA, size = .5) , 
    panel.grid.minor = element_blank(),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") # Ensure equal margins for all plots
  )

# Define the PCA plot to match the style of your other plots
plot_pca <- ggplot(pca_data_sub, aes(x = Year, y = PC1)) +
  geom_line() +
  labs(y = "Principal Component 1") +
  common_theme +
  scale_x_continuous(breaks = common_breaks) +
  scale_y_continuous(limits = c(min(pca_data_sub$PC1), max(pca_data_sub$PC1))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

# Second component plot
plot_pca_pc2 <- ggplot(pca_data_sub, aes(x = Year, y = PC2)) +
  geom_line() +
  labs(x = "Year", y = "Principal Component 2") +
  common_theme +
  scale_x_continuous(breaks = common_breaks) +
  scale_y_continuous(limits = c(min(pca_data_sub$PC2), max(pca_data_sub$PC2)))

# Combine the PCA plots with the other plots if needed
combined_pca_plot <- plot_grid(plot_pca, plot_pca_pc2, ncol = 1, align = "v", axis = "lr", rel_heights = c(1, 1.1))

combined_pca_plot

# Save or display the PCA plots
ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/ClimateFigs/climate_pca_by_year.svg", 
       plot = combined_pca_plot, width = 8, height = 6)












