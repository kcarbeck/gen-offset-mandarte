# plot output of annual genomic offset, climate, and population demography from Mandarte
# katherine carbeck
# 22 aug 2024

setwd("/Users/katherine/Documents/Projects/SOSP_AmNat/data/mandarteOffsetOutput")
library(tidyverse)
library(gridExtra)
library(cowplot)

# population demography + offset
dat <- read.csv("Mandarte_genomic_offset_output_AND_demography_AND_Climate.csv")
str(dat)

# filter data for the years 1975 to 2018
filtered_results <- dat[dat$Year2 >= 1975 & dat$Year2 <= 2023, ]
head(filtered_results)

#quick preview
ggplot(dat, aes(x = Year, y = GenomicOffset, group = 1)) +
  geom_line(color = "gray10") +
  geom_point(color = "red") +
  labs(
    x = "Year",
    y = "Genomic Offset"
  ) +
  theme_minimal()



################################  plotting  #####################################
# years - because different demog rates have different end dates
full_years <- data.frame(Year2 = 1975:2023)

#merge the full year range with filtered results
extended_data <- full_years %>%
  left_join(filtered_results, by = "Year2")
str(extended_data)

#common breaks for x-axis
common_breaks <- seq(1975, 2023, by = 5)

# ok now create each individual plot
plot_genomic_offset <- ggplot(extended_data, aes(x = Year2, y = GenomicOffset)) +
  geom_line() +
  labs(y = "Genomic Offset") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(0, max(extended_data$GenomicOffset, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_population_growth <- ggplot(extended_data, aes(x = Year2, y = Lambda)) +
  geom_line() +
  labs(y = "Population Growth Rate") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(0, max(extended_data$Lambda, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_RS <- ggplot(extended_data[!is.na(extended_data$mean_indep_young_per_female),], aes(x = Year2, y = mean_indep_young_per_female)) +
  geom_line() +
  labs(y = "Reproductive Success") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(0, max(extended_data$mean_indep_young_per_female, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_adult_survival <- ggplot(extended_data, aes(x = Year2, y = Ad_fem_sur )) +
  geom_line() +
  labs(y = "Adult Survival") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(0, max(extended_data$Ad_fem_sur, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_juvenile_survival <- ggplot(extended_data[!is.na(extended_data$Juv_surv_fledglings),], aes(x = Year2, y = Juv_surv_fledglings)) +
  geom_line() +
  labs(y = "Juvenile Survival") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(0, max(extended_data$Juv_surv_fledglings, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_pc1 <- ggplot(extended_data, aes(x = Year2, y = PC1)) +
  geom_line() +
  labs(y = "PC1") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(min(extended_data$PC1, na.rm = TRUE), max(extended_data$PC1, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

plot_pc2 <- ggplot(extended_data, aes(x = Year2, y = PC2)) +
  geom_line() +
  labs(y = "PC2") +
  common_theme +
  scale_x_continuous(breaks = common_breaks, limits = c(1975, 2023)) +
  scale_y_continuous(limits = c(min(extended_data$PC2, na.rm = TRUE), max(extended_data$PC2, na.rm = TRUE))) +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())

#combine plots
combined_plot <- plot_grid(plot_genomic_offset, plot_pc1, plot_pc2, plot_population_growth, 
                           plot_RS, plot_adult_survival, plot_juvenile_survival, 
                           ncol = 1, align = "v", axis = "lr", 
                           rel_heights = c(1, 1, 1, 1, 1, 1, 1.1))

combined_plot

#save
ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/combined_full_plot_updated.svg", 
       plot = combined_plot, width = 8, height = 16)



########  END  ########
