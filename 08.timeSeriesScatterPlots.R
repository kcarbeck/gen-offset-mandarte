# time series scatter plots of genomic offset and population demography 
# katherine carbeck
# 22 aug 2024

setwd("/Users/katherine/Documents/Projects/SOSP_AmNat/data/mandarteOffsetOutput")

library(tidyverse)
library(tseries)
library(cowplot)

# outline:
# 1. load data
# 2. ccfs for each variable
# 3. viz CCF scatter plots


#### 1. data ####
dat <- read.csv("Mandarte_genomic_offset_output_AND_demography_AND_Climate.csv")
str(dat)

# filter data for the years 1975 to 2018
filtered_results <- dat[dat$Year2 >= 1972 & dat$Year2 <= 2023, ]
head(filtered_results)



####  2. CROSS CORRELATION FUNCTION  ####
#directly measures the correlation between the two time series at various lags

# ~lambda~ #
dat_sub <- filtered_results %>%
  dplyr::select(Year2, GenomicOffset, Lambda)
                #mean_indep_young_per_female, Juv_surv_fledglings, Juv_surv_indep, Ad_fem_res_surv_prev_year, Ad_fem_sur)
#NAs
dat_sub <- na.omit(dat_sub)

#CCF
ccf_lambda <- ccf(dat_sub$GenomicOffset, dat_sub$Lambda , lag.max=10, type="correlation")
plot(ccf_lambda)
ccf_data <- data.frame(
  lag = ccf_lambda$lag,
  acf = ccf_lambda$acf
)
ccf_data
#  -3 -0.176807643
#  -2 -0.241432074 **
#  -1 -0.129598112
#   0 -0.084524162


# ~Adult Survival~ # 
dat_sub <- filtered_results %>%
  dplyr::select(Year2, GenomicOffset, Ad_fem_sur) #Ad_fem_sur
#NAs
dat_sub <- na.omit(dat_sub)

#CCF
ccf_ad <- ccf(dat_sub$GenomicOffset, dat_sub$Ad_fem_sur , lag.max=10, type="correlation")
plot(ccf_ad)
ccf_data <- data.frame(
  lag = ccf_ad$lag,
  acf = ccf_ad$acf
)
ccf_data
# -3 -0.151461607 **
# -2  0.006691385
# -1  0.043910598
#  0 -0.042053723


# ~Juv Survival~ # 
dat_sub <- filtered_results %>%
  dplyr::select(Year2, GenomicOffset, Juv_surv_fledglings)
#NAs
dat_sub <- na.omit(dat_sub)

#CCF
ccf_juv <- ccf(dat_sub$GenomicOffset, dat_sub$Juv_surv_fledglings , lag.max=10, type="correlation")
plot(ccf_juv)
ccf_data <- data.frame(
  lag = ccf_juv$lag,
  acf = ccf_juv$acf
)
ccf_data
# -3  0.4128337202
# -2 -0.0937533385
# -1 -0.2813059121 **
#  0 -0.0375837215 


# ~Repro success~ # 
dat_sub <- filtered_results %>%
  dplyr::select(Year2, GenomicOffset, mean_indep_young_per_female)
#NAs
dat_sub <- na.omit(dat_sub)

#CCF
ccf_rs <- ccf(dat_sub$GenomicOffset, dat_sub$mean_indep_young_per_female , lag.max=10, type="correlation")
plot(ccf_rs)
ccf_data <- data.frame(
  lag = ccf_rs$lag,
  acf = ccf_rs$acf
)
ccf_data
#  -3  0.043096617
#  -2 -0.225812163
#  -1 -0.299809267 **
#   0 -0.250218749


####  3. SCATTER PLOTS  ####
#loop through each var and lag
variables <- c("Lambda", "Ad_fem_sur", "Juv_surv_fledglings", "mean_indep_young_per_female")
variable_labels <- c("Population Growth Rate", "Adult Survival", "Juvenile Survival", "Reproductive Success")
lags <- 0:3

#list for storing the plots
plot_list <- list()

for (var in variables) {
  for (lag in lags) {
    #lagged var column
    filtered_results[[paste0(var, "_lag", lag)]] <- dplyr::lead(filtered_results[[var]], n = lag)
    
    #subset data & remove NAs specific to current var and lag
    df_lagged <- filtered_results %>%
      dplyr::select(Year2, GenomicOffset, !!sym(paste0(var, "_lag", lag))) %>%
      na.omit() 
    
    # check if the dataframe is mismatched 
    if (nrow(df_lagged) == 0) {
      print(paste("No valid data for", var, "at lag", lag))
      next
    }
    
    #calc corr
    correlation <- cor(df_lagged$GenomicOffset, df_lagged[[paste0(var, "_lag", lag)]], use = "complete.obs")
    
    #now plot
    plot <- ggplot(df_lagged, aes(x = GenomicOffset, y = !!sym(paste0(var, "_lag", lag)))) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +  # Using correct formula here
      annotate("text", x = Inf, y = Inf, label = paste("r = ", round(correlation, 3), sep = ""), 
               vjust = 1.5, hjust = 1.5, size = 5, color = "blue") +
      labs(x = "Genomic Offset", y = paste0(variable_labels[which(variables == var)], " (Lagged ", lag, " Years)")) +
      common_theme
    
    #and add the plot to my list
    plot_list[[paste0(var, "_lag", lag)]] <- plot
  }
}

#combine all the plots using cowplot::plot_grid which should make them into a nice grid layout
combined_plot <- plot_grid(
  plot_list$Lambda_lag0, plot_list$Lambda_lag1, plot_list$Lambda_lag2, plot_list$Lambda_lag3,
  plot_list$Ad_fem_sur_lag0, plot_list$Ad_fem_sur_lag1, plot_list$Ad_fem_sur_lag2, plot_list$Ad_fem_sur_lag3,
  plot_list$Juv_surv_fledglings_lag0, plot_list$Juv_surv_fledglings_lag1, plot_list$Juv_surv_fledglings_lag2, plot_list$Juv_surv_fledglings_lag3,
  plot_list$mean_indep_young_per_female_lag0, plot_list$mean_indep_young_per_female_lag1, plot_list$mean_indep_young_per_female_lag2, plot_list$mean_indep_young_per_female_lag3,
  ncol = 4, align = "hv"
)

combined_plot


ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/combined_scatterplots_3years.svg", 
       plot = combined_plot, width = 14, height = 12)


