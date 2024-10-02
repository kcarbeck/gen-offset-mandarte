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

# filter data for the years 
filtered_results <- dat[dat$Year2 >= 1975 & dat$Year2 <= 2023, ]
head(filtered_results)

# common theme for plots, without grid lines
common_theme <- theme_minimal() +
  theme(
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_rect(color = "grey20", fill = NA, size = .5),  # Keep the panel border
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")  # Ensure equal margins for all plots
  )



#***************************************************# 
####        2. CROSS CORRELATION FUNCTION        ####
#***************************************************# 
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

# ~Juv Survival~ # 
dat_sub <- filtered_results %>%
  dplyr::select(Year2, GenomicOffset, Juv_surv_fledglings)
#NAs
dat_sub <- na.omit(dat_sub)
dat_sub
#CCF
ccf_juv <- ccf(dat_sub$GenomicOffset, dat_sub$Juv_surv_fledglings , lag.max=10, type="correlation")
plot(ccf_juv)
ccf_data <- data.frame(
  lag = ccf_juv$lag,
  acf = ccf_juv$acf
)
ccf_data

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


#***************************************************# 
#empty df for results
correlation_results <- data.frame(
  Lag = integer(),
  Lambda = numeric(),
  Lambda_p_value = numeric(),
  Adult_Survival = numeric(),
  Adult_Survival_p_value = numeric(),
  Juvenile_Survival = numeric(),
  Juvenile_Survival_p_value = numeric(),
  Reproductive_Success = numeric(),
  Reproductive_Success_p_value = numeric(),
  stringsAsFactors = FALSE
)

# loop for each var and lag
variables <- c("Lambda", "Ad_fem_sur", "Juv_surv_fledglings", "mean_indep_young_per_female")
variable_labels <- c("Population Growth Rate", "Adult Survival", "Juvenile Survival", "Reproductive Success")
lags <- 1:3

#list to store correlations for each var
results_list <- list(Lambda = numeric(3), Lambda_p_value = numeric(3), 
                     Adult_Survival = numeric(3), Adult_Survival_p_value = numeric(3),
                     Juvenile_Survival = numeric(3), Juvenile_Survival_p_value = numeric(3),
                     Reproductive_Success = numeric(3), Reproductive_Success_p_value = numeric(3))

for (lag in lags) {
  # for each var calculate cor and p values
  for (var in variables) {
    # lag for each var
    filtered_results[[paste0(var, "_lag", lag)]] <- dplyr::lead(filtered_results[[var]], n = lag)
    
    # subset data & remove NAs
    df_lagged <- filtered_results %>%
      dplyr::select(Year2, GenomicOffset, !!sym(paste0(var, "_lag", lag))) %>%
      na.omit()
    
    # pearson cor and pvals
    cor_test_result <- cor.test(df_lagged$GenomicOffset, df_lagged[[paste0(var, "_lag", lag)]], use = "complete.obs")
    correlation <- cor_test_result$estimate
    p_value <- cor_test_result$p.value
    
    #store output
    if (var == "Lambda") {
      results_list$Lambda[lag] <- round(correlation, 3)
      results_list$Lambda_p_value[lag] <- round(p_value, 5)
    } else if (var == "Ad_fem_sur") {
      results_list$Adult_Survival[lag] <- round(correlation, 3)
      results_list$Adult_Survival_p_value[lag] <- round(p_value, 5)
    } else if (var == "Juv_surv_fledglings") {
      results_list$Juvenile_Survival[lag] <- round(correlation, 3)
      results_list$Juvenile_Survival_p_value[lag] <- round(p_value, 5)
    } else if (var == "mean_indep_young_per_female") {
      results_list$Reproductive_Success[lag] <- round(correlation, 3)
      results_list$Reproductive_Success_p_value[lag] <- round(p_value, 5)
    }
  }
  
  # append to final df
  correlation_results <- rbind(correlation_results, data.frame(
    Lag = lag,
    Lambda = results_list$Lambda[lag],
    Lambda_p_value = results_list$Lambda_p_value[lag],
    Adult_Survival = results_list$Adult_Survival[lag],
    Adult_Survival_p_value = results_list$Adult_Survival_p_value[lag],
    Juvenile_Survival = results_list$Juvenile_Survival[lag],
    Juvenile_Survival_p_value = results_list$Juvenile_Survival_p_value[lag],
    Reproductive_Success = results_list$Reproductive_Success[lag],
    Reproductive_Success_p_value = results_list$Reproductive_Success_p_value[lag]
  ))
}

correlation_results

#save
write.csv(correlation_results, "/Users/katherine/Documents/Projects/SOSP_AmNat/output_tables/formatted_correlation_results.csv", row.names = FALSE)




#***************************************************# 
####             3. SCATTER PLOTS             ####
#***************************************************# 

#loop through each var and lag
variables <- c("Lambda", "Ad_fem_sur", "Juv_surv_fledglings", "mean_indep_young_per_female")
variable_labels <- c("Population Growth Rate", "Adult Survival", "Juvenile Survival", "Reproductive Success")
lags <- 1:3

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
    correlation <- cor(df_lagged$GenomicOffset, df_lagged[[paste0(var, "_lag", lag)]], use = "na.or.complete")
    print(paste(var, lag, round(correlation,3)))
    
    #now plot
    plot <- ggplot(df_lagged, aes(x = GenomicOffset, y = !!sym(paste0(var, "_lag", lag)))) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +  
      annotate("text", x = Inf, y = Inf, label = paste("r = ", round(correlation, 3), sep = ""), 
               vjust = 1.5, hjust = 1.5, size = 5, color = "blue") +
      labs(x = "Genomic Offset", y = paste0(variable_labels[which(variables == var)], " (Lag ", lag, ")")) +
      common_theme
    
    #and add the plot to my list
    plot_list[[paste0(var, "_lag", lag)]] <- plot
  }
}

#combine all the plots using cowplot::plot_grid which should make them into a nice grid layout
combined_plot <- plot_grid(
  plot_list$Lambda_lag1, plot_list$Lambda_lag2, plot_list$Lambda_lag3,
  plot_list$Ad_fem_sur_lag1, plot_list$Ad_fem_sur_lag2, plot_list$Ad_fem_sur_lag3,
  plot_list$Juv_surv_fledglings_lag1, plot_list$Juv_surv_fledglings_lag2, plot_list$Juv_surv_fledglings_lag3,
  plot_list$mean_indep_young_per_female_lag1, plot_list$mean_indep_young_per_female_lag2, plot_list$mean_indep_young_per_female_lag3,
  ncol = 3, align = "hv"
)

combined_plot

ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/combined_scatterplots_3years.svg", 
       plot = combined_plot, width = 14, height = 12)




#***************************************************# 
####         4. DETRENDED CORRELATION          ####
#***************************************************#
dat <- read.csv("Mandarte_genomic_offset_output_AND_demography_AND_Climate.csv")
dat <- dat[dat$Year2 >= 1975 & dat$Year2 <= 2023, ]

## 1. regress each variable on year and get residuals
# regress genomic offset on year to capture trend
model_offset <- lm(GenomicOffset ~ Year2, data = dat)
summary(model_offset) 

#juvenile survival
model_juvenile_survival <- lm(Juv_surv_fledglings ~ Year2, data = dat)
summary(model_juvenile_survival)

# adult survival
model_adult_survival <- lm(Ad_fem_sur ~ Year2, data = dat)
summary(model_adult_survival)

# reproductive success
model_reproductive_success <- lm(mean_indep_young_per_female ~ Year2, data = dat)
summary(model_reproductive_success)

# pop growth
model_population_growth <- lm(Lambda ~ Year2, data = dat)
summary(model_population_growth)

# climate
model_pc1 <- lm(PC1 ~ Year2, data = dat)
summary(model_pc1)

model_pc2 <- lm(PC2 ~ Year2, data = dat)
summary(model_pc2)


##
residuals_offset <- residuals(model_offset)
residuals_js <- residuals(model_juvenile_survival)
residuals_as <- residuals(model_adult_survival)
residuals_rs<- residuals(model_reproductive_success)
residuals_lambda <- residuals(model_population_growth)
residuals_js
residuals_offset

## 2. correlate detrended offset with detrended data

## 2.1. juv survival
# subset years where both residuals_offset and residuals_js have data
years_js <- dat$Year2[!is.na(residuals_js)]
years_offset_js <- dat$Year2[!is.na(residuals_offset)]
common_years_js <- intersect(years_js, years_offset_js)

# subset residuals for only years in common
residuals_offset_js <- residuals_offset[dat$Year2 %in% common_years_js]
residuals_js_aligned <- residuals_js[dat$Year2 %in% common_years_js]
valid_pairs_js <- !is.na(residuals_offset_js) & !is.na(residuals_js_aligned)
residuals_offset_js_clean <- residuals_offset_js[valid_pairs_js]
residuals_js_clean <- residuals_js_aligned[valid_pairs_js]

# correlation
cor(residuals_offset_js_clean, residuals_js_clean)
# -0.2290545


## 2.2. adult survival
cor(residuals_offset, residuals_as)
# -0.02199156


## 2.3. reproductive success
years_rs <- dat$Year2[!is.na(residuals_rs)]
common_years_rs <- intersect(years_rs, years_offset_js)
residuals_offset_rs <- residuals_offset[dat$Year2 %in% common_years_rs]
residuals_rs_aligned <- residuals_rs[dat$Year2 %in% common_years_rs]
valid_pairs_rs <- !is.na(residuals_offset_rs) & !is.na(residuals_rs_aligned)
residuals_offset_rs_clean <- residuals_offset_rs[valid_pairs_rs]
residuals_rs_clean <- residuals_rs_aligned[valid_pairs_rs]
cor(residuals_offset_rs_clean, residuals_rs_clean)
#  -0.3480372


## 2.4. pop growth
cor(residuals_offset, residuals_lambda)
# -0.07252234



#### Create a new data frame with the Year2 column and the residuals
# Create a new data frame with Year2 as the reference
residuals_df <- data.frame(Year2 = dat$Year2)

# Add residuals for genomic offset 
residuals_df$Residuals_Offset <- residuals_offset

# For Juvenile Survival, match the residuals with the correct years
# Extract the actual years from the indices of residuals_js
actual_years_js <- dat$Year2[as.numeric(names(residuals_js))]

# Now match these years with Year2 in the residuals_df and fill in the residuals
residuals_df$Residuals_JS <- NA
residuals_df$Residuals_JS[residuals_df$Year2 %in% actual_years_js] <- residuals_js

# Repeat for other residuals
# For Adult Survival
actual_years_as <- dat$Year2[as.numeric(names(residuals_as))]
residuals_df$Residuals_AS <- NA
residuals_df$Residuals_AS[residuals_df$Year2 %in% actual_years_as] <- residuals_as

# For Reproductive Success
actual_years_rs <- dat$Year2[as.numeric(names(residuals_rs))]
residuals_df$Residuals_RS <- NA
residuals_df$Residuals_RS[residuals_df$Year2 %in% actual_years_rs] <- residuals_rs

# Add residuals for Population Growth (already matches Year2)
actual_years_lambda <- dat$Year2[as.numeric(names(residuals_lambda))]
residuals_df$Residuals_Lambda <- NA
residuals_df$Residuals_Lambda[residuals_df$Year2 %in% actual_years_lambda] <- residuals_lambda


residuals_df



####### CCF WITH DETRENDED DATA ######
#directly measures the correlation between the two time series at various lags

# ~lambda~ #
dat_sub <- residuals_df %>%
  dplyr::select(Year2, Residuals_Offset, Residuals_Lambda)
dat_sub <- na.omit(dat_sub)

#CCF
ccf_lambda <- ccf(dat_sub$Residuals_Offset, dat_sub$Residuals_Lambda , lag.max=10, type="correlation")
plot(ccf_lambda)
ccf_data <- data.frame(
  lag = ccf_lambda$lag,
  acf = ccf_lambda$acf
)
ccf_data

# ~Adult Survival~ # 
dat_sub <- residuals_df %>%
  dplyr::select(Year2, Residuals_Offset, Residuals_AS) #Ad_fem_sur
dat_sub <- na.omit(dat_sub)

#CCF
ccf_ad <- ccf(dat_sub$Residuals_Offset, dat_sub$Residuals_AS , lag.max=10, type="correlation")
plot(ccf_ad)
ccf_data <- data.frame(
  lag = ccf_ad$lag,
  acf = ccf_ad$acf
)
ccf_data

# ~Juv Survival~ # 
dat_sub <- residuals_df %>%
  dplyr::select(Year2, Residuals_Offset, Residuals_JS)
dat_sub <- na.omit(dat_sub)

#CCF
ccf_juv <- ccf(dat_sub$Residuals_Offset, dat_sub$Residuals_JS , lag.max=10, type="correlation")
plot(ccf_juv)
ccf_data <- data.frame(
  lag = ccf_juv$lag,
  acf = ccf_juv$acf
)
ccf_data


# ~Repro success~ # 
dat_sub <- residuals_df %>%
  dplyr::select(Year2, Residuals_Offset, Residuals_RS)
dat_sub <- na.omit(dat_sub)

#CCF
ccf_rs <- ccf(dat_sub$Residuals_Offset, dat_sub$Residuals_RS , lag.max=10, type="correlation")
plot(ccf_rs)
ccf_data <- data.frame(
  lag = ccf_rs$lag,
  acf = ccf_rs$acf
)
ccf_data

#***************************************************# 


# Create a data frame to store all CCF results
ccf_results <- data.frame(
  Lag = ccf_lambda$lag,  # Lag is the same across all CCF analyses
  Lambda_CCF = ccf_lambda$acf,  # Store CCF for lambda
  Adult_Survival_CCF = ccf_ad$acf,  # Store CCF for adult survival
  Juvenile_Survival_CCF = ccf_juv$acf,  # Store CCF for juvenile survival
  Repro_Success_CCF = ccf_rs$acf  # Store CCF for reproductive success
)

# View the table of CCF results
print(ccf_results)

# Optionally, you can save it as a CSV file for further analysis or reporting
write.csv(ccf_results, "/Users/katherine/Documents/Projects/SOSP_AmNat/output_tables/CCF_detrended_results.csv", row.names = FALSE)


#***************************************************# 

# Initialize an empty dataframe to store the correlation results in the required format
detrended_correlation_results <- data.frame(
  Lag = integer(),
  Lambda = numeric(),
  Lambda_p_value = numeric(),
  Adult_Survival = numeric(),
  Adult_Survival_p_value = numeric(),
  Juvenile_Survival = numeric(),
  Juvenile_Survival_p_value = numeric(),
  Reproductive_Success = numeric(),
  Reproductive_Success_p_value = numeric(),
  stringsAsFactors = FALSE
)

# Loop through each var and lag for detrended results
variables <- c("Residuals_Lambda", "Residuals_AS", "Residuals_JS", "Residuals_RS")
variable_labels <- c("Residuals_Lambda", "Residuals_AS", "Residuals_JS", "Residuals_RS")
lags <- 1:3

# Temporary list to store correlations for each variable
results_list <- list(Lambda = numeric(3), Lambda_p_value = numeric(3), 
                     Adult_Survival = numeric(3), Adult_Survival_p_value = numeric(3),
                     Juvenile_Survival = numeric(3), Juvenile_Survival_p_value = numeric(3),
                     Reproductive_Success = numeric(3), Reproductive_Success_p_value = numeric(3))

for (lag in lags) {
  # Loop through the variables and calculate the correlations and p-values
  for (var in variables) {
    # Lag the detrended residuals variable by the specified lag
    residuals_df[[paste0(var, "_lag", lag)]] <- dplyr::lead(residuals_df[[var]], n = lag)
    
    # Subset data & remove NAs specific to the current variable and lag
    df_lagged <- residuals_df %>%
      dplyr::select(Year2, Residuals_Offset, !!sym(paste0(var, "_lag", lag))) %>%
      na.omit()
    
    # Perform Pearson correlation and calculate p-value using cor.test()
    cor_test_result <- cor.test(df_lagged$Residuals_Offset, df_lagged[[paste0(var, "_lag", lag)]], use = "complete.obs")
    correlation <- cor_test_result$estimate
    p_value <- cor_test_result$p.value
    
    # Store the results for each variable and lag
    if (var == "Residuals_Lambda") {
      results_list$Lambda[lag] <- round(correlation, 3)
      results_list$Lambda_p_value[lag] <- round(p_value, 5)
    } else if (var == "Residuals_AS") {
      results_list$Adult_Survival[lag] <- round(correlation, 3)
      results_list$Adult_Survival_p_value[lag] <- round(p_value, 5)
    } else if (var == "Residuals_JS") {
      results_list$Juvenile_Survival[lag] <- round(correlation, 3)
      results_list$Juvenile_Survival_p_value[lag] <- round(p_value, 5)
    } else if (var == "Residuals_RS") {
      results_list$Reproductive_Success[lag] <- round(correlation, 3)
      results_list$Reproductive_Success_p_value[lag] <- round(p_value, 5)
    }
  }
  
  # Append results for the current lag to the final dataframe
  detrended_correlation_results <- rbind(detrended_correlation_results, data.frame(
    Lag = lag,
    Lambda = results_list$Lambda[lag],
    Lambda_p_value = results_list$Lambda_p_value[lag],
    Adult_Survival = results_list$Adult_Survival[lag],
    Adult_Survival_p_value = results_list$Adult_Survival_p_value[lag],
    Juvenile_Survival = results_list$Juvenile_Survival[lag],
    Juvenile_Survival_p_value = results_list$Juvenile_Survival_p_value[lag],
    Reproductive_Success = results_list$Reproductive_Success[lag],
    Reproductive_Success_p_value = results_list$Reproductive_Success_p_value[lag]
  ))
}

detrended_correlation_results

# Save the formatted detrended correlation results to a CSV file
write.csv(detrended_correlation_results, "/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/detrended_correlation_results.csv", row.names = FALSE)



#***************************************************# 
####     5. SCATTER PLOTS  - DETRENDED        ####
#***************************************************# 
residuals_df

#loop through each var and lag
variables <- c("Residuals_Lambda", "Residuals_AS", "Residuals_JS", "Residuals_RS")
variable_labels <- c("Population Growth Rate", "Adult Survival", "Juvenile Survival", "Reproductive Success")
lags <- 1:3

#list for storing the plots
plot_list <- list()

for (var in variables) {
  for (lag in lags) {
    #lagged var column
    residuals_df[[paste0(var, "_lag", lag)]] <- dplyr::lead(residuals_df[[var]], n = lag)
    
    #subset data & remove NAs specific to current var and lag
    df_lagged <- residuals_df %>%
      dplyr::select(Year2, Residuals_Offset, !!sym(paste0(var, "_lag", lag))) %>%
      na.omit() 
    
    # check if the dataframe is mismatched 
    if (nrow(df_lagged) == 0) {
      print(paste("No valid data for", var, "at lag", lag))
      next
    }
    
    #calc corr
    correlation <- cor(df_lagged$Residuals_Offset, df_lagged[[paste0(var, "_lag", lag)]], use = "complete.obs")
    
    #now plot
    plot <- ggplot(df_lagged, aes(x = Residuals_Offset, y = !!sym(paste0(var, "_lag", lag)))) +
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
combined_plot_dt <- plot_grid(
  plot_list$Residuals_Lambda_lag1, plot_list$Residuals_Lambda_lag2, plot_list$Residuals_Lambda_lag3,
  plot_list$Residuals_AS_lag1, plot_list$Residuals_AS_lag2, plot_list$Residuals_AS_lag3,
  plot_list$Residuals_JS_lag1, plot_list$Residuals_JS_lag2, plot_list$Residuals_JS_lag3,
  plot_list$Residuals_RS_lag1, plot_list$Residuals_RS_lag2, plot_list$Residuals_RS_lag3,
  ncol = 3, align = "hv"
)

combined_plot_dt


ggsave("/Users/katherine/Documents/Projects/SOSP_AmNat/output_figures/combined_scatterplots_dt_3years.svg", 
       plot = combined_plot_dt, width = 10, height = 12)

