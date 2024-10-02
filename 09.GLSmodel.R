# time series analysis to genomic offset and population demography 
# katherine carbeck
# 22 aug 2024

setwd("/Users/katherine/Documents/Projects/SOSP_AmNat/data/mandarteOffsetOutput")
library(tidyverse)
library(nlme)


#### 1. data ####
dat <- read.csv("Mandarte_genomic_offset_output_AND_demography_AND_Climate.csv")
str(dat)


##### 2. test genomic offset effects on population trend ####
# genomic offset with a 2-year lag influences population growth rate
dat_sub <- dat
dat_sub$GenomicOffset_lag2 <- dplyr::lag(dat$GenomicOffset, n = 2)
dat_sub$GenomicOffset_lag1 <- dplyr::lag(dat$GenomicOffset, n = 1)

dat_sub_lambda <- dat_sub %>%
  dplyr::select(Year2, GenomicOffset, GenomicOffset_lag1, GenomicOffset_lag2, Lambda)

dat_sub_lambda <- na.omit(dat_sub_lambda)

#hypothesis: Genomic offset two years prior (GenomicOffset_lag2) has a significant effect on population growth rate
gls_lambda <- gls(Lambda ~ GenomicOffset_lag2 ,
                  data = dat_sub_lambda,
                  correlation = corAR1(form = ~ Year2))
summary(gls_lambda)
plot(gls_lambda)

residuals_gls <- residuals(gls_lambda)
acf(residuals_gls)


##### 3. test genomic offset effects on reproductive success ####
dat_sub <- dat
dat_sub$GenomicOffset_lag1 <- dplyr::lag(dat$GenomicOffset, n = 1)
head(dat_sub)

dat_sub_rs <- dat_sub %>%
  dplyr::select(Year2, GenomicOffset, GenomicOffset_lag1, mean_indep_young_per_female,sd_indep_young_per_female)

dat_sub_rs <- na.omit(dat_sub_rs)


gls_rs <- gls(mean_indep_young_per_female ~ GenomicOffset_lag1,
              data = dat_sub_rs,
              correlation = corAR1(form = ~ Year2)) 
summary(gls_rs) 
plot(gls_rs)
residuals_gls <- residuals(gls_rs)
qqnorm(residuals_gls)
qqline(residuals_gls, col = "red") 
hist(residuals_gls, breaks = 20, xlab = "Residuals")
acf(residuals_gls)


##### 4. test genomic offset effects on juv survival ####
dat_sub <- dat
dat_sub$GenomicOFfset_lag2 <- dplyr::lag(dat$GenomicOffset, n = 2)
dat_sub$GenomicOFfset_lag1 <- dplyr::lag(dat$GenomicOffset, n = 1)

dat_sub_juv <- dat_sub %>%
  dplyr::select(Year2, GenomicOffset, GenomicOffset_lag2, Juv_surv_fledglings)

dat_sub_juv <- na.omit(dat_sub_juv)

gls_juv <- gls(Juv_surv_fledglings ~ GenomicOffset_lag2  , 
               data = dat_sub_juv,
               correlation = corAR1(form = ~ Year2)) 
summary(gls_juv)
plot(gls_juv)
residuals_gls <- residuals(gls_juv)
qqnorm(gls_juv)
qqline(residuals_gls, col = "red") 
hist(residuals_gls, breaks = 20, xlab = "Residuals")
acf(residuals_gls)


##### 5. test genomic offset effects on adult survival ####

dat_sub <- dat
dat_sub$GenomicOffset_lag3 <- dplyr::lag(dat$GenomicOffset, n = 3)
dat_sub$GenomicOffset_lag2 <- dplyr::lag(dat$GenomicOffset, n = 2)
dat_sub$GenomicOffset_lag1 <- dplyr::lag(dat$GenomicOffset, n = 1)

dat_sub_ad <- dat_sub %>%
  dplyr::select(Year2, GenomicOffset, GenomicOffset_lag1, GenomicOffset_lag2,GenomicOffset_lag3, Ad_fem_sur)

dat_sub_ad <- na.omit(dat_sub_ad)

gls_ad <- gls(Ad_fem_sur ~ GenomicOffset_lag3  , 
               data = dat_sub_ad,
               correlation = corAR1(form = ~ Year2)) 
summary(gls_ad)
plot(gls_ad)
residuals_gls <- residuals(gls_ad)
qqnorm(gls_ad)
qqline(residuals_gls, col = "red") 
hist(residuals_gls, breaks = 20, xlab = "Residuals")
acf(residuals_gls)



