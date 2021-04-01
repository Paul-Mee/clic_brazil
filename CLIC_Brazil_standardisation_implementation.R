### Data clean up

rm(list=ls())

##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

library(dplyr)
library(sf)

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))

# set seed as some processes (bed simulator are stochastic)
set.seed(1234)




########################
# Part 1 Brazil
########################


# 2A: cases
load(paste0(dir_formatted_case_data,"Brazil_case_timeseries_clean.RData"))

# 2B: age distribution of Covid-19 cases
load(paste0(dir_case_age_dist,"Brazil_case_age_clean.RData"))

# 2C: age distribution of the population
load(paste0(dir_pop_age_dist,"Brazil_pop_age_clean.RData"))

# 2D: timing of interventions
load(paste0(dir_interventions,"Brazil_interventions_clean.RData"))

# 2E contextual region data
region_tab <- Brazil_age_dist[, 1:2]
colnames(region_tab) = c("Area", "Region")

# 2F Brazil deaths data
load(paste0(dir_formatted_death_data,"Brazil_deaths_timeseries_clean.RData"))

# 2G Brazil sociodemographic index and population density
load(paste0(dir_covariates,"Brazil_mun_covs.RData"))

# 2H Updated length of stay distributions
load(paste0(dir_data_objects,"LOS_Brazil_data.RData"))
load(paste0(dir_data_objects,"LOS_ITU_Brazil_data.RData"))
LOS <- function(times) sample(LOS_absolute, times, replace = T)
LOS_ITU <- function(times) sample(LOS_ITU_absolute, times, replace = T)

prop_hosp <- 0.920202
prop_ITU <- 0.3807172

# 2I Geogrpahical information of municipality centroids
Text_IBGE <- st_read(paste0(dir_ibge_data,"Brazil_AD2_shape.shp"))


# run standardisation for all districts
BigStandard <- Big.standardise(c_dat = Brazil_cases,
                               deaths_dat = Brazil_deaths,
                               age_dist = Brazil_age_dist,
                               case_age_dist = Brazil_case_age_dist,
                               Intervention = Intervention,
                               covariates = SDI,
                               prop_hosp = prop_hosp,
                               prop_ITU = prop_ITU)

save(BigStandard, file = paste0(dir_data_objects,"Brazil_BigStandard_results_",
                                gsub("-", "_", Sys.Date()),
                                ".RData"))



