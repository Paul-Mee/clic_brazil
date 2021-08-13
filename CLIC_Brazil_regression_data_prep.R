####
### This version of the code prepares the data required for the linear regression 
### and Tobit analyses of Mean Rt and Time to peak incidence
### V1.0 PM 13/01/2021
####

rm(list=ls())



##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))


# Main packages
library("tidyverse")
library("dplyr")
library("corrplot") 
library("lubridate") 



### Fetch latest data set 
#load(fetch_latest(fileDir = dir_data_objects,
#                  type = "BigStandard"))

# update for revised analysis for the paper
load(paste0(dir_data_objects,"Brazil_BigStandard_results_16_07_21.RData"))
#load(paste0(dir_data_objects,"Brazil_BigStandard_results_2021_01_14.RData"))

std_case_dat <- BigStandard$standardised_incidence


### Censoring 
#cens_date = as.Date("2021-01-14")
cens_date = as.Date("2021-07-16")

### Data up to censoring date - for paper 
std_case_dat <- std_case_dat [which(std_case_dat$date_end<=format(as.Date(cens_date), "%Y-%m-%d"))  ,] 


# read municipality sociodemogrpahics data

load(paste0(dir_covariates,"Brazil_mun_covs_advar.RData"))

### Add additional covariates
std_case_dat <- merge(std_case_dat,SDI,by.x="Area",by.y="Area_Name", all.x=TRUE)



#std_case_dat <- std_case_dat %>% dplyr::select(Area,IBGE,date_end,cum_cases,standardised_cases,standardised_deaths,
#                                         popden,Piped_water,Sewage_or_septic,Travel_time,GDP_pc_2018,unadj_fert_rate,unadj_Mean_years_edu,SDI_index)

std_case_dat <- std_case_dat %>% dplyr::select(Area,IBGE,date_end,cum_cases,standardised_cases,standardised_deaths,
                                         popden,Piped_water,Sewage_or_septic,Travel_time,GDP_pc_2018,unadj_fert_rate,unadj_Mean_years_edu,SDI_index)


## Drop .x suffix
#colnames(std_case_dat) <- gsub('.x','',colnames(std_case_dat))

## Convert to numeric - first remove white space and blank
std_case_dat$GDP_pc_2018 <- gsub(' ','',std_case_dat$GDP_pc_2018)
std_case_dat$GDP_pc_2018 <- gsub(',','',std_case_dat$GDP_pc_2018)
std_case_dat$GDP_pc_2018 <- as.numeric(std_case_dat$GDP_pc_2018)

### Fix column names 

names(std_case_dat)[12] <- "Fert_rate"
names(std_case_dat)[13] <- "Mean_mother_edu"

std_case_dat$Area_char <- as.character(std_case_dat$Area)


## State as factor
## Get state data 
std_case_dat$Region <- sub('.*_', '', std_case_dat$Area_char)
std_case_dat$region.f <- as.factor(std_case_dat$Region)
levels(std_case_dat$region.f)
# ## Set Sao Paolo as the reference level 
std_case_dat <- within(std_case_dat, region.f <- relevel(region.f, ref = 26))
levels(std_case_dat$region.f)

### Adding data on regional groupings of states
br_state_region <- read.csv(paste0(dir_geo_data,"brazil_states_regions.csv"))
names(br_state_region)[1] <- "state"
names(br_state_region)[2] <- "geo_region"
std_case_dat <-  merge( x=std_case_dat, y=br_state_region , by.x="region.f" , by.y="state")

std_case_dat$log_popden <- log(std_case_dat$popden)
std_case_dat$log_travel_time_hours <- log(std_case_dat$Travel_time/60) 

### Changing scale of variables for more meaningful interpretation 

std_case_dat$Piped_water_percent <- std_case_dat$Piped_water*100
std_case_dat$Sewage_or_septic_percent <- std_case_dat$Sewage_or_septic*100


#Select first n places use for debugging
#std_case_dat$Area_number <- as.numeric(as.factor(std_case_dat$Area))
#std_case_dat<- std_case_dat[ which(std_case_dat$Area_number <= 200) ,]


#### Gen failure variable and origin time
## Start of epidemic = 0.1 standardised case per 1000
## Standardised case units = per 1000 (i.e. 1 per 10000 = 0.1)

min_cases <- 0.1
## Fail epidemic = 10 - cases per 10000
# Will do sensitivity analysis around this 
fail_cases <- 1.0
#fail_cases <- 1.5
## epidemic start date 

tmp.dat <- std_case_dat[which( std_case_dat$standardised_cases>=min_cases),]
start_date <- min(tmp.dat$date_end)



### Failure variable
std_case_dat$fail <- 0
std_case_dat$fail[  std_case_dat$standardised_cases >= fail_cases] <- 1



max_days  <- as.integer(difftime(cens_date ,start_date , units = c("days")))


### Generate time variables (days since first case threshold passed)
 std_case_dat$days_end <- std_case_dat$date_end - start_date + 1 

 ### Censoring
 std_case_dat_cens <-  std_case_dat %>% dplyr::filter(std_case_dat$date_end <= cens_date) 
 
 
 
 ### Select days_end for all places that fail 

 ### For places that fail select first failure day 
 ### Select row for first day of failure 
 ## all failures
 std_case_dat_fail <- std_case_dat_cens[which(std_case_dat_cens$fail==1),]
 # order by place and days 
 std_case_dat_fail  <- std_case_dat_fail[order(std_case_dat_fail$Area,std_case_dat_fail$days),] 
 # Select first failure
 std_case_dat_fail$seq_val <- ave( std_case_dat_fail$days_end, std_case_dat_fail$Area, FUN = seq_along)
 std_case_dat_fail <- std_case_dat_fail[which(std_case_dat_fail$seq_val==1),]
 ## drop seq_val
 std_case_dat_fail <- subset( std_case_dat_fail, select = -c(seq_val))
 
 
 # For Tobit analysis days_end should be set to max day for all places that have not yet reached the threshold
 ### For places that do not fail select last day
 
 std_case_dat_not_fail <- std_case_dat_cens
 ## Select last day 
 std_case_dat_not_fail <- std_case_dat_not_fail %>% group_by(Area) %>% slice(which.max(date_end))
 ## Select those that do not fail
 std_case_dat_not_fail <- std_case_dat_not_fail[which(std_case_dat_not_fail$fail==0),]
 ## Check that they have reached censor date 
 std_case_dat_not_fail  <-  std_case_dat_not_fail %>% dplyr::filter(date_end == cens_date) 
 
 
 ## Row bind std_case_dat_fail and std_case_dat_fail
 
 std_case_dat_lm  <- rbind(std_case_dat_fail,std_case_dat_not_fail)
 

 ## Drop rows with missing covariate data 
 
 std_case_dat_lm  <- std_case_dat_lm  %>% dplyr::filter(!(is.na(std_case_dat_lm$log_popden))) 





saveRDS(std_case_dat_lm,file=paste0(dir_Rt_data,"Brazil_lm_covariates_fail_10.RDS"))
#saveRDS(std_case_dat_lm,file=paste0(dir_Rt_data,"Brazil_lm_covariates_fail_5.RDS"))
#saveRDS(std_case_dat_lm,file=paste0(dir_Rt_data,"Brazil_lm_covariates_fail_15.RDS"))
 
