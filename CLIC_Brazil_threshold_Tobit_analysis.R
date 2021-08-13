####
### Script to load lm_data prepared using PM_multivar_anal_lm_data_prep_v#.R
### and carry out Tobit regression analysis 
####

rm(list=ls())




##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))



# Main packages
library("tidyverse")
library("dplyr")
library("ggplot2")
library("survival")
library("survminer")
library("finalfit")
library("corrplot") 
library("lubridate") 
library("MASS")
library("gridExtra") 
library("rms")
library("reshape2")
library("pammtools")
library("GGally")
library("VGAM")
library("bestNormalize")
library("writexl")
library("broom")
library("data.table")


### Functions 
### Source multivar functions
source(paste0(dir_scripts,"CLIC_Brazil_multivar_functions.R"))

### Simple function to get Tobit model output

### Interpretation of tobit models
### https://stats.idre.ucla.edu/r/dae/tobit-models/




std_case_dat_lm <- readRDS(file=paste0(dir_Rt_data,"Brazil_lm_covariates_fail_10.RDS"))
## Basic Tobit model 



# Max value of days_end_num for censoring
std_case_dat_lm$days_end_num <- as.integer(std_case_dat_lm$days_end)
max_days = as.integer(max(std_case_dat_lm$days_end_num))





covar_all <- c( "geo_region_factor", "log_popden","Piped_water_percent",
                "Sewage_or_septic_percent", "log_travel_time_hours", "SDI_index")

covar_cont <- c( "log_popden","Piped_water_percent",
                "Sewage_or_septic_percent", "log_travel_time_hours", "SDI_index")

## geo region as a factor 

std_case_dat_lm$geo_region_factor <- as.factor(std_case_dat_lm$geo_region)

## 1) Investigating bivariate relationships in the dataset
correlations.dat <- as.data.frame(cor(std_case_dat_lm[, covar_cont]))

## output as an excel file

writexl::write_xlsx(correlations.dat,paste0(dir_results,"lm_correlations.xlsx"))

## scaling variables to avoid very small coeeficients 
# std_case_dat_lm$Piped_water_percent <- std_case_dat_lm$Piped_water_percen/1000
# std_case_dat_lm$Sewage_or_septic_percent <- std_case_dat_lm$Sewage_or_septic_percent/1000

#covar_cont <- c( "log_popden","Piped_water_percent_1000",
#                 "Sewage_or_septic_percent_1000", "log_travel_time_hours", "SDI")

### Variable summary 

summary.dat <- summ_tab(std_case_dat_lm,covar_cont,2)


### add geo factors

tmp.dat <- as.data.frame(table(std_case_dat_lm$geo_region))

names(tmp.dat)[1] <- "term"
names(tmp.dat)[2] <- "dist"

tmp.dat$term <- paste0("geo_region_factor",tmp.dat$term)

summary.dat <- rbind(tmp.dat,summary.dat)

## keep row numbers
summary.dat <-  as.data.frame(data.table::setDT(summary.dat, keep.rownames = TRUE)[])


### Model building strategy - Basic model based on geo region - develop adjusted model using forward stepwise introduction of variables 
### and ANOVA to assess which variables contribute over and above geo region 

### Model for geo region 
mod1 <- vglm(days_end_num ~ geo_region_factor, tobit(Upper = max_days), data = std_case_dat_lm)
# 


### Univariate analyses adding each variable individually

 for (i in 1:length(covar_all)) { 
   print(covar_all[i])
   model <- paste0("days_end_num ~  " , covar_all[i] )
   print(model)
   mod1_u <- vglm(model, tobit(Upper = max_days), data = std_case_dat_lm)
   if (i==1){tmp.dat <- tobit_tab(mod1_u,1,2)
        ### Select required covariate 
        tmp1.dat <- tmp.dat[which( (grepl(covar_all[i], tmp.dat$term, fixed=TRUE))==TRUE),] 
        }
   if (i>1){tmp.dat <- tobit_tab(mod1_u,1,2) 
   ### Select required covariate 
   tmp.dat <- tmp.dat[which( (grepl(covar_all[i], tmp.dat$term, fixed=TRUE))==TRUE),] 
   tmp1.dat <- rbind(tmp1.dat,tmp.dat) 
    }

 }
## set row names to 1st column

#tmp1.dat <- as.data.frame(data.table::setDT(tmp.dat, keep.rownames = TRUE)[])
univar_table.dat <- tmp1.dat[c(1,2,9,10,8)]

### Travel time and piped water rate not associated in univariate analyses 

#### Look at adding individual variables to build multivariate model ....
## Add the following
# covar_cont <- c( "log_popden","Piped_water_percent",
#                  "Sewage_or_septic_percent", "log_travel_time_hours", "SDI")

### Base model 
mod1 <- vglm(days_end_num ~ geo_region_factor , tobit(Upper = max_days), data = std_case_dat_lm)

### LR test to compare models 
## 1) Add population density 
mod2 <- vglm(days_end_num ~ geo_region_factor + log_popden, tobit(Upper = max_days), data = std_case_dat_lm)
pchisq(2 * (logLik(mod2) - logLik(mod1)), df = 2, lower.tail = FALSE)
### Keep log_popden

## 2) Add piped water
mod3 <- vglm(days_end_num ~ geo_region_factor + log_popden   + Piped_water_percent , tobit(Upper = max_days), data = std_case_dat_lm)
pchisq(2 * (logLik(mod3) - logLik(mod2)), df = 2, lower.tail = FALSE)
## keep piped water 

## 2b Sewage instead of piped water 
# mod3b <- vglm(days_end_num ~ geo_region_factor + log_popden   + Sewage_or_septic_percent , tobit(Upper = max_days), data = std_case_dat_lm)
# pchisq(2 * (logLik(mod3b) - logLik(mod2)), df = 2, lower.tail = FALSE)
# 
# ## 2c Add piped water
# mod3c <- vglm(days_end_num ~ geo_region_factor + log_popden   + Sewage_or_septic_percent + Piped_water_percent , tobit(Upper = max_days), data = std_case_dat_lm)
# pchisq(2 * (logLik(mod3c) - logLik(mod3b)), df = 2, lower.tail = FALSE)

### Using this order of variables you would drop piped water not sewage ? Therefore keep both 


## 3) Add Sewage
mod5 <- vglm(days_end_num ~ geo_region_factor + log_popden    
                   + Piped_water_percent+ Sewage_or_septic_percent , tobit(Upper = max_days), data = std_case_dat_lm)
pchisq(2 * (logLik(mod5) - logLik(mod3)), df = 2, lower.tail = FALSE)
## keep Sewage

## 4) add Travel time
mod6 <- vglm(days_end_num ~ geo_region_factor + log_popden  + Piped_water_percent + Sewage_or_septic_percent
                            + log_travel_time_hours, tobit(Upper = max_days), data = std_case_dat_lm)
pchisq(2 * (logLik(mod6) - logLik(mod5)), df = 2, lower.tail = FALSE)
## Keep  Travel time

## 5) Add SDI 
mod7 <- vglm(days_end_num ~ geo_region_factor + log_popden  + Piped_water_percent + Sewage_or_septic_percent
             + log_travel_time_hours + SDI_index , tobit(Upper = max_days), data = std_case_dat_lm)
pchisq(2 * (logLik(mod6) - logLik(mod5)), df = 2, lower.tail = FALSE)
## Keep  SDI

## 6) Alternative model with all variables 
# mod7 <- vglm(days_end_num ~ geo_region_factor + log_popden  + Piped_water_percent + Sewage_or_septic_percent
#              + log_travel_time_hours +  + SDI , tobit(Upper = max_days), data = std_case_dat_lm)

## Looking at components of SDI 
#mod7 <- vglm(days_end_num ~ geo_region_factor + log_popden  + Piped_water_percent + Sewage_or_septic_percent
#            + log_travel_time_hours +   Fert_rate + GDP_pc_2018 + Mean_mother_edu  , tobit(Upper = max_days), data = std_case_dat_lm)


### Output final model
tmp.dat <- tobit_tab(mod7,1,2)

multivar_table.dat <- tmp.dat[c(1,2,9,10,8)]


### Combined model for tabular output

tab1.dat <- merge(summary.dat,univar_table.dat,by="term",all.x=TRUE)

### Merge tab1 and multivar into one dataframe

final_table.dat <- merge(tab1.dat,multivar_table.dat,by="term",all.x=TRUE,all.y=TRUE)

### Sort by row number 

final_table.dat <- dplyr::arrange(final_table.dat,as.numeric(rn))

### formatting


### Fix Confidence intervals

final_table.dat$est_CI95_Uni  <- paste0(as.character(final_table.dat$Estimate.x)," [",
                                        as.character(final_table.dat$CI_2.5.x),",",as.character(final_table.dat$CI_97.5.x),"] ")

final_table.dat$est_CI95_Multi  <- paste0(as.character(final_table.dat$Estimate.y)," [",
                                        as.character(final_table.dat$CI_2.5.y),",",as.character(final_table.dat$CI_97.5.y),"] ")

## Fix NAs
final_table.dat$na_flag <- grepl( "NA",final_table.dat$est_CI95_Uni, fixed = TRUE)
final_table.dat$est_CI95_Uni <- ifelse(final_table.dat$na_flag==TRUE, "", final_table.dat$est_CI95_Uni)
final_table.dat$pvals.x <- ifelse(final_table.dat$na_flag==TRUE, "", final_table.dat$pvals.x)

final_table.dat$na_flag <- grepl( "NA",final_table.dat$est_CI95_Multi, fixed = TRUE)
final_table.dat$est_CI95_Multi <- ifelse(final_table.dat$na_flag==TRUE, "", final_table.dat$est_CI95_Multi)
final_table.dat$pvals.y <- ifelse(final_table.dat$na_flag==TRUE, "", final_table.dat$pvals.y)


final_table.dat <- final_table.dat[c(1,3,12,7,13,11)]

names(final_table.dat)[1] <- "Variable"
names(final_table.dat)[2] <- "Med_IQR_N"
names(final_table.dat)[3] <- "Univariate_Estimate_CI"
names(final_table.dat)[4] <- "Univariate_p_value"
names(final_table.dat)[5] <- "Multivariate_Estimate_CI"
names(final_table.dat)[6] <- "Multivariate_p_value"


### To preserve the format
#final_table.dat <- final_table.dat %>% mutate_all(as.character)
## Fix p value estimates
final_table.dat$Univariate_p_value <- ifelse(final_table.dat$Univariate_p_value=="0.00", "<0.01", final_table.dat$Univariate_p_value)
final_table.dat$Multivariate_p_value <- ifelse(final_table.dat$Multivariate_p_value=="0.00", "<0.01", final_table.dat$Multivariate_p_value)
#final_table.dat

writexl::write_xlsx(final_table.dat,paste0(dir_results,"tobit_final_model_10.xlsx"))
