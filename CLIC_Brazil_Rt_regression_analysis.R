####
### Script to run  regression analyses
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
library("MASS")
library("lmtest")
library("broom")
library("emmeans")
library("reshape")
library("plot3D")
library("gridExtra")
library("finalfit")
library("writexl")
library("tidyr")
library("multcomp")
library("Epi") 


### Functions 
### Source multivar functions
source(paste0(dir_scripts,"CLIC_Brazil_multivar_functions.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))

### Load Rt prediction data 


#rt_all_dat <- readRDS("CC_data/City_Case_data/Brazil/Brazil_formatted/Rt_Data/Brazil_rt_prediction-current.RDS")

#### Repalce this with censored data 
rt_all_dat <- readRDS(paste0(dir_Rt_data,"Brazil_rt_prediction-current_cens_jan14.RDS"))

# Sample 10 places at random

# set seed so I can repeat this
# set.seed(99)
# # arbitrary number to sample
# n <- 10
# # equal weighting to cities and with no replacement
# smpl <- sample(unique(rt_all_dat$city_state), n)
# rt_all_dat_subset <- rt_all_dat[rt_all_dat$city_state %in% smpl, ]


### Drop zero values

# ggplot(data=rt_all_dat_subset2,aes(x = Date, y = Rtotalhat, color=city_state, group=city_state))+
#   geom_line() +
#   xlab("Date") +
#   ylab("Rt") +
#   scale_x_date(date_breaks = "1 week", date_labels="%d-%b") +
#   #coord_cartesian(ylim = c(0.0, 3.0))

#ggsave("PM_test_results/Rt_GT0_random_10_all.png", width=40, height=16, units="cm")

### Need to define start date for analysis as local arrival = 1 case per 10000
### Merge with standardised case data 


### Fetch latest data set 
load(fetch_latest(fileDir = dir_data_objects,
                  type = "BigStandard"))

std_case_dat <- BigStandard$standardised_incidence

## Keep place date and standardised incidence 

tmp_case_dat <- std_case_dat[c("Area", "date_end", "standardised_cases")]

## Identify date where each place reaches 1 case per 10000 (0.1 cases per 1000) )
## Keep row for first day passing threshold

start_case_dat <- tmp_case_dat %>% 
    group_by(Area) %>% 
    filter(standardised_cases > 0.1) %>% 
    slice(1)

## merge to rt_all_dat on date and place 
names(start_case_dat)[2] <- "Start_Date"
start_case_dat <- start_case_dat[c("Area", "Start_Date")]

rt_merge_dat <- merge(rt_all_dat,start_case_dat,by.x=c("city_state"),by.y=c("Area"),all.x=TRUE)

rt_merge_dat$day_number <- as.integer(rt_merge_dat$Date - rt_merge_dat$Start_Date)




# tmp2_case_dat <- rt_merge_dat %>% 
#   group_by(city_state) %>% 
#   arrange(city_state,Date) %>% 
#   slice(1)
# 
# #### Histogram of max days to start
# 
#  ggplot(tmp2_case_dat, aes(x=day_number)) + 
#   geom_histogram()
#  
# summary(tmp2_case_dat$day_number)

### Drop Rt predictions of 0 predictions for R0 as artefacts 

rt_gt0_dat <- rt_merge_dat[ which(rt_merge_dat$Rt_Smooth > 0 ) ,]

### Start date = epidemic start date 


tmp.dat <- rt_gt0_dat[c("city_state", "Start_Date")]
date_rt_start_dat <- tmp.dat %>% 
  group_by(city_state) %>% 
  slice(1)


### Mean Rt - over rnage day_start to day_end (days after Rt > 0 )
### All data
day_start <- 30 
day_end <- 180



mean_rt_dat <- rt_gt0_dat  %>%   
               dplyr::filter(day_number>=day_start & day_number <= day_end ) %>%
               group_by(city_state) %>%
               summarise_at(vars( "Rt_Smooth"), mean) 



rt_mean_dat <- merge(date_rt_start_dat,mean_rt_dat,by="city_state")


names(rt_mean_dat)[3] <- "Rt_mean"

### Get day of year as a number 
rt_mean_dat$start_date_day <- lubridate::yday(rt_mean_dat$Start_Date)

saveRDS(rt_mean_dat,file=paste0(dir_Rt_data,"Brazil_mean_Rt.RDS"))

### Merge with other variables
### Covariate data created in PM_multivar_anal_v3.R

covar_dat <- readRDS(paste0(dir_Rt_data,"Brazil_lm_covariates_fail_10.RDS"))
rt_mean_covar_dat <- merge(rt_mean_dat,covar_dat,by.x="city_state",by.y="Area_char",all.x=TRUE)
## geo region as a factor 

rt_mean_covar_dat$geo_region_factor <- as.factor(rt_mean_covar_dat$geo_region)

### Histogram of Rt_mean

# p <- rt_mean_covar_dat %>%
#   ggplot( aes(x=Rt_mean)) +
#   geom_histogram( binwidth=0.05, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#   ggtitle("Rt_mean - bin = 0.05") +
#   theme(
#     plot.title = element_text(size=20)
#   )
# p
# ggsave("PM_test_results/RT_historgram.png",p,  width=20, height=15, units="cm")

#rt_mean_covar_dat$log_Rt_mean <- log(rt_mean_covar_dat$Rt_mean)

### Histogram of log Rt_mean

# p <- rt_mean_covar_dat %>%
#   ggplot( aes(x=log_Rt_mean)) +
#   geom_histogram( binwidth=0.05, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#   ggtitle("log Rt_mean - bin = 0.05") +
#   theme(
#     plot.title = element_text(size=20)
#   )
# p
# ggsave("PM_test_results/log_RT_mean_historgram.png",p,  width=20, height=15, units="cm")

## Histogram of start date day 

# p <- rt_mean_covar_dat %>%
#   ggplot( aes(x=start_date_day)) +
#   geom_histogram( binwidth=5, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
#   ggtitle("Start Date Day  - bin = 5") +
#   theme(
#     plot.title = element_text(size=20)
#   )
# p
# ggsave("PM_test_results/Mid_Day_historgram.png",p,  width=20, height=15, units="cm")

### Categorical variable for start date day  

rt_mean_covar_dat$start_day_group <- cut(
  rt_mean_covar_dat$start_date_day,
  breaks = quantile(rt_mean_covar_dat$start_date_day, c(0, 0.33, 0.66,  1),na.rm = TRUE),
  labels = c("gp1", "gp2", "gp3"),
  right  = FALSE,
  include.lowest = TRUE
)



# Sumary stats 
## This was hand coded - labels could be autoamtically assigned

tapply(rt_mean_covar_dat$Start_Date, rt_mean_covar_dat$start_day_group, summary)

rt_mean_covar_dat$start_day_group <- factor(rt_mean_covar_dat$start_day_group, 
                                            levels = c("gp1", "gp2" , "gp3"),
                                            labels = c("19-Mar to 26-Apr", "27-Apr to 12-May", "13-May to 17-Jul"))



table(rt_mean_covar_dat$start_day_group)

covar_cont <- c( "log_popden","Piped_water_percent",
                 "Sewage_or_septic_percent", "log_travel_time_hours", "SDI_index")

## Drop rows with NA for any continuous covariate 

rt_omit_dat  <- na.omit(rt_mean_covar_dat, cols=covar_cont)


 
 ### Model building 2 - Start with geo region , day and geo_region*day and add other variables


 
covid.mod1 <- lm(Rt_mean ~ geo_region_factor*start_day_group  , data = rt_omit_dat)
summary(covid.mod1)
Epi::ci.lin(covid.mod1)

### interpreting this model 
#emmeans::emtrends(covid.mod1, ~ start_day_group)
covid.mod1b <- lm(Rt_mean ~ start_day_group:geo_region_factor + start_day_group + geo_region_factor , data = rt_omit_dat)
summary(covid.mod1b)
Epi::ci.lin(covid.mod1b)

## ggplot stacked bar chart of coefficient values 
#https://www.r-graph-gallery.com/48-grouped-barplot-with-ggplot2.html
## useful article on intepreting models with interaction terms
# https://www.andrew.cmu.edu/user/achoulde/94842/lectures/lecture10/lecture10-94842.html


### plotting this (line plot)

emmeans::emmip(covid.mod1,  start_day_group ~  geo_region_factor)

### plotting this (bar plot)

# geo_day_dat1 <- as.data.frame(emmeans::emmeans(covid.mod1, ~ start_day_group*geo_region_factor))
# 
# 
# p <- ggplot(data= geo_day_dat1, aes(x=geo_region_factor,y=emmean, fill=start_day_group)) +
#       geom_bar(stat="identity",position="dodge") + 
#       geom_errorbar(position=position_dodge(.9),width=.25, aes(ymax=upper.CL, ymin=lower.CL),alpha=0.3) + 
#       geom_text(aes(label=sprintf("%0.2f", round(emmean, digits = 2))), position=position_dodge(width=0.9), vjust=-0.5,size=3.5) + 
#       labs(x="Geographic region", y="Rt Mean", fill="Mid day",
#        title = "Rt Mean predictions for combinations of Mid day and Region 
#               \n (Unadjusted plot)") + 
#   coord_cartesian(ylim = c(0.9, 1.5) ) 
# p
# ggsave("PM_test_results/lm_Rtmean_geo_bar.png",p,  width=20, height=15, units="cm")


covar_all <- c( "start_day_group" ,"geo_region_factor", "log_popden","Piped_water_percent",
                 "Sewage_or_septic_percent", "log_travel_time_hours", "SDI")

### Getting tabular output 



### Get summary tabl,
summary.dat <- summ_tab(rt_omit_dat,covar_cont,2)
### add  start point day 

tmp.dat <- as.data.frame(table(rt_omit_dat$start_day_group))
names(tmp.dat)[1] <- "term"
names(tmp.dat)[2] <- "dist"

tmp.dat$term <- paste0("start_day_group",tmp.dat$term)

summary.dat <- rbind(tmp.dat,summary.dat)

# ### add  geo region 

tmp.dat <- as.data.frame(table(rt_omit_dat$geo_region))

names(tmp.dat)[1] <- "term"
names(tmp.dat)[2] <- "dist"

tmp.dat$term <- paste0("geo_region_factor",tmp.dat$term)

summary.dat <- rbind(tmp.dat,summary.dat)

## keep row numbers
summary.dat <-  as.data.frame(data.table::setDT(summary.dat, keep.rownames = TRUE)[])
summary.dat$rn <- as.numeric(summary.dat$rn)


### Summarise by group
# rt_omit_dat %>%
#   group_by(geo_region_factor) %>%
#   summarise(mean(Rt_mean))
# 
# levels(rt_omit_dat$start_day_group)


### Get univariate stats
univar.dat <- uni_tab(rt_omit_dat,"Rt_mean ~ ",covar_all,2,3)  


# covar_cont <- c(  "log_popden","Piped_water_percent","Sewage_or_septic_percent", "log_travel_time_hours", "SDI") 
## Suggests that we should drop GDP variable 

covid.mod3 <- lm(Rt_mean ~ start_day_group*geo_region_factor + log_popden  , data = rt_omit_dat)
anova(covid.mod3,covid.mod1)

## keep log popden

covid.mod4 <- lm(Rt_mean ~ start_day_group*geo_region_factor + log_popden + Piped_water_percent  , data = rt_omit_dat)
anova(covid.mod3,covid.mod4)

## keep piped water 

covid.mod5 <- lm(Rt_mean ~ start_day_group*geo_region_factor + log_popden   + Piped_water_percent + Sewage_or_septic_percent , data = rt_omit_dat)
anova(covid.mod5,covid.mod4)
## keep Sewage

covid.mod6 <- lm(Rt_mean ~ start_day_group*geo_region_factor + log_popden   + Piped_water_percent + Sewage_or_septic_percent 
                                                             + log_travel_time_hours  , data = rt_omit_dat)
anova(covid.mod6,covid.mod5)
## keep travel

covid.mod7 <- lm(Rt_mean ~ start_day_group*geo_region_factor + log_popden   + Piped_water_percent + Sewage_or_septic_percent 
                 + log_travel_time_hours + SDI  , data = rt_omit_dat)
anova(covid.mod7,covid.mod6)

# Keep SDI 

### Multivariate output 
multivar.dat <- multi_tab(rt_omit_dat,"Rt_mean ~ start_day_group*geo_region_factor + log_popden + Piped_water_percent + Sewage_or_septic_percent + log_travel_time_hours + SDI",2,3)

## fix column numbers ###

final_table.dat <- merge_sum_uni_mult(summary.dat,univar.dat,multivar.dat)


## Fix p value estimates
final_table.dat$Univariate_p_value <- ifelse(final_table.dat$Univariate_p_value=="0.00", "<0.01", final_table.dat$Univariate_p_value)
final_table.dat$Multivariate_p_value <- ifelse(final_table.dat$Multivariate_p_value=="0.00", "<0.01", final_table.dat$Multivariate_p_value)


geo_day_dat1 <- as.data.frame(emmeans::emmeans(covid.mod7, ~ start_day_group*geo_region_factor))
geo_day_dat1$start_day_group <- factor(geo_day_dat1$start_day_group, 
                                            levels = c("gp1", "gp2" , "gp3"),
                                            labels = c("19-Mar to 26-Apr", "27-Apr to 12-May", "13-May to 17-Jul"))


# p <- ggplot(data= geo_day_dat1, aes(x=geo_region_factor,y=emmean, fill=start_day_group)) +
#       geom_bar(stat="identity",position="dodge") +
#       geom_errorbar(position=position_dodge(.9),width=.25, aes(ymax=upper.CL, ymin=lower.CL),alpha=0.3) +
#       geom_text(aes(label=sprintf("%0.2f", round(emmean, digits = 2))), position=position_dodge(width=0.9), vjust=-0.5,size=3.5) +
#       labs(x="Geographic region", y="Rt Mean", fill="Date of local epidemic start",
#        title = "") +
#   coord_cartesian(ylim = c(0.8,1.25) )
# p
# ggsave("PM_test_results/lm_Rtmean_geo_bar.png",p,  width=20, height=15, units="cm")

### Export the table 

regress_file <- paste0(dir_results,"RT_regress_final_table_",as.character(day_start),"_",as.character(day_end),".xlsx")
write.xlsx(final_table.dat ,file = regress_file, row.names = FALSE)

summary(rt_mean_covar_dat$Rt_mean)

### Plotting estimates for adjusted model 
inter_file <- paste0(dir_results,"RT_regress_interact_estimates_",as.character(day_start),"_",as.character(day_end),".xlsx")
interact_estimates.dat <- as.data.frame(Epi::ci.lin(covid.mod7))

interact_estimates.dat$Variable <- rownames(interact_estimates.dat)

names(interact_estimates.dat)[4] <- "p_value"
names(interact_estimates.dat)[5] <- "L95CI"
names(interact_estimates.dat)[6] <- "U95CI"

interact_estimates.dat$Estimate <- formatC(interact_estimates.dat$Estimate, format = "f", digits = 3)
interact_estimates.dat$p_value <- formatC(interact_estimates.dat$p_value, format = "f", digits = 3)
interact_estimates.dat$p_value <- ifelse(interact_estimates.dat$p_value=="0.000", "<0.001", interact_estimates.dat$p_value)
interact_estimates.dat$L95CI <- formatC(interact_estimates.dat$L95CI, format = "f", digits = 3)
interact_estimates.dat$U95CI <- formatC(interact_estimates.dat$U95CI, format = "f", digits = 3)


interact_estimates.dat$est_ci <- paste0(interact_estimates.dat$Estimate," (",interact_estimates.dat$L95CI," - "
                                         ,interact_estimates.dat$U95CI,")")

#write.xlsx(interact_estimates.dat ,file = inter_file, row.names = FALSE)

#interact_estimates.dat <- interact_estimates.dat[c(7,8,4)]


# bar_data.dat <- read.csv2(file=paste0(dir_results,"RT_regress_interact_estimates_30_150_bar_chart.csv"),sep=",")
# names(bar_data.dat)[1] <- "Estimate"
# 
# 
# 
# 
# bar_data.dat$Estimate <- as.numeric(bar_data.dat$Estimate )
# bar_data.dat$L95CI <- as.numeric(bar_data.dat$L95CI  )
# bar_data.dat$U95CI <- as.numeric(bar_data.dat$U95CI  )
# bar_data.dat$start_day <- factor(bar_data.dat$start_day , levels = c("24-Apr to 31-May","1-Jun to 30-Jun","1-Jul to 14-Sep"))
# bar_data.dat$Estimate_label <- formatC(bar_data.dat$Estimate, format = "f", digits = 2)
# 
# p <- ggplot(bar_data.dat, aes(fill=start_day, y=Estimate, x=geo_region)) + 
#   geom_bar(position="dodge", stat="identity",colour="black",width=0.8) +
#   geom_errorbar(aes(ymin=L95CI, ymax=U95CI), width=.2,position=position_dodge(0.9),colour="grey50") +
#   geom_abline(slope=0, intercept=0.0,  col = "black") +
#   geom_text(aes(label=Estimate_label), position=position_dodge(width=0.9), vjust=-0.8,colour="black",size = 3.0) +
#   labs(y = "Estimate",
#        x = "Geographic region",
#        fill="") 
# p
#ggsave(paste0(dir_results,"RT_regress_interaction_estimates.png"),p,  width=20, height=15, units="cm")

# 
# bar_data.dat$start_day_group  <- bar_data.dat$Variable
# bar_data.dat$geo_region  <- ""
# 
# 
# 
# 
# ## Sort out start_day_group
# # make column blank if not start_day_group
# bar_data.dat$start_day_group[!grepl("start_day_group",bar_data.dat$start_day_group)]<-""
# 
# bar_data.dat$start_day_group[bar_data.dat$start_day_group == "DEF"] <-"NEW1"
## Get data in the format for grouped bar chart 



write.xlsx(interact_estimates.dat ,file = inter_file, row.names = FALSE)

## Check range of Rtmean

summary(rt_omit_dat$Rt_mean)
summary(covid.mod7)




#adj_mean_rt.dat <- as.data.frame(emmeans::emmeans(covid.mod7, ~ start_day_group | geo_region_factor))

# ### table of frequencies
# tmp.dat <- as.data.frame(table(rt_omit_dat$geo_region_factor,rt_omit_dat$start_day_group))
# tmp.dat <- tmp.dat %>% dplyr::arrange(Var1,Var2)
# names(tmp.dat)[1] <- "geo_region_factor"
# names(tmp.dat)[2] <- "start_day_group"  
# # Set row name to 1st column
# tmp.dat <- as.data.frame(data.table::setDT(tmp.dat, keep.rownames = TRUE)[])
# tmp.dat$rn <- as.numeric(tmp.dat$rn)
# 
# Mean_Rt_summary.dat <- merge(tmp.dat,adj_mean_rt.dat,by=c("geo_region_factor","start_day_group"))
# 
# Mean_Rt_summary.dat$emmean <- formatC(Mean_Rt_summary.dat$emmean, format = "f", digits = 3)
# Mean_Rt_summary.dat$lower.CL <- formatC(Mean_Rt_summary.dat$lower.CL, format = "f", digits = 3)
# Mean_Rt_summary.dat$upper.CL <- formatC(Mean_Rt_summary.dat$upper.CL, format = "f", digits = 3)
# 
# Mean_Rt_summary.dat$est_ci <- paste0(Mean_Rt_summary.dat$emmean," (",Mean_Rt_summary.dat$lower.CL," - ",Mean_Rt_summary.dat$upper.CL,") ")
# 
# Mean_Rt_summary.dat <- Mean_Rt_summary.dat %>% dplyr::arrange(rn)
# 
# Mean_Rt_summary.dat <- Mean_Rt_summary.dat[c(1,2,4,10)]
# 
# write.xlsx(Mean_Rt_summary.dat ,file = "plots/calc_Rt_geo_day.xlsx", row.names = FALSE)
# 
# 
# p <- ggplot(data= geo_day.dat, aes(x=geo_region_factor,y=emmean, fill=start_day_group)) +
#   geom_bar(stat="identity",position="dodge") + 
#   geom_errorbar(position=position_dodge(.9),width=.25, aes(ymax=upper.CL, ymin=lower.CL),alpha=0.3) + 
#   geom_text(aes(label=sprintf("%0.2f", round(emmean, digits = 2))), position=position_dodge(width=0.9), vjust=-0.5,size=3.5) + 
#   labs(x="Geographic region", y="Rt Mean", fill="Mid day",
#       title = "Rt Mean predictions for combinations of Mid day and Region  (Rt mean window - 30 - 100 days)
#               \n (Controlling for Per-capita income and fertility rate)") + 
#   coord_cartesian(ylim = c(0.9, 1.5) ) 
# p
# ggsave("PM_test_results/lm_Rtmean_geo_adj_bar-30-100.png",p,  width=30, height=15, units="cm")


