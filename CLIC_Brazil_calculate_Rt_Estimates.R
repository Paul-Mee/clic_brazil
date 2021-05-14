######################################################################
## APEestim renewal results for Brazilian states

# Method from: Parag, KV, and Donnelly, CA. (2019) “Optimising Renewal Models for 
# Real-Time Epidemic Prediction and Estimation” BioRxiv: 835181.

# subsequently published here>
#    https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1007990

# Adjusts an input curve of onsets to account for notification delays and then
# applies the APE renewal approach to estimate R(t) for the adjusted curve
######################################################################
######################################################################
## Code to Compare EpiFilter with APEestim and EpiEstim
# From: Parag, KV, (2020) “Improved real-time estimation of reproduction numbers
# at low case incidence and between epidemic waves” BioRxiv.
######################################################################
# Run Rt predictions for selected Brazilian Municipalities by each method
######################################################################
# This version runs estimation for all cities
#####################################

# Workflow:


##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))

this_date <- Sys.Date()
#today <- format(today, format="%d-%B-%Y")

# Main packages
library("EpiEstim")
library("caTools")
library("lubridate")
library("tidyverse")
library("stringr")
library("zoo")
library("foreign")
library("MASS")
library("mgcv")
library("ggpubr")
library("dplyr")
library("fBasics")
library("RcppRoll")


# Source Epifilter functions

files.sources = list.files(path = epi_filter_file_sources)
for (i in 1:length(files.sources)) {
  source(paste0(c(epi_filter_file_sources, files.sources[i]), collapse = ''))
}



sample_n_groups = function(grouped_df, size, replace = FALSE, weight=NULL) {
  grp_var <- grouped_df %>% 
    groups %>%
    unlist %>% 
    as.character
  random_grp <- grouped_df %>% 
    summarise() %>% 
    sample_n(size, replace, weight) %>% 
    mutate(unique_id = 1:NROW(.))
  grouped_df %>% 
    right_join(random_grp, by=grp_var) %>% 
    group_by_(grp_var) 
}




fname <-  paste0(dir_data_objects,"Brazil_BigStandard_results.RData")
print(fname)

load(fname)


### Selecting data  ranges for analysis 
start_date = as.Date("2020-04-01")
origin_date = as.Date("2020-01-01")
cens_date = as.Date("2021-01-14")
### Cutting off last two weeks of data 




### Using standardised dataset 

c_dat <- BigStandard$standardised_incidence

### Data since start date 
c_dat <- c_dat [which(c_dat$date_end>=format(as.Date(start_date), "%Y-%m-%d"))  ,] 
### Data up to censoring date - if needed  
#c_dat <- c_dat [which(c_dat$date_end<=format(as.Date(cens_date), "%Y-%m-%d"))  ,] 
### Data in selected cities



c_dat <- c_dat[c("Area","date_end","cum_cases","X","Y")]

names(c_dat)[1] <- "city_state"
names(c_dat)[2] <- "date"




#Sort dataframe
c_dat_sort <- c_dat[with(c_dat, order(c_dat$city_state, c_dat$date)), ]

#Convert from cumulative to incremental cases by group)
c_dat_sort$inc_cases <- ave(c_dat_sort$cum_cases, as.factor(c_dat_sort$city_state), FUN=function(x) c(NA,diff(x)))

# Drop first day as no inc_cases
#c_dat_sort$inc_cases[1] <- c_dat_sort$cum_cases[1]
c_dat_sort <- slice(c_dat_sort, (2:nrow(c_dat_sort)))

c_dat_sort$city_state <- as.character(c_dat_sort$city_state)  

### Set up variables used in Rt calculation for each city    

#### Creating the required time variables 

# week of year 
c_dat_sort$day_of_week.f <- wday(c_dat_sort$date,label=TRUE)
c_dat_sort$day_of_week <- wday(c_dat_sort$date)
# Week of year 
c_dat_sort$week_number <- week(c_dat_sort$date)
c_dat_sort$week_number.f <- factor(week(c_dat_sort$date))
# day of year 
# Revised version use days since Jan 1st 2020 rather than day of year to avoid discontinuity around 31st Dec
c_dat_sort$day_of_year <- as.integer(c_dat_sort$date - origin_date)
c_dat_sort$day_of_year.f <- factor(c_dat_sort$day_of_year)


# 
### Split into a dataframe for each municipality

city_dat_list = split(c_dat_sort, list(c_dat_sort$city_state) )

#length(city_dat_list)

#t(sapply(city_dat_list, sapply, mean))


### Correcting for Dec 31st Effect 
## Create variable end_year_flag = 1 for Dec 30th - 31st and 0 otherwise
end_year_start = as.Date("2020-12-30")
end_year_end = as.Date("2020-12-31")
c_dat_sort$end_year_flag=0
c_dat_sort$end_year_flag[c_dat_sort$date >= end_year_start & c_dat_sort$date <= end_year_end ] <- 1

c_dat_sort$end_year_flag.f <- as.factor(c_dat_sort$end_year_flag)



### Split into a dataframe for each municipality

city_dat_list = split(c_dat_sort, list(c_dat_sort$city_state) )

#length(city_dat_list)

#t(sapply(city_dat_list, sapply, mean))


### Loop through each element in the list    
## Convert warnings to errors so that gam models fail with warnings

check_data <- data.frame( city_name=character(),
                          pass_fail=character(),
                          days_data=integer(),
                          cum_cases=integer())

options(warn=2)
#i <- 1
for (i in 1:length(city_dat_list)) {
    city_dat_tmp <- as.data.frame(city_dat_list[i])
    print (city_dat_tmp[1,1])
    #city_dat_tmp <- city_dat_tmp[c(1:6)]
    names(city_dat_tmp)[1] <- "city_state"
    names(city_dat_tmp)[2] <- "date"
    names(city_dat_tmp)[3] <- "cum_cases"
    names(city_dat_tmp)[4] <- "X"
    names(city_dat_tmp)[5] <- "Y"
    names(city_dat_tmp)[6] <- "inc_cases"
    names(city_dat_tmp)[7] <- "day_of_week.f"
    names(city_dat_tmp)[8] <- "day_of_week"
    names(city_dat_tmp)[9] <- "week_number"
    names(city_dat_tmp)[10] <- "week_number.f"
    names(city_dat_tmp)[11] <- "day_of_year"
    names(city_dat_tmp)[12] <- "day_of_year.f"
    names(city_dat_tmp)[13] <- "end_year_flag"
    names(city_dat_tmp)[14] <- "end_year_flag.f"
    
    ### Data cleaning if there is a v large increment in cases after a string of 0 or v low values reduce increment to 0 
    
    city_dat_tmp$prev_30_inc <- RcppRoll::roll_sum(city_dat_tmp$inc_cases,31, fill=NA, align="right") - city_dat_tmp$inc_cases
    city_dat_tmp <- within(city_dat_tmp, inc_cases[inc_cases >  prev_30_inc] <- 0)
    
    print("Maximum Cumulative cases")
    print(as.character(max(city_dat_tmp$cum_cases,na.rm = TRUE)))
     ### Only run if > 30 days data & total cases > 500
    print("Number of weeks of data")
    print(as.character(nrow(city_dat_tmp)/7))
    
    ### Set k parameter for gam model as number of weeks of data 
    kval= floor(nrow(city_dat_tmp)/7) 
    
    n_days= nrow(city_dat_tmp)  
    n_cases =  max(city_dat_tmp$cum_cases,na.rm = TRUE)

  if (n_days > 30 & n_cases > 500) {
    ### Check daily distribution of cases prior to model fit
    
    
    ## Data for prediction model
    new_pred_data <- as.data.frame(city_dat_tmp$day_of_week.f)
    new_pred_data$day_of_year <- city_dat_tmp$day_of_year
    new_pred_data$day_of_year.f <- city_dat_tmp$day_of_year.f
    new_pred_data$week_number <- week(city_dat_tmp$date)
    new_pred_data$week_number.f <- factor(week(city_dat_tmp$date))
    new_pred_data$end_year_flag.f <- city_dat_tmp$end_year_flag.f 
    
    ### Bar chart day of week 
    ### Used to check original distribution of reporting days 
    ### Assummption of under reporting on weekends and over reporting on weekddays
    # day_total <- as.data.frame(aggregate(city_dat_tmp$inc_cases, by=list(city_dat_tmp$day_of_week.f), FUN=sum))
    # names(day_total)[1] <- "Day"
    # names(day_total)[2] <- "Total_Cases"
    # orig_bar_chart <- (ggplot(day_total, aes(x=Day, y=Total_Cases))
    #                     + ggtitle("Original daily totals")
    #                   + geom_bar(stat = "identity"))
    # orig_bar_chart
    
    
    ### Creating additional time related variables 


    ### Build model 
    ### Spline model
    
    ### Checking for models that fail
    skip_to_next <- FALSE
    
    ### Correcting for day of week 
    tryCatch(gam_m1 <- gam(inc_cases ~  s(day_of_year,k=kval) + day_of_week.f , method = "REML",  data = city_dat_tmp, family = nb()), 
                        error = function(e) { skip_to_next <<- TRUE}) 
  
    if(isTRUE(skip_to_next))   {
      #sink(file=log_file,append=TRUE)
      print("Model failed for city")
      print (city_dat_tmp[1,1])
      print("Days of data")
      print (as.character(n_days))
      print("Total_cases")
      print (as.character(n_cases))
      # sink()

      check_data <-  check_data %>% add_row(city_name = city_dat_tmp[1,1],
                             pass_fail = "fail",
                             days_data = n_days,
                             cum_cases = n_cases)
      next }

      if(!isTRUE(skip_to_next))  {
      check_data <- check_data %>% add_row(city_name = city_dat_tmp[1,1],
                             pass_fail = "success",
                             days_data = n_days,
                             cum_cases = n_cases)
     }
    
    
    
    ## Loop through each day of the week and calculate average predicted number of cases for each day 
    for(j in levels(city_dat_tmp$day_of_week.f )){ 
      #print(j)
      new_pred_data$day_of_week.f <- j
      pred_inc_day <- paste("predicted_inc_cases_",j,sep="")
      city_dat_tmp[[pred_inc_day]] <- predict.gam(gam_m1 , newdata = new_pred_data, type ="response")
    } 
    city_dat_tmp$mean_pred_inc_cases <- (rowSums(city_dat_tmp[, grep("predicted_inc_cases_", names(city_dat_tmp))]))/7
    
    ### Bar chart for predicted cases 
    # pred_day_total <- as.data.frame(aggregate(city_dat_tmp$mean_pred_inc_cases, by=list(city_dat_tmp$day_of_week.f), FUN=sum))
    # 
    # names(pred_day_total)[1] <- "Day"
    # names(pred_day_total)[2] <- "Total_Predicted_Cases"
    # 
    # 
    # pred_bar_chart <- (ggplot(pred_day_total, aes(x=Day, y=Total_Predicted_Cases))
    #                    + ggtitle("Predicted daily totals")
    #                    + geom_bar(stat = "identity"))
    # 
    # pred_bar_chart
    
    ### Replace inc_cases with predicted value
    
    ### Original and smoothed incident cases
    # inc_plot <- ggplot(city_dat_tmp) +
    #   geom_line(aes(x=date,y=inc_cases)) + 
    #   geom_line(aes(x=date,y=mean_pred_inc_cases)) 
    # inc_plot
    # plot_name  <- paste("plots/smooth_inc_data_",select_city_state,".png",sep="")
    # ggsave(plot_name, width=40, height=16, units="cm")
    
    city_dat_tmp$orig_inc_cases <- city_dat_tmp$inc_cases
    
    #### Test predictions are reasonable
    
    max_orig_cases <- max(city_dat_tmp$orig_inc_cases,na.rm = TRUE)
    min_orig_cases <- min(city_dat_tmp$orig_inc_cases,na.rm = TRUE)
    mean_orig_cases <- mean(city_dat_tmp$orig_inc_cases,na.rm = TRUE)

    max_pred_cases <- max(city_dat_tmp$mean_pred_inc_cases,na.rm = TRUE)
    min_pred_cases <- min(city_dat_tmp$mean_pred_inc_cases,na.rm = TRUE)
    mean_pred_cases <- mean(city_dat_tmp$mean_pred_inc_cases,na.rm = TRUE)

    if( abs(mean_orig_cases-mean_pred_cases) >= 20)  {
    sink("CC_Scripts/log_files/check_predictions.log",append=TRUE)
    print (city_dat_tmp[1,1])
    print("large diff in mean predicted values")
    print("original mean cases")
    print(as.character(mean_orig_cases))
    print("predicted mean cases")
    print(as.character(mean_pred_cases))
    print("difference")
    print(as.character(abs(mean_orig_cases-mean_pred_cases) ))
    sink()
    }
    
  ### This section for truncation ## drop last n days data as incomplete
  trunc_day=4
  max_day=max(city_dat_tmp$date)
  last_day = max_day - trunc_day
  city_dat_tmp <- city_dat_tmp[which(city_dat_tmp$date<=last_day),] 
  
  
  ### this section for mean shift   
    
  ### Simple adjustment for delays - subtract mean reporting delay in days # Currently not used
  mean_delay=7
  city_dat_tmp$adj_date <- format((as.Date(city_dat_tmp$date, "%d/%m/%Y")   - mean_delay), "%d/%m/%Y")
   
       
  #####
  ## Epifilter implementation
  ######################################################################
  ## EpiFilter: provides formally smoothed and exact estimates
  # Method based on Bayesian recursive filtering and smoothing
  ######################################################################
  
  
  # Incidence and dates
  # select start day  
  tstart = 1
  # Round mean cases to nearest integer
  Iday = round(city_dat_tmp$mean_pred_inc_cases[tstart:nrow(city_dat_tmp)])
  dates  = city_dat_tmp$adj_date[tstart:nrow(city_dat_tmp)]
  # Time series lengths
  nday = length(dates); tday = 1:nday
  
  # Approximate serial interval distribution from Ferguson et al
  wdist = dgamma(tday, shape = 2.3669, scale = 2.7463)
  
  # Total infectiousness
  Lday = rep(0, nday) 
  for(i in 2:nday){
    # Total infectiousness
    Lday[i] = sum(Iday[seq(i-1, 1, -1)]*wdist[1:(i-1)])    
  }
  
  # Setup grid and noise parameters
  Rmin = 0.01; Rmax = 10; eta = 0.1
  
  # Uniform prior over grid of size m
  m = 200; pR0 = (1/m)*rep(1, m)
  # Delimited grid defining space of R
  Rgrid = seq(Rmin, Rmax, length.out = m)
  
  # Filtered (causal) estimates as list [Rmed, Rhatci, Rmean, pR, pRup, pstate]
  Rfilt = epiFilter(Rgrid, m, eta, pR0, nday, Lday[tday], Iday[tday], 0.025)
  
  # Smoothed estimates as list of [Rmed, Rhatci, Rmean, qR]
  Rsmooth = epiSmoother(Rgrid, m, Rfilt[[4]], Rfilt[[5]], nday, Rfilt[[6]], 0.025)   

  ### Use the accumulated prediction error metric 
  # # From: Parag, KV, and Donnelly, CA. (2019) "Optimising Renewal Models for 
  # Real-Time Epidemic Prediction and Estimation" BioRxiv: 835181.
  # APE assuming all cases are used
  
  # Generates a probability distribution for the serial interval
  # Serial interval for COVID-19
  # Approximate serial interval distribution from Ferguson et al
  mean_si <- 6.5; sd_si <- 4.03
  #Serial interval for COVID-19
  si_distr <- discr_si(seq(0, 30), mean_si, sd_si)
  Lam = overall_infectivity(Iday, si_distr)
  # Clean Lam vectors of NAs
  Lam[is.na(Lam)] = 0

  # Priors and settings
  # a is used to calculate the confidence interval (0.025 = 95% CI - Check )
  Rprior = c(1, 3); a = 0.025
  folres = "PM_test_results"
  Rtotal = apeEstim(Iday, si_distr, Lam, Rprior, a, 1, folres)
  Rtotalhat = Rtotal[[2]][[4]] # mean R estimate
  Rtotalci = Rtotal[[2]][[5]] # confidence around R estimate
  
  ### Epiestim - Fixed time window 
  # For fixed window get times of incidence curve
  win = 7  # weekly
  Rtotalfix = apeSpecific(Iday, si_distr, Lam, Rprior, a, 1, win)
  
  Rfixtmp <- Rtotalfix[[2]]

  ### Plotting results 
  
  Rplot_data <- as.data.frame(dates[1:nday])
  Rplot_data$dates <- as.Date(Rplot_data$dates,"%d/%m/%Y")
  
  Rplot_data$city_state<- city_dat_tmp$city_state[tstart:nrow(city_dat_tmp)]
  
  Rplot_data$smooth_inc_cases <- Iday
  Rplot_data$orig_inc_cases <- city_dat_tmp$inc_cases[tstart:nrow(city_dat_tmp)]
 
  Rplot_data$Rt_EF_filter <- Rfilt[[3]]
  Rplot_data$Rt_EF_smooth <- Rsmooth[[3]]
  Rplot_data$Rt_ApEstim[2:nday] <- Rtotalhat
  # Plotting the 95% confidence intervals 
  Rplot_data$Rt_ApEstim_LCI[2:nday] <- Rtotalci[1,]
  Rplot_data$Rt_ApEstim_UCI[2:nday] <- Rtotalci[2,]
  
  # R Smooth CI's 
 RSmoothCI <-  data.frame(Rsmooth[[2]])
 
Rplot_data$Rt_RSmooth_LCI[2:nday] <- as.numeric(RSmoothCI[1,2:nday])
Rplot_data$Rt_RSmooth_UCI[2:nday] <- as.numeric(RSmoothCI[2,2:nday])
  
  
  Rplot_data$Rt_EpiEstim[2:nday] <- Rfixtmp[[4]]
  
  ### Drop first day 
  Rplot_data <- slice(Rplot_data, (2:nday))
  
  ### Reformat dataframe as per previous analysis
  
  plot_data  <- Rplot_data[c(2,3,4,5,7,11,12,8,9,10)]
  
  names(plot_data)[1] <- "Date"
  names(plot_data)[2] <- "city_state"
  names(plot_data)[3] <- "smooth_inc_cases"
  names(plot_data)[4] <- "orig_inc_cases"
  names(plot_data)[5] <- "Rt_Smooth"
  names(plot_data)[6] <- "Rt_Smooth_LCI"
  names(plot_data)[7] <- "Rt_Smooth_UCI"
  names(plot_data)[8] <- "Rt_ApeEstim"
  names(plot_data)[9] <- "Rt_ApeEstim_LCI"
  names(plot_data)[10] <- "Rt_ApeEstim_UCI"

  
  ### Adjust Rt to 0 (no prediction) if Rtotalhat is a whole number n.000000
  ### Accounts for very low incidence values
  #plot_data <- within(plot_data, Rtotalhat[Rtotalhat%%1==0 ] <- 0)
  ### Adjust Upper and Lowe CI to 0 
  #plot_data <- within(plot_data, Lower_CI[Rtotalhat==0 ] <- 0)
  #plot_data <- within(plot_data, Upper_CI[Rtotalhat==0 ] <- 0)

  
  
  ### Append to overall dataframe      
  if(exists("all_plot_data")==FALSE) {
    all_plot_data <- plot_data}
  else{
    all_plot_data <-  rbind(all_plot_data,plot_data)}
  
  # # yrange for plot 
  # 
  # 
  # ymin_lim = min(Rplot_data$Rt_EF_smooth) 
  # 
  # ymax_lim = mean(Rplot_data$Rt_EF_smooth) + 0.05
  # 
  # plot_title <-  paste("Estimated Rt by different methods - ",select_city_state,sep="")
  # plot_sub_title <- "Data series from June 1st 2020"
  
  # EF_Rt_plot <- ggplot(Rplot_data) + 
  #   geom_line(aes(x=dates,y=Rt_EF_smooth,color="Epifilter")) +
  #   geom_line(aes(x=dates,y=Rt_ApEstim,color="ApeEStim")) +
  #   geom_line(aes(x=dates,y=Rt_EpiEstim,color="EpiEStim (7 day window")) +
  #   coord_cartesian(ylim = c(ymin_lim, ymax_lim)) +
  #   geom_hline(yintercept=1.0, linetype="solid") +
  #   scale_x_date(date_breaks = "2 week", date_labels="%d-%m") +
  #   labs(colour = "Estimation method") +
  #   ggtitle(label = plot_title , subtitle = plot_sub_title) +
  #   xlab("Date") +
  #   ylab("Rt") 
  # EF_Rt_plot
  # plot_name  <- paste("plots/Rt_method_compare_",select_city_state,".png",sep="")
  # ggsave(plot_name, width=40, height=16, units="cm")
  
  }
    
}


### Save R dataset
if(exists("all_plot_data")==TRUE)  {
saveRDS(all_plot_data,file=paste0(dir_Rt_data,"Brazil_rt_prediction-current.RDS"))}


#saveRDS(check_data,file="CC_data/City_Case_data/Brazil/Brazil_formatted/Rt_Data/check_data.RDS")
## Plot of success failure

#check_data <- readRDS("CC_data/City_Case_data/Brazil/Brazil_formatted/Rt_Data/check_data.RDS")
## Summary stats
# check_data %>% count(pass_fail)
# 
# tapply(check_data$days_data, check_data$pass_fail, summary)
# 
# tapply(check_data$cum_cases, check_data$pass_fail, summary)
# 
# 
# ggplot(check_data, aes(x=days_data, y=cum_cases, color=pass_fail)) + 
#   geom_point(size=1,shape=16) + 
#   theme_classic() 
#   #coord_cartesian(ylim = c(0, 3000)) 
# ggsave("plots/check_rt_fail_yfull.png", width=40, height=16, units="cm")


## Reset warning messages
options(warn=1)

