### file for all functions used in the Covid-19 LACPT aanlysis


# Oliver Brady
# 30th April 2020
# Edit Paul Mee 
# 15 Apr 2021 - reduce size of output files


# date.rehsape() function converts from wide to long format while preservign date structures
date.reshape <- function(c_dat){
  # preserve dates list and convert to R date format
  if(any(grepl("X", colnames(c_dat)))){
    dates_list <- as.Date(gsub("_", "/", gsub("X", "", colnames(c_dat[2:ncol(c_dat)]))), format = "%d/%m/%Y")
    
    # rehape to long format
    c_dat = reshape(c_dat,
                    idvar = "Area_Name",
                    varying = list(2:ncol(c_dat)),
                    direction = "long",
                    v.names = "X")
    # reassign dates
    c_dat = data.frame(Area = c_dat$Area_Name,
                       date_end = dates_list[c_dat$time],
                       cum_cases = as.numeric(c_dat$X))
  }else{
    dates_list <- as.Date(gsub("cum_cases.", "", colnames(c_dat[2:ncol(c_dat)])))
    
    # rehape to long format
    c_dat = reshape(c_dat,
                    idvar = "Area_Name",
                    varying = list(2:ncol(c_dat)),
                    direction = "long",
                    v.names = "cum_cases")
    # reassign dates
    c_dat = data.frame(Area = as.factor(as.character(c_dat$Area_Name)),
                       date_end = dates_list[c_dat$time],
                       cum_cases = as.numeric(c_dat$cum_cases))
  }
  return(c_dat)
}





# takes data on grouped age-specific COVID case data and denominator population age groups
# and calculates the proportion of all cases in 1 year age bands
age.prop <- function(case_age_dist, denom){
  
  # add denominator population for age groups
  case_age_dist$pop <- NA
  for(i in 1:nrow(case_age_dist)){
    if((case_age_dist$Age_high[i] + 1) > ncol(denom)){
      if((case_age_dist$Age_low[i] + 1) > ncol(denom)){
        case_age_dist$pop[i] = sum(denom[ncol(denom)])
      }else{
        case_age_dist$pop[i] = sum(denom[(case_age_dist$Age_low[i] + 1):ncol(denom)])
      }
    }else{
      case_age_dist$pop[i] = sum(denom[(case_age_dist$Age_low[i] + 1):(case_age_dist$Age_high[i] + 1)])
    }
    
  }
  
  # now calculate incidence
  case_age_dist$Incidence = case_age_dist$Cases / case_age_dist$pop
  
  # and give predictions for all ages
  # if the oldest age group in the case age data  is < 100 assign incidence rates 
  # in oldest age group to those up to 100
  if(max(case_age_dist$Age_high) < 100){case_age_dist$Age_high[length(case_age_dist$Age_high)] = 100}
  if(max(case_age_dist$Age_high) > 100){case_age_dist$Age_high[length(case_age_dist$Age_high)] = 100}
  case_age_dist_ALLAGE = data.frame(age = 0:100)
  case_age_dist_ALLAGE$risk = sapply(case_age_dist_ALLAGE$age, 
                                     function(x) case_age_dist$Incidence[(x >= case_age_dist[, 1]) & (x <=      case_age_dist[, 2])])
  
  # and noramlise to make all sum to 1
  case_age_dist_ALLAGE$risk = unlist(case_age_dist_ALLAGE$risk) / sum(unlist(case_age_dist_ALLAGE$risk))
  case_age_dist = case_age_dist_ALLAGE # rename for simplicity
  
  # and return result
  return(case_age_dist)
}







# smart rounding function that rounds to integer but preserves the sum of the vector
smart.round <- function(x) {
  y <- floor(x)
  indices <- tail(order(x-y), round(sum(x)) - sum(y))
  y[indices] <- y[indices] + 1
  y
}







# main function for calculating age-specific incidence from raw case numbers
# main function for calculating age-specific incidence from raw case numbers
age_incid_standardise <- function(n_cases, # the number of cases to be distributed - now in vectorised form
                                  origin_age_dist, # the number of people in ages 0-90 in origin area
                                  target_age_dist, # the number of people in ages 0-90 in target area
                                  COVID_age_dist = case_age_dist) # the age distribution of COVID cases
{
  require(data.table)
  # calculate expected number of cases in each age group if everyone was exposed
  origin_age_dist = data.frame(Age = 0:90,
                               pop = origin_age_dist,
                               Exp = origin_age_dist * COVID_age_dist$risk[1:91])
  
  origin_age_dist <- data.table::as.data.table(origin_age_dist)
  # now normalise and multiple by n cases
  origin_age_dist$Exp = 1 * origin_age_dist$Exp / sum(origin_age_dist$Exp)
  
  # now give me age specific incidence
  origin_age_dist$Age_incidence = origin_age_dist$Exp / origin_age_dist$pop
  # adjust for NaNs
  origin_age_dist$Age_incidence[!is.finite(origin_age_dist$Age_incidence)] = 0
  
  # now apply to a standardised target age distribution
  origin_age_dist$Exp_standardised_cases = origin_age_dist$Age_incidence * target_age_dist
  
  # apply to target age distribution
  new_cases = sapply(n_cases, function(x) sum(x * origin_age_dist$Exp_standardised_cases))
  
  # now sum into a total expected number of cases
  return(new_cases)
}



# depricated- now use real death data
#CFR.apply <- function(c_dat, CFR = c(0.01, 0.014, 0.015)){
#  c_dat$Deaths_2_5 = c_dat$cum_cases * CFR[1]
#  c_dat$Deaths_50 = c_dat$cum_cases * CFR[2]
#  c_dat$Deaths_97_5 = c_dat$cum_cases * CFR[3]
#  
#  return(c_dat)
#}





# converts dates of interventions to the origin scale
district.start.date.find <- function(c_dat, Intervention){
  # list of interventions put in place (remove area names and restrictions not yet put in place)
  intTypes <- names(Intervention)
  
  if(is.null(nrow(Intervention))){
    intTypes = intTypes[!is.na(Intervention)]
    intTypes = intTypes[intTypes != "Area_Name"]
    
    c_dat = data.frame(c_dat, matrix(NA, nrow = nrow(c_dat), ncol = length(intTypes)))
    colnames(c_dat)[(ncol(c_dat) - length(intTypes) + 1):ncol(c_dat)] = intTypes
    
    # now go through by each type of intervention
    for(i in 1:length(intTypes)){
      # if outside date range don't proceed
      if(all(Intervention[intTypes[i]] < as.Date(c_dat$date_end))){
        c_dat[, intTypes[i]] = 0
      }else{
        if(all(Intervention[intTypes[i]] > as.Date(c_dat$date_end))){
          c_dat[, intTypes[i]] = NA
        }else{
          # days with intervention
          t1 <- c_dat$Days_since_start[c_dat$date_end >= Intervention[intTypes[i]]]
          
          # district first day with intervention
          t1B <- aggregate(t1, by = list(c_dat$Area[c_dat$date_end >= Intervention[intTypes[i]]]), FUN = min)
          
          # now match back to original dataframe
          c_dat[, intTypes[i]] = t1B$x[match(c_dat$Area, t1B$Group.1)]
        }
      }
    }
  }else{
    intTypes = intTypes[!is.na(Intervention[1, ])]
    intTypes = intTypes[intTypes != "Area_Name"]
    
    c_dat = data.frame(c_dat, matrix(NA, nrow = nrow(c_dat), ncol = length(intTypes)))
    colnames(c_dat)[(ncol(c_dat) - length(intTypes) + 1):ncol(c_dat)] = intTypes
    
    # now go through by each type of intervention
    for(i in 1:length(intTypes)){
      # if outside date range don't proceed
      if(all(Intervention[1, intTypes[i]] < as.Date(c_dat$date_end))){
        c_dat[, intTypes[i]] = 0
      }else{
        if(all(Intervention[1, intTypes[i]] > as.Date(c_dat$date_end))){
          c_dat[, intTypes[i]] = NA
        }else{
          # days with intervention
          t1 <- c_dat$Days_since_start[c_dat$date_end >= Intervention[1, intTypes[i]]]
          
          # district first day with intervention
          t1B <- aggregate(t1, by = list(c_dat$Area[c_dat$date_end >= Intervention[1, intTypes[i]]]), FUN = min)
          
          # now match back to original dataframe
          c_dat[, intTypes[i]] = t1B$x[match(c_dat$Area, t1B$Group.1)]
        }
      }
    }
  }
  
  
  
  
  return(c_dat)
}



# estimates deaths from cases
CFR.apply <- function(c_dat, CFR = c(0.01, 0.014, 0.015)){
  c_dat$Deaths_2_5 = c_dat$cum_cases * CFR[1]
  c_dat$Deaths_50 = c_dat$cum_cases * CFR[2]
  c_dat$Deaths_97_5 = c_dat$cum_cases * CFR[3]
  
  return(c_dat)
}





# distribution used for length of stay
LOS <- function(times) rweibull(times, shape = 2, scale = 10)
LOS_ITU <- function(times) rweibull(times, shape = 2, scale = 10)





# bed simulator function that simulates admissions and overlaps between patients
bed.simulator <- function(caseDays, nsims = 10, type = "hosp"){
  caseDays = as.numeric(round(caseDays, 0)) # integer approximation
  # onlty proceed if cases
  if(any(caseDays > 0)){
    
    sim_tracker = matrix(0, nrow = nsims, ncol = length(caseDays))
    for(k in 1:nsims){
      
      # go through day by day and add new admissions and how long they will be in for
      hold = cbind(1:length(caseDays), caseDays)
      hold = rep(hold[, 1], times = hold[, 2])
      if(type == "hosp"){hold2 = round(LOS(sum(caseDays)), 0)}else{hold2 = round(LOS_ITU(sum(caseDays)), 0)}
      LOS_realisations <- data.frame(day_admitted = hold,
                                     LOS = hold2)
      # now to a list where each number represents a day in which someone is admitted
      LOS_list = apply(LOS_realisations, 1, function(x) x[1]:(x[1] + x[2]))
      # now t oa table making sure to count 0s
      LOS_table <- table(factor(unlist(LOS_list), levels = 1:length(caseDays)))
      
      # now add to main sim tracker collector
      sim_tracker[k, ] = as.numeric(LOS_table)
    }
    # now summarise the distribution of estimated beds needed per area at time i
    return(apply(sim_tracker, 2, quantile, probs = c(0.025, 0.5, 0.975)))
  }else{
    return(matrix(0, nrow = 3, ncol = length(caseDays)))
  }
}







# wrapper fucntion that applied bed.simulator to different regions and appends results back to c_dat
# wrapper fucntion that applied bed.simulator to different regions and appends results back to c_dat
region.bed.simulator <- function(c_dat){
  # only proceed if more than one timepoint
  if(nrow(c_dat) > 1){
    # temporary DaysSince start of epidemic for each municipality
    c_dat$tempDays_since_start = as.numeric(c_dat$date_end - min(c_dat$date_end))
    
    
    c_dat_wide = reshape(c_dat[, c("Area", "cum_cases", "tempDays_since_start")],
                         v.names = "cum_cases",
                         idvar = "Area",
                         timevar = "tempDays_since_start",
                         direction = "wide")
    # convert from cumulative to new incident cases
    # replace NAs with max values
    for(i in 1:nrow(c_dat_wide)){
      c_dat_wide[i, 2:ncol(c_dat_wide)][is.na(c_dat_wide[i, 2:ncol(c_dat_wide)])] = max(c_dat_wide[i, 2:ncol(c_dat_wide)], na.rm = T)
    }
    
    c_dat_wide[, 2:ncol(c_dat_wide)] = t(apply(c_dat_wide[ ,2:ncol(c_dat_wide)], 
                                               1,
                                               function(x) diff(c(0, x))))
    # apply bed simulator to each county at a time and match back to the timepoints in c_dat
    c_dat$Bed_occ_2_5 = rep(NA, nrow(c_dat))
    c_dat$Bed_occ_50 = rep(NA, nrow(c_dat))
    c_dat$Bed_occ_97_5 = rep(NA, nrow(c_dat))
    c_dat$ITU_Bed_occ_2_5 = rep(NA, nrow(c_dat))
    c_dat$ITU_Bed_occ_50 = rep(NA, nrow(c_dat))
    c_dat$ITU_Bed_occ_97_5 = rep(NA, nrow(c_dat))
    
    for(i in 1:nrow(c_dat_wide)){
      # run bed simulator
      caseDays = as.matrix(c_dat_wide[i, 2:ncol(c_dat_wide)])
      caseDays[caseDays < 0] = 0 # jsut a quick check
      bed_occupy = data.frame(Days_since_start = (1:(ncol(c_dat_wide) - 1)) - 1,
                              t(bed.simulator(caseDays * prop_hosp, type = "hosp")))
      ITU_bed_occupy = data.frame(Days_since_start = (1:(ncol(c_dat_wide) - 1)) - 1,
                                  t(bed.simulator(caseDays * prop_hosp * prop_ITU, type = "ITU")))
      # trim bed_occupy to relevant days since start
      bed_occupy = bed_occupy[bed_occupy$Days_since_start %in% c_dat[(c_dat$Area == c_dat_wide$Area[i]), c("tempDays_since_start")], ]
      ITU_bed_occupy = ITU_bed_occupy[ITU_bed_occupy$Days_since_start %in% c_dat[(c_dat$Area == c_dat_wide$Area[i]), c("tempDays_since_start")], ]
      
      # now assign back to main dataset
      c_dat[(c_dat$Area == c_dat_wide$Area[i]), c("Bed_occ_2_5", "Bed_occ_50", "Bed_occ_97_5")] = bed_occupy[, 2:4]
      c_dat[(c_dat$Area == c_dat_wide$Area[i]), c("ITU_Bed_occ_2_5", "ITU_Bed_occ_50", "ITU_Bed_occ_97_5")] = ITU_bed_occupy[, 2:4]
    }
    # remove temporary column
    c_dat = c_dat[, colnames(c_dat) != "tempDays_since_start"]
    return(c_dat)
  }else{
    c_dat$Bed_occ_2_5 = c_dat$cum_cases
    c_dat$Bed_occ_50 = c_dat$cum_cases
    c_dat$Bed_occ_97_5 = c_dat$cum_cases
    c_dat$ITU_Bed_occ_2_5 = c_dat$cum_cases
    c_dat$ITU_Bed_occ_50 = c_dat$cum_cases
    c_dat$ITU_Bed_occ_97_5 = c_dat$cum_cases
    return(c_dat)
  }
}





# multi.ptocess function wraps lots of processes into one
multi.process <- function(city_data,
                          origin_age_dist,
                          target_age_dist,
                          COVID_age_dist,
                          Intervention,
                          country_CFR){
  # 01 standardise
  city_data$standardised_cases <- NA
  for(i in 1:nrow(city_data)){
    city_data$standardised_cases[i] <- age_incid_standardise(n_cases = city_data$cum_cases[i],
                                                             origin_age_dist,
                                                             target_age_dist,
                                                             COVID_age_dist)
  }
  
  # 02 re-route origin
  city_data<- re.route.origin(city_data)
  # only proceed if the area has enough cases
  if(nrow(city_data) > 0){
    # 03 add dates of Interventions
    if(!is.na(Intervention[1])){city_data = district.start.date.find(city_data, Intervention)}
    
    # 04 filtering variables (will add others soon)
    city_data$Region = region_tab$Region[match(city_data$Area, region_tab$Area)]
    
    # 05 adding bed occupancy and death variables
    city_data = CFR.apply(city_data, country_CFR)
    city_data = region.bed.simulator(city_data)
  }
  
  return(city_data)
}






# reformat area to combine with province name to give one variable (repeated area names between provinces)
ad2.ad1.combine <- function(Ad1Ad2_data){
  if("Province" %in% colnames(Ad1Ad2_data)){
    Ad1Ad2_data$Area_Name = apply(Ad1Ad2_data[, c("Area_Name", "Province")], 
                                  1, 
                                  function(x) paste(x[1], x[2], sep = "_"))
    Ad1Ad2_data = Ad1Ad2_data[, colnames(Ad1Ad2_data) != "Province"]
  }
  if("Geography" %in% colnames(Ad1Ad2_data)){
    Ad1Ad2_data$Area_Name = apply(Ad1Ad2_data[, c("Area_Name", "Geography")], 
                                  1, 
                                  function(x) paste(x[1], x[2], sep = "_"))
    Ad1Ad2_data = Ad1Ad2_data[, colnames(Ad1Ad2_data) != "Geography"]
  }
  if("Level_1_admin" %in% colnames(Ad1Ad2_data)){
    Ad1Ad2_data$Area_Name = apply(Ad1Ad2_data[, c("Area_Name", "Level_1_admin")], 
                                  1, 
                                  function(x) paste(x[1], x[2], sep = "_"))
    Ad1Ad2_data = Ad1Ad2_data[, colnames(Ad1Ad2_data) != "Level_1_admin"]
  }
  if("State" %in% colnames(Ad1Ad2_data)){
    Ad1Ad2_data$Area_Name = apply(Ad1Ad2_data[, c("Area_Name", "State")], 
                                  1, 
                                  function(x) paste(x[1], x[2], sep = "_"))
    Ad1Ad2_data = Ad1Ad2_data[, colnames(Ad1Ad2_data) != "State"]
  }
  return(Ad1Ad2_data)
}




# wrapper function for all of the standardisation processes
Big.standardise <- function(c_dat,
                            deaths_dat,
                            age_dist,
                            case_age_dist,
                            Intervention,
                            covariates = NA,
                            prop_hosp = 1,
                            prop_ITU = 1){
  # c_dat = Brazil_cases
  # deaths_dat = Brazil_deaths
  # age_dist = Brazil_age_dist
  # case_age_dist = Brazil_case_age_dist
  # Intervention = Intervention
  # covariates = SDI
  # prop_hosp = 1
  # prop_ITU = 1
  
  ################
  # part 1: building focus district relevant parameters:
  ################
  
  # copy of the dataset to be modified
  f_c_dat <- c_dat
  
  # target population age distribution is now the national age distribution
  f_pop_age = as.numeric(colSums(age_dist[, 2:ncol(age_dist)]))
  
  # identify case age distribution
  f_case_age <- case_age_dist
  
  # identify time of interventions
  Intervention <- Intervention
  
  
  ###################
  # part 2: performs a range of processing functions to add new variables
  ###################
  
  # adding bed occupancy and death variables
  f_c_dat$cum_deaths = deaths_dat$cum_cases
  # remove bed simulation part
  #f_c_dat = region.bed.simulator(f_c_dat)
  
  ## Age-incidence standardisation
  f_c_dat <- AIS.multiregion(f_c_dat, 
                             target_age_dist = f_pop_age,
                             age_dist = age_dist,
                             case_age_dist = case_age_dist)
  
  # depricated- now done in the visualisation script
  ## re-routing origin
  #f_c_dat <- re.route.origin(f_c_dat)
  
  ## adding dates of interventions
  #f_c_dat = district.start.date.find(f_c_dat, Intervention)
  
  ## filtering variables ((will add others soon))filtering by region
  f_c_dat$Region <- sapply(as.character(f_c_dat$Area), function(x) substr(x,
                                                            nchar(x) - 1,
                                                            nchar(x)))
  
  # add covariates if relevant
  # now not adding to this file
  # if(length(covariates) > 1){
  #   f_c_dat$popden <- covariates$popden[match(f_c_dat$Area, covariates$Area_Name)]
  #   f_c_dat$SDI <- covariates$SDI_index[match(f_c_dat$Area, covariates$Area_Name)]
  #   f_c_dat$Piped_water <- covariates$Piped_water[match(f_c_dat$Area, covariates$Area_Name)]
  #   f_c_dat$Sewage_or_septic <- covariates$Sewage_or_septic[match(f_c_dat$Area, covariates$Area_Name)]
  #   f_c_dat$Travel_time <- covariates$Travel_time[match(f_c_dat$Area, covariates$Area_Name)]
  # }
  
  # convert standardised counts to incidence (cases per 1,000 population)
  f_c_dat$standardised_cases = 1000 * f_c_dat$standardised_cases / sum(f_pop_age)
  f_c_dat$standardised_deaths = 1000 * f_c_dat$standardised_deaths / sum(f_pop_age)
  
  # Remove bed occupancy part 
  # f_c_dat$Stan_Bed_occ_2_5 = 1000 * f_c_dat$Stan_Bed_occ_2_5 / sum(f_pop_age)
  # f_c_dat$Stan_Bed_occ_50 = 1000 * f_c_dat$Stan_Bed_occ_50 / sum(f_pop_age)
  # f_c_dat$Stan_Bed_occ_97_5 = 1000 * f_c_dat$Stan_Bed_occ_97_5 / sum(f_pop_age)
  # 
  # f_c_dat$Stan_ITU_Bed_occ_2_5 = 1000 * f_c_dat$Stan_ITU_Bed_occ_2_5 / sum(f_pop_age)
  # f_c_dat$Stan_ITU_Bed_occ_50 = 1000 * f_c_dat$Stan_ITU_Bed_occ_50 / sum(f_pop_age)
  # f_c_dat$Stan_ITU_Bed_occ_97_5 = 1000 * f_c_dat$Stan_ITU_Bed_occ_97_5 / sum(f_pop_age)
  
  
  # add lat long of municiplaity centroid
  f_c_dat$X = Text_IBGE$MIDX[match(f_c_dat$Area, Text_IBGE$Text_name)]
  f_c_dat$Y = Text_IBGE$MIDY[match(f_c_dat$Area, Text_IBGE$Text_name)]
  
  # calculate standardised outbreak start date for each area
  # outbreak start date now defined as first date in which an incidence of greater than 
  # 1 case per 10,000 inhabitant is exceeded (i.e. standardised cases >= 0.1)
  f_c_dat_OB <- f_c_dat[f_c_dat$standardised_cases >= 0.1, ]
  OB_start = aggregate(date_end ~ Area, data = f_c_dat_OB, FUN = min)
  
  # wrap objects into one list
  SmallWrap = list(standardised_incidence = f_c_dat,
                   OB_start_times = OB_start,
                   Intervention = Intervention)
  
  return(SmallWrap)
}
  

# loads latest case data files given a directory
fetch_latest <- function(fileDir, type = "cases"){
  opts <- data.frame(as.numeric(1:length(list.files(fileDir))), list.files(fileDir))
  
  if(type == "cases"){
    # subset to RDS files
    opts = opts[grepl(".RDS", opts[, 2]), ]
    # trim to date part
    opts = opts[grepl("brazil_daily_cases_ibge_api_", opts[, 2]), ]
    opts[, 2] = gsub("brazil_daily_cases_ibge_api_", "", opts[, 2])
    opts[, 2] = gsub(".RDS", "", opts[, 2])
    opts[, 2] = gsub("-", "", opts[, 2])
    # to date
    opts[, 2] = as.Date(opts[, 2], "%d%b%Y")
    
    # select most recent and return that loaded file
    opts = opts[order(opts[, 2]), ]
    # load and return
    compiled_data <- readRDS(paste0(fileDir,
                                    list.files(fileDir)[opts[nrow(opts), 1]]))
    return(compiled_data)
  }
  
  
  if(type == "deaths"){
    # subset to RDS files
    opts = opts[grepl(".RDS", opts[, 2]), ]
    # trim to date part
    opts = opts[grepl("brazil_daily_deaths_ibge_api_", opts[, 2]), ]
    opts[, 2] = gsub("brazil_daily_deaths_ibge_api_", "", opts[, 2])
    opts[, 2] = gsub(".RDS", "", opts[, 2])
    opts[, 2] = gsub("-", "", opts[, 2])
    # to date
    opts[, 2] = as.Date(opts[, 2], "%d%b%Y")
    
    # select most recent and return that loaded file
    opts = opts[order(opts[, 2]), ]
    # load and return
    compiled_data <- readRDS(paste0(fileDir,
                                    list.files(fileDir)[opts[nrow(opts), 1]]))
    return(compiled_data)
  }
  
  
  if(type == "BigStandard"){
    # subset to RData files
    opts = opts[grepl(".RData", opts[, 2]), ]
    # trim to date part
    opts = opts[grepl("Brazil_BigStandard_results_", opts[, 2]), ]
    opts[, 2] = gsub("Brazil_BigStandard_results_", "", opts[, 2])
    opts[, 2] = gsub(".RData", "", opts[, 2])
    opts[, 2] = gsub("_", "", opts[, 2])
    # to date
    opts[, 2] = as.Date(opts[, 2], "%Y%m%d")
    
    # select most recent and return that loaded file
    opts = opts[order(opts[, 2]), ]
    # load and return
    return(paste0(fileDir,list.files(fileDir)[opts[nrow(opts), 1]]))
  }
}

# compiles data from multiple files reporting case darta at different times
# Archived for now

data_compile <- function(fileList){
  # load in first file (oldest report)
  holdDat <- readRDS(fileList[[1]])
  # check all numeric columns are numeric
  for(i in 3:ncol(holdDat)){holdDat[, i] = as.numeric(holdDat[, i])}
  
  # store column header date names
  holdDates <- as.Date(gsub("X", "", colnames(holdDat)[c(-1, -2)]), format = "%d_%m_%Y")
  
  # ok now loop through updated data and update the holdDat object if:
  # contains new dates
  # contains new areas
  # contains more cases for included areas than the previous report
  
  for(k in 2:length(fileList)){
    newDat <- readRDS(fileList[[k]])
    # check all numeric columns are numeric
    for(i in 3:ncol(newDat)){newDat[, i] = as.numeric(newDat[, i])}
    
    # contains new dates
    newdates <- data.frame(Cindex = 3:ncol(newDat),
                           Date = as.Date(gsub("X", "", colnames(newDat)[c(-1, -2)]), format = "%d_%m_%Y"))
    
    AddDates <- newdates[!(newdates[, 2] %in% holdDates), ]
    
    # contains new areas
    newdAreas <- data.frame(Cindex = 1:nrow(newDat),
                            Area = ad2.ad1.combine(newDat)$Area_Name)
    
    AddAreas <- newdAreas[!(newdAreas[, 2] %in% ad2.ad1.combine(holdDat)$Area_Name), ]
    
    # now add new dates and areas
    holdDat = rbind(holdDat, newDat[AddAreas[, 1], 1:ncol(holdDat)])
    holdDat = cbind(holdDat, newDat[match(ad2.ad1.combine(holdDat)$Area_Name, ad2.ad1.combine(newDat)$Area_Name), 
                                    AddDates[, 1]])
    
    # update column names and date list
    colnames(holdDat)[ncol(holdDat)] = colnames(newDat)[ncol(newDat)]
    holdDates <- as.Date(gsub("X", "", colnames(holdDat)[c(-1, -2)]), format = "%d_%m_%Y")
    
    
    ## now go through areas in both datasets and only update if there are more cases
    corrRow = match(ad2.ad1.combine(newDat)$Area_Name, ad2.ad1.combine(holdDat)$Area_Name)
    
    comparison = holdDat[, 3:(ncol(holdDat) - 1)] < newDat[corrRow, 3:(ncol(holdDat) - 1)]
    
    holdDat[, 3:(ncol(holdDat) - 1)][comparison] = newDat[corrRow, 3:(ncol(holdDat) - 1)][comparison]
  }
  return(holdDat)
}


# wrapper function for performing standardisation across multiple regions
AIS.multiregion <- function(c_dat, target_age_dist, age_dist, case_age_dist){
  
  # c_dat = f_c_dat
  # target_age_dist = f_pop_age
  # age_dist = age_dist
  # case_age_dist = case_age_dist
  
  # initialise vectors to be filled
  c_dat$standardised_cases = rep(NA, nrow(c_dat))
  c_dat$standardised_deaths = rep(NA, nrow(c_dat))
  
  # Remove bed occupancy data 
  
  # c_dat$Stan_Bed_occ_2_5 = rep(NA, nrow(c_dat))
  # c_dat$Stan_Bed_occ_50 = rep(NA, nrow(c_dat))
  # c_dat$Stan_Bed_occ_97_5 = rep(NA, nrow(c_dat))
  # 
  # c_dat$Stan_ITU_Bed_occ_2_5 = rep(NA, nrow(c_dat))
  # c_dat$Stan_ITU_Bed_occ_50 = rep(NA, nrow(c_dat))
  # c_dat$Stan_ITU_Bed_occ_97_5 = rep(NA, nrow(c_dat))
  
  # unique municipalities
  u_muns <- as.factor(unique(as.character(c_dat$Area)))
  c_dat$Area <- as.factor(unique(as.character(c_dat$Area)))
  
  for(i in 1:length(u_muns)){
    # draw down new age distribution of municipality
    if(u_muns[i] %in% as.character(age_dist$Name)){
      origin_age_dist = as.numeric(age_dist[as.character(age_dist$Name) ==u_muns[i], 2:ncol(age_dist)])
    }else{
      origin_age_dist = as.numeric(age_dist[1, 2:ncol(age_dist)])
    }
    
    # now standardise
    for(k in 3:4){
      c_dat[c_dat$Area == u_muns[i], (k + 2)] = age_incid_standardise(n_cases = c_dat[c_dat$Area == u_muns[i], k],
                                                                       origin_age_dist = origin_age_dist,
                                                                       target_age_dist = target_age_dist,
                                                                       COVID_age_dist = case_age_dist)
    }
  }
  
  return(c_dat)
}








# re establishes the time origin in each area in terms opf days since reporting "daysSince" cases
re.route.origin <- function(c_dat, daysSince = 0.1, Zerotrim = TRUE){
  # assemble a table that calculates start time in each area
  # if the area never experiences > 10 standardised cases return nothing
  if(any(c_dat$standardised_cases >= daysSince)){
    date_tab <- table(c_dat$Area[c_dat$standardised_cases >= daysSince], c_dat$date_end[c_dat$standardised_cases >= daysSince])
    Area_start_date = data.frame(Area = rownames(date_tab),
                                 startDate = as.Date(colnames(date_tab)[apply(date_tab, 1, which.max)]))
    
    # add a new column to c_dat that calculates days relative to area start date
    c_dat$Days_since_start = as.numeric(as.Date(c_dat$date_end) -  
                                          as.Date(Area_start_date$startDate[match(c_dat$Area, Area_start_date$Area)]))
    
    # remove datapoints before the outbreak in each area has officially begun (save original copy first for later use)
    c_dat_original = c_dat
    if(Zerotrim){c_dat = c_dat[c_dat$Days_since_start >= 0, ]}
    
    return(c_dat)
  }else{
    return(c_dat[c_dat$standardised_cases >= daysSince, ])
  }
}











