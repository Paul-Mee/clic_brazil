# data pre-processing script
# loads in semi-foramtted .csv files for case tiem series, age distribution fo cases, age distribution of 
# the populationa nd intervention timelines, performs some basic error checking then saves them as .RData objects
# to be directly laoded into the standardisation script

# Oliver Brady
# 30th April 2020
# Altered to run in batch on the server 
#rm(list = ls())
### Data cleaning
rm(list=ls())


### Required libraries
library(sf)

##############
### Directory set up
### Update this with your local directories
##############

dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))



# loads functions to be used for some of the preprocessing
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))


##############
# 01 Brazil
##############


### 1A population age distribution
Brazil_age_dist <- readRDS(paste0(dir_pop_age_dist,"brazil_age_structure_ibge_May14.RDS"))

# strip NAs
Brazil_age_dist = Brazil_age_dist[!is.na(Brazil_age_dist[, 3]), ]
colnames(Brazil_age_dist)[1] = "Area_Name"
Brazil_age_dist = data.frame(Name = ad2.ad1.combine(Brazil_age_dist)[, 1],
                             Brazil_age_dist[, 4:ncol(Brazil_age_dist)])
Brazil_age_dist[, 2:ncol(Brazil_age_dist)] = apply(Brazil_age_dist[, 2:ncol(Brazil_age_dist)], 2, as.numeric)
save(Brazil_age_dist, file = paste0(dir_pop_age_dist,"Brazil_pop_age_clean.RData"))





### 1B Covid Case timeseries formatting
c_dat <- readRDS(paste0(dir_formatted_case_data,"brazil_daily_cases_ibge_api.RDS"))
c_dat_deaths <- readRDS(paste0(dir_formatted_death_data,"brazil_daily_deaths_ibge_api.RDS"))

# check they are numeric (not for IBGE codes)
#for(i in 3:ncol(c_dat)){c_dat[, i] = as.numeric(c_dat[, i])} 
for(i in 3:(ncol(c_dat)-1)){c_dat[, i] = as.numeric(c_dat[, i])}   
#for(i in 3:ncol(c_dat_deaths)){c_dat_deaths[, i] = as.numeric(c_dat_deaths[, i])}
for(i in 3:(ncol(c_dat_deaths)-1)){c_dat_deaths[, i] = as.numeric(c_dat_deaths[, i])} 

# converyt municipality and state variables to one text variable
c_dat <- ad2.ad1.combine(c_dat)
c_dat_deaths <- ad2.ad1.combine(c_dat_deaths)

# retain IBGE-> text name table
IBGE_Text = aggregate(City_ibge_code ~ Area_Name, c_dat, FUN = mean)
# then delete it
c_dat = c_dat[, 1:(ncol(c_dat) - 1)]
c_dat_deaths = c_dat_deaths[, 1:(ncol(c_dat_deaths) - 1)]

c_dat <- date.reshape(c_dat)
c_dat_deaths <- date.reshape(c_dat_deaths)

# check for errors in cumulative case counts - should never go down over time
c_dat <- reshape(c_dat, v.names = "cum_cases", idvar = "Area", timevar = "date_end", direction = "wide")
c_dat_deaths <- reshape(c_dat_deaths, v.names = "cum_cases", idvar = "Area", timevar = "date_end", direction = "wide")



### now numerical non-cumulative case count fixing if there are any cases where the data are not cumulative
if(any(apply(c_dat[, 2:ncol(c_dat)], 1, function(x) any(diff(as.numeric(x)) < 0)))){
  c_dat[, 2:ncol(c_dat)] = t(apply(c_dat[, 2:ncol(c_dat)], 1, function(x) diff(c(0, as.numeric(x)))))
  c_dat[, 2:ncol(c_dat)][c_dat[, 2:ncol(c_dat)] < 0] = 0
  c_dat[, 2:ncol(c_dat)] = t(apply(c_dat[, 2:ncol(c_dat)], 1, cumsum))
}

if(any(apply(c_dat_deaths[, 2:ncol(c_dat_deaths)], 1, function(x) any(diff(as.numeric(x)) < 0)))){
  c_dat_deaths[, 2:ncol(c_dat_deaths)] = t(apply(c_dat_deaths[, 2:ncol(c_dat_deaths)], 1, function(x) diff(c(0, as.numeric(x)))))
  c_dat_deaths[, 2:ncol(c_dat_deaths)][c_dat_deaths[, 2:ncol(c_dat_deaths)] < 0] = 0
  c_dat_deaths[, 2:ncol(c_dat_deaths)] = t(apply(c_dat_deaths[, 2:ncol(c_dat_deaths)], 1, cumsum))
}

### district reconciliation with age distribtion data
# identify districts for which there is no match in the age distribution data:
noMatch <- as.character(unique(c_dat$Area[!(c_dat$Area %in% Brazil_age_dist$Name)]))
#noMatch
# fuzzy matching algorithm to find best alternatives
c_dat$Area = as.character(c_dat$Area)
c_dat_deaths$Area = as.character(c_dat_deaths$Area)
IBGE_Text$Area_Name = as.character(IBGE_Text$Area_Name)

stillNoMatch <- vector()

if(length(noMatch) > 0) {

for(i in 1:length(noMatch)){
  closeMatch <- as.character(Brazil_age_dist$Name[agrep(noMatch[i], Brazil_age_dist$Name)])
  
  # if there are two choose the one with matching state
  if(length(closeMatch) == 2){
    closeMatch = closeMatch[grepl(substr(noMatch[i],
                                         nchar(noMatch[i]) - 2,
                                         nchar(noMatch[i])),
                                  closeMatch)]
  }
  
  if(length(closeMatch) > 0){
    # renaming
    c_dat$Area[c_dat$Area == noMatch[i]] = closeMatch
    c_dat_deaths$Area[c_dat_deaths$Area == noMatch[i]] = closeMatch
    IBGE_Text$Area_Name[IBGE_Text$Area_Name == noMatch[i]] = closeMatch
    
  }else{stillNoMatch = c(stillNoMatch, noMatch[i])}
}

# manual exception matching
stillNoMatch
}


# manually find new matching text name based on lat long from google maps search
# gh = st_point(c(-48.894544, -28.422858))
# Text_IBGE <- st_read("CC_data/City_Case_data/Brazil/source_data/ibge/Brazil_AD2_shape.shp")
# st_intersects(gh, Text_IBGE)
# Text_IBGE[st_intersects(gh, Text_IBGE)[[1]], ]

### This section of code is throwing an error when running in batch 
### but not when the individual R scripts are ru
### Will coment out for manual matching for now. Will only effect 5 places

# manual_match <- data.frame(original = c("Paraíso das Águas_MS",
#                                         "Mojuí dos Campos_PA",
#                                         "Pinto Bandeira_RS",
#                                         "Balneário Rincão_SC",
#                                         "Pescaria Brava_SC"),
#                            replacement = c("Costa Rica_MS",
#                                            "Santarem_PA",
#                                            "Bento Gonçalves",
#                                            "Içara_SC",
#                                            "Laguna_SC"))
# 
# for(i in 1:nrow(manual_match)){
#   if(manual_match$replacement[i] %in% c_dat$Area){
#     c_dat[c_dat$Area == manual_match$replacement[i], 2:ncol(c_dat)] = 
#       c_dat[c_dat$Area == manual_match$replacement[i], 2:ncol(c_dat)] + 
#       c_dat[c_dat$Area == manual_match$original[i], 2:ncol(c_dat)]
#     c_dat = c_dat[c_dat$Area != manual_match$original[i], ]
#   }else{
#     c_dat$Area[c_dat$Area == manual_match$original[i]] = manual_match$replacement[i]
#   }
# }
# 
# for(i in 1:nrow(manual_match)){
#   if(manual_match$replacement[i] %in% c_dat_deaths$Area){
#     c_dat_deaths[c_dat_deaths$Area == manual_match$replacement[i], 2:ncol(c_dat_deaths)] = 
#       c_dat_deaths[c_dat_deaths$Area == manual_match$replacement[i], 2:ncol(c_dat_deaths)] + 
#       c_dat_deaths[c_dat_deaths$Area == manual_match$original[i], 2:ncol(c_dat_deaths)]
#     c_dat_deaths = c_dat_deaths[c_dat_deaths$Area != manual_match$original[i], ]
#   }else{
#     c_dat_deaths$Area[c_dat_deaths$Area == manual_match$original[i]] = manual_match$replacement[i]
#   }
# }

# now re-check:
all(c_dat$Area %in% Brazil_age_dist$Name)

# if still errors need to manually update or data will jsut be deleted
c_dat = c_dat[c_dat$Area %in% Brazil_age_dist$Name, ]
c_dat_deaths = c_dat_deaths[c_dat_deaths$Area %in% Brazil_age_dist$Name, ]

# also check congruity with the Text_IBGE shapefile
#Text_IBGE <- st_read("CC_data/City_Case_data/Brazil/source_data/ibge/Brazil_AD2_shape.shp")
#
#IBGE_hold <- floor(IBGE_Text$City_ibge_code[match(c_dat$Area_Name, IBGE_Text$Area_Name)] / 10)
#noMatch = IBGE_hold[!(IBGE_hold %in% Text_IBGE$IBGE)]
#noMatch
#"Luís Eduardo Magalhães_BA" "Riachão do Bacamarte_PB"   "Mesquita_RJ"




# apply date.reshape function to convert to Long format
names(c_dat)[1] = "Area_Name"
names(c_dat_deaths)[1] = "Area_Name"
Brazil_cases = date.reshape(c_dat)
Brazil_deaths = date.reshape(c_dat_deaths)

### !!!!!!! Subset to only areas that have reported >= 50 cases
caseMAX <- aggregate(Brazil_cases$cum_cases, by = list(Brazil_cases$Area), FUN = max)
caseMAX = caseMAX[caseMAX[, 2] >= 50, ]
Brazil_cases = Brazil_cases[Brazil_cases$Area %in% caseMAX[, 1], ]
Brazil_deaths = Brazil_deaths[Brazil_deaths$Area %in% caseMAX[, 1], ]


# save the reforamtted data
save(Brazil_cases, file = paste0(dir_formatted_case_data,"Brazil_case_timeseries_clean.RData"))
save(Brazil_deaths, file = paste0(dir_formatted_death_data,"Brazil_deaths_timeseries_clean.RData"))

# depricated
# add mapping variables (incidence and lat longs)
#Brazil_cases$cum_incid = 1000 * Brazil_cases$cum_cases / 
#  rowSums(Brazil_age_dist[match(Brazil_cases$Area, Brazil_age_dist$Name), 2:ncol(Brazil_age_dist)])
#
#Text_IBGE <- st_read("CC_data/City_Case_data/Brazil/source_data/ibge/Brazil_AD2_shape.shp")
#Brazil_cases$X = Text_IBGE$MIDX[match(Brazil_cases$Area, Text_IBGE$Text_name)]
#Brazil_cases$Y = Text_IBGE$MIDY[match(Brazil_cases$Area, Text_IBGE$Text_name)]

# save for mapping
#save(Brazil_cases, file = "CC_data/City_Case_data/Brazil/Brazil_formatted/cases/Brazil_case_timeseries_clean_FORMAP.RData")





### 1C Age distribution of Covid cases
case_age_dist <- readRDS(paste0(dir_case_age_dist,"all_brazil_age_case_dist_ibge.RDS"))
case_age_dist <- ad2.ad1.combine(case_age_dist)
# only interested in national age distribution - in this dataset a national age profile is applied
# to a synthetic population of 10,000 for each areas, so outcome is the same
case_age_dist <- case_age_dist[case_age_dist$Area_Name == case_age_dist$Area_Name[1], 2:4]


# add a relevant denominator population - a matrix with number of people in 1 year age bands
denom <- matrix(colSums(Brazil_age_dist[, 2:ncol(Brazil_age_dist)]), nrow = 1)
# run age age prop to convert to 1 year age groups
Brazil_case_age_dist <- age.prop(case_age_dist, denom)

# now save
save(Brazil_case_age_dist, file = paste0(dir_case_age_dist,"Brazil_case_age_clean.RData"))




### 1D Intervention formatting

Intervention <- readRDS(paste0(dir_interventions,"all_brazil_interventions_introduction_date_cepal.RDS"))
Intervention <- ad2.ad1.combine(Intervention)

# reformat dates
for(i in 2:ncol(Intervention)){Intervention[, i] = as.Date(Intervention[, i], format = "%d/%m/%Y")}


save(Intervention, file = paste0(dir_interventions,"Brazil_interventions_clean.RData"))


### 1E Covariates - population density and Sociodemographic index for Brazil

IBGE10 <- read.csv(paste0(dir_ibge_data,"Municipalities_sociodemographics_chars_UNICODE.csv"))
fert_rate <- read.csv(paste0(dir_ibge_data,"Maternal_characteristics.csv"))
colnames(IBGE10) <- gsub(pattern = "\\.", replacement = "_", x = colnames(IBGE10))

#  Socio Development index
SDI <- data.frame(IBGE = floor(IBGE10$Codigo / 10),
                  mun_Income_pc = IBGE10$Renda_per_capita___2000,
                  Fert_rate = IBGE10$Taxa_de_fecundidade__2000_,
                  mun_Mean_years_edu = fert_rate$Mean_mother_age[match(floor(IBGE10$Codigo / 10), fert_rate$IBGE_mun)])

# give NAs the mean value
SDI$mun_Income_pc[is.na(SDI$mun_Income_pc)] = mean(SDI$mun_Income_pc, na.rm = T)
SDI$Fert_rate[is.na(SDI$Fert_rate)] = mean(SDI$Fert_rate, na.rm = T)
SDI$mun_Mean_years_edu[is.na(SDI$mun_Mean_years_edu)] = mean(SDI$mun_Mean_years_edu, na.rm = T)

#Scale
SDI[, 2:4] = apply(SDI[, 2:4], 2, function(x) (x - min(x)) / (max(x) - min(x)))
SDI$SDI_index = rowSums(SDI[, 2:4]) / 3

# add population density
IBGE_stats <- read.csv(paste0(dir_ibge_data,"GAUL_IBGE_conversion_full.csv"))
SDI$popden <- (IBGE_stats$IBGE_MUN_POP / IBGE_stats$Area)[match(SDI$IBGE, IBGE_stats$IBGE_CODE)]
# give the 92 NAs the median value
SDI$popden[is.na(SDI$popden)] = median(SDI$popden, na.rm = T)

# add an "Area" name consistent with the formatting of the case data
Area_IBGE <- st_read(paste0(dir_ibge_data,"Brazil_AD2_shape.shp"))
SDI$Area_Name = Area_IBGE$Text_name[match(SDI$IBGE, Area_IBGE$IBGE)]
# trim any NAs
SDI = SDI[!is.na(SDI$Area_Name), ]

# drinking water sources
water <- read.csv(paste0(dir_ibge_data,"Water_sources.csv"))
names(water)[1] <- "Municipality"
water$Municipality = sapply(water$Municipality, function(x) substr(x, start = 1, stop = 6))
water$Municipality = as.numeric(water$Municipality)
SDI$Piped_water = water$Proportion[match(SDI$IBGE, water$Municipality)]

# sanitary sources
sanitary <- read.csv(paste0(dir_ibge_data,"Sanitary.csv"))
names(sanitary)[1] <- "Municipality"
sanitary$Municipality = sapply(sanitary$Municipality, function(x) substr(x, start = 1, stop = 6))
sanitary$Municipality = as.numeric(sanitary$Municipality)
SDI$Sewage_or_septic = sanitary$Proportion_of_sew_or_sep[match(SDI$IBGE, sanitary$Municipality)]

# travel time to state capital
# Edited for encoding
Travel_time <- read.csv(paste0(dir_ibge_data,"Travel_times.csv"),encoding="UTF-8")
SDI$Travel_time = Travel_time$State_cap_travel_time[match(SDI$Area_Name, Travel_time$Text_name)]
# give NAs the median travel time
SDI$Travel_time[is.na(SDI$Travel_time)] = median(SDI$Travel_time, na.rm = T)






# save
save(SDI, file = paste0(dir_covariates,"Brazil_mun_covs.RData"))



# 1F updated hospital length of stay calculations for Brazil
# load(paste0(dir_data_objects,"CVE_Date_delays.RData"))
# 
# # date formatting
# brazil_cve_dates$dt_hospitalization = as.Date(brazil_cve_dates$dt_hospitalization, format= "%m/%d/%y")
# brazil_cve_dates$dt_itu_admission = as.Date(brazil_cve_dates$dt_itu_admission, format= "%m/%d/%y")
# brazil_cve_dates$dt_itu_discharge = as.Date(brazil_cve_dates$dt_itu_discharge, format= "%m/%d/%y")
# brazil_cve_dates$dt_death_or_discharge = as.Date(brazil_cve_dates$dt_death_or_discharge, format= "%m/%d/%y")
# brazil_cve_dates$dt_symptom_onset = as.Date(brazil_cve_dates$dt_symptom_onset, format= "%m/%d/%y")
# 
# 
# # a few data entry errors
# brazil_cve_dates$dt_hospitalization[brazil_cve_dates$dt_hospitalization > as.Date("2020-05-04")] = NA
# brazil_cve_dates$dt_itu_admission[brazil_cve_dates$dt_itu_admission > as.Date("2020-05-04")] = NA
# brazil_cve_dates$dt_itu_discharge[brazil_cve_dates$dt_itu_discharge > as.Date("2020-05-04")] = NA
# brazil_cve_dates$dt_death_or_discharge[brazil_cve_dates$dt_death_or_discharge > as.Date("2020-05-04")] = NA
# 
# # Length of hosptial stay distribution
# LOS_absolute <- as.numeric(brazil_cve_dates$dt_death_or_discharge - brazil_cve_dates$dt_hospitalization)
# LOS_ITU_absolute <- as.numeric(brazil_cve_dates$dt_itu_discharge - brazil_cve_dates$dt_itu_admission)
# hist(LOS_absolute)
# hist(LOS_ITU_absolute)
# 
# # trim cases from the past month to avoid overepresenting short hospital stays
# LOS_absolute <- LOS_absolute[brazil_cve_dates$dt_hospitalization <= "2020-04-04"]
# LOS_ITU_absolute <- LOS_ITU_absolute[brazil_cve_dates$dt_itu_admission <= "2020-04-04"]
# #hist(LOS_absolute)
# #hist(LOS_ITU_absolute)
# 
# # remove NAs
# LOS_absolute = LOS_absolute[!is.na(LOS_absolute)]
# LOS_ITU_absolute = LOS_ITU_absolute[!is.na(LOS_ITU_absolute)]
# 
# # define funcitons and save
# LOS <- function(times) sample(LOS_absolute, times, replace = T)
# LOS_ITU <- function(times) sample(LOS_ITU_absolute, times, replace = T)
# 
# #save(LOS, file = "CC_Intermediate_data_obj/LOS_Brazil_function.RData")
# save(LOS_absolute, file = paste0(dir_data_objects,"LOS_Brazil_data.RData"))
# #save(LOS_ITU, file = "CC_Intermediate_data_obj/LOS_ITU_Brazil_function.RData")
# save(LOS_ITU_absolute, file = paste0(dir_data_objects,"LOS_ITU_Brazil_data.RData"))
# 
# # proportion of cases hospitalised
# brazil_cve_dates_final = brazil_cve_dates[brazil_cve_dates$dt_symptom_onset <= "2020-04-04", ]
# #sum(brazil_cve_dates_final$hospitalised) / nrow(brazil_cve_dates_final) # 0.920202
# 
# # proportion of hospitalised cases that require ITU at some point during their stay
# #sum(brazil_cve_dates_final$icu) / sum(brazil_cve_dates_final$hospitalised) # 0.3807172
# 
# # CFR among cases > 1 month old (resolved cases)
# #table(brazil_cve_dates_final$outcome)[1] / sum(table(brazil_cve_dates_final$outcome)[1:2]) # 0.3417722 












