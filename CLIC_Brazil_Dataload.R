
######################################
### R script to process Brazil case data 
###############################################

### Required libraries
require(httr)
require(jsonlite)
require(dplyr)
require(reshape2)
require(data.table)
require(covid19br)

### Data clean up


##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# get daily update 
today <- Sys.Date()
today <- format(today, format="%d-%B-%Y")



##############
### API call to get data 
#############

### Brazil.IO data 
## Get all data in csv format 
# brazil_io_csv <- scan (gzcon(rawConnection(content( GET("https://data.brasil.io/dataset/covid19/caso.csv.gz")))),what="",sep="\n")  
# brazil_cases <- data.frame(strsplit(brazil_io_csv, ",")) 
# 
#  row.names(brazil_cases) <- brazil_cases[,1]
#  # transpose data 
#  brazil_cases <- t(brazil_cases[,-1])
#  # delete row names
#  row.names(brazil_cases) <- c()
#  
#  brazil_cases <- data.table::data.table(brazil_cases)
# # 
# # Format dates
#  brazil_cases$date <- as.Date(brazil_cases$date, format = "%Y-%m-%d")
# # 
# # Keep city level data 
#   brazil_cases_io <- brazil_cases[ which(brazil_cases$place_type=='city'),]
# # Encode city names correctly  
#   Encoding(brazil_cases_io$city) <- "UTF-8"
#   
#   vars <- c("date", "state", "city" , "confirmed" , "deaths" , "city_code")
#   brazil_cases_io <- brazil_cases_io[, ..vars]  

# brazil_cases_dat  <- brazil_cases_io

#####

# Brazil MoH data

### Get Brazil MoH COVID data 

cities.dt <- covid19br::downloadCovid19("cities")

### rename variables for compatibility with previous code

cities.dt$confirmed <- cities.dt$accumCases
cities.dt$deaths <- cities.dt$accumDeaths


### Select only those places where city name is not blank

cities.dt <-filter(cities.dt , city!="")


### Convert to 7 digit ibge codes
### Format IBGE code to as character
cities.dt$city_ibge_code <- as.character(cities.dt$city_code)
## Read IBGE data 
ibge_sd.df <- read.csv(paste0(dir_ibge_data,'Municipalities_sociodemographics_chars_UNICODE.csv'))
## Keep only IBGE code column and name
ibge_data.df <- ibge_sd.df[c("Codigo","Munic_pio")]
## Create column with with 6 digits
ibge_data.df$ibge_code_6 <- substring( as.character(ibge_data.df$Codigo),1,6)
## Merge with existing data 
cities.dt <- merge(cities.dt,ibge_data.df,by.x='city_ibge_code',by.y='ibge_code_6')


### New dt subset of columns

vars <- c("date", "state", "city" , "confirmed" , "deaths" , "Codigo")
brazil_cases_moh <- cities.dt[, ..vars]

names(brazil_cases_moh)[6] <- "city_code"

brazil_cases_dat  <- brazil_cases_moh

#fname <- paste0(dir_source_data,"brazil_raw_cases_api_", today,".csv")
#write.csv(brazil_io_full,file = fname,row.names=FALSE)

##############
### STEP 1 Data formatting from API download format  
#############

# ### Cases data 


brazil_cases_dat  <- brazil_cases_moh

## to test limiting to data until end of Sept 2020
#brazil_cases_dat <- brazil_cases_dat %>% dplyr::filter(date < as.Date("30-09-2020","%d-%m-%Y"))

# Remove cases which cannot be assigned to a particular municipality 
brazil_cases_dat <- brazil_cases_dat[ which(!brazil_cases_dat$city=='Importados/Indefinidos'),]

######
### This next section deals with the situation where the total cases decreases on a particular day 
### this is corrected by subtracting the decrease from the next days total
### this is continued in a loop until the dat increases each day 
#####


### Sort by Date , State and Municipality 
brazil_cases_dat <- brazil_cases_dat[with(brazil_cases_dat, order(state, city, date)), ]

brazil_cases_dat$confirmed <- as.numeric(as.character(brazil_cases_dat$confirmed))
brazil_cases_dat$deaths <- as.numeric(as.character(brazil_cases_dat$deaths))

# Drop rows where confirmed = 0 
brazil_cases_dat <- brazil_cases_dat[ which(!brazil_cases_dat$confirmed==0),]

# incremental increases in cases
brazil_cases_dat <- brazil_cases_dat %>%
  group_by(state,city) %>%
  mutate(case_inc = confirmed - dplyr::lag(confirmed))


# incremental increases in deaths
brazil_cases_dat <- brazil_cases_dat %>%
  group_by(state,city) %>%
  mutate(death_inc = deaths - dplyr::lag(deaths))

brazil_cases_dat <- data.table::data.table(brazil_cases_dat)

### First entry = NA so replace with confirmed value
brazil_cases_dat$case_inc <- ifelse(is.na(brazil_cases_dat$case_inc), brazil_cases_dat$confirmed, brazil_cases_dat$case_inc)
brazil_cases_dat$death_inc <- ifelse(is.na(brazil_cases_dat$death_inc), brazil_cases_dat$deaths, brazil_cases_dat$death_inc)


print(paste( "The total number of cases before correction = " , as.character(sum(brazil_cases_dat$case_inc)) ,sep="")) 
print(paste( "The total number of deaths before correction = " , as.character(sum(brazil_cases_dat$death_inc)) ,sep="")) 


### Generate a data table with all dates between the start of data collection and today

date.min <- min(brazil_cases_dat$date)
date.max <- max(brazil_cases_dat$date)
all.dates <- seq(date.min, date.max, by="day")

# Convert all dates to a data table 
all.dates.frame <- data.frame(list(date=all.dates))
all.dates <- copy(as.data.table(all.dates))
all.dates$merge_col <- "A"

# Merge all cities and dates 
all_cities <- brazil_cases_dat[, .(city, state, city_ibge_code)]
all_cities <- unique(all_cities, by =c("city", "state", "city_ibge_code"))
all_cities$merge_col <- "A"

all_dates_cities <- merge(all.dates,all_cities,by="merge_col",allow.cartesian=TRUE)
all_dates_cities <- all_dates_cities[, .(all.dates,city, state, city_ibge_code)]
names(all_dates_cities)[1] <- "date"

### Merge Municipality data to dates - missing days should be NULL
brazil_cases_dat_fill <- merge(all_dates_cities,brazil_cases_dat,by=c("date","city_ibge_code"),all.x=TRUE)
### Keep only required data 
names(brazil_cases_dat_fill)[1] <- "date"
names(brazil_cases_dat_fill)[2] <- "city_ibge_code"
names(brazil_cases_dat_fill)[3] <- "city"
names(brazil_cases_dat_fill)[4] <- "state"

brazil_cases_dat_fill <- brazil_cases_dat_fill[, .(date, city_ibge_code, city, state, case_inc, death_inc)]






### Replace NA with 0 in case increment and death increment - where no increrements were reported on a particular day 
brazil_cases_dat_fill$case_inc <- ifelse(is.na(brazil_cases_dat_fill$case_inc), 0, brazil_cases_dat_fill$case_inc)
brazil_cases_dat_fill$death_inc <- ifelse(is.na(brazil_cases_dat_fill$death_inc), 0, brazil_cases_dat_fill$death_inc)
# order data
brazil_cases_dat_fill <- brazil_cases_dat_fill[with(brazil_cases_dat_fill, order(state, city, date)), ]



### For negative values of cases substract from next days value - repeat until all increments >= 0 
i_count =  0 
repeat{ 
  i_count <- i_count + 1 
  print(paste("Case correction cycle #",as.character(i_count)," - Current lowest case increase", as.character(min(brazil_cases_dat_fill$case_inc)) ))  
  ## Create a column of negative increments 
  brazil_cases_dat_fill$neg_case_inc <- ifelse( (brazil_cases_dat_fill$case_inc < 0 ), brazil_cases_dat_fill$case_inc, 0)
  ## Set to case_inc to 0 if negative and on the last day of reporting 
  brazil_cases_dat_fill$case_inc <- ifelse((brazil_cases_dat_fill$case_inc < 0 & brazil_cases_dat_fill$date ==  date.max ), 0, brazil_cases_dat_fill$case_inc)
  ## Create a column where negative increments are one row lower by group
  brazil_cases_dat_fill <- brazil_cases_dat_fill %>%
    group_by(city_ibge_code) %>%
    arrange(date) %>%
    mutate(neg_case_inc_next =  dplyr::lag(neg_case_inc, default = first(neg_case_inc)))
  ## add case_inc to neg_case_inc_next
  brazil_cases_dat_fill$case_inc_corr <- brazil_cases_dat_fill$case_inc + brazil_cases_dat_fill$neg_case_inc_next
  ## Replace previous negative with zero 
  brazil_cases_dat_fill$case_inc_corr  <- ifelse( (brazil_cases_dat_fill$case_inc < 0 ), 0, brazil_cases_dat_fill$case_inc_corr)
  ## Replace case_inc value with case_inc_corr
  brazil_cases_dat_fill <- brazil_cases_dat_fill[c(1,2,3,4,9,6)]
  names(brazil_cases_dat_fill)[5] <- "case_inc"
  if(min(brazil_cases_dat_fill$case_inc)==0) {
    break
  }
} 

print(paste( "The total number of cases after correction = " , as.character(sum(brazil_cases_dat_fill$case_inc)) ,sep="")) 

### For negative values of deaths  substract from next days value - repeat until all increments >= 0 
i_count =  0 
repeat{ 
  i_count <- i_count + 1 
  print(paste("Deaths correction cycle #",as.character(i_count)," - Current lowest deaths increase", as.character(min(brazil_cases_dat_fill$death_inc)) ))  
  ## Create a column of negative increments 
  brazil_cases_dat_fill$neg_death_inc <- ifelse( (brazil_cases_dat_fill$death_inc < 0 ), brazil_cases_dat_fill$death_inc, 0)
  ## Set to case_inc to 0 if negative and on the last day of reporting 
  brazil_cases_dat_fill$death_inc <- ifelse((brazil_cases_dat_fill$death_inc < 0 & brazil_cases_dat_fill$date ==  date.max ), 0, brazil_cases_dat_fill$death_inc)
  ## Create a column where negative increments are one row lower by group
  brazil_cases_dat_fill <- brazil_cases_dat_fill %>%
    group_by(city_ibge_code) %>%
    arrange(date) %>%
    mutate(neg_death_inc_next =  dplyr::lag(neg_death_inc, default = first(neg_death_inc)))
  ## add death_inc to neg_death_inc_next
  brazil_cases_dat_fill$death_inc_corr <- brazil_cases_dat_fill$death_inc + brazil_cases_dat_fill$neg_death_inc_next
  ## Replace previous negative with zero 
  brazil_cases_dat_fill$death_inc_corr  <- ifelse( (brazil_cases_dat_fill$death_inc < 0 ), 0, brazil_cases_dat_fill$death_inc_corr)
  ## Replace case_inc value with case_inc_corr
  brazil_cases_dat_fill <- brazil_cases_dat_fill[c(1,2,3,4,5,9)]
  names(brazil_cases_dat_fill)[6] <- "death_inc"
  if(min(brazil_cases_dat_fill$death_inc)==0) {
    break
  }
} 

print(paste( "The total number of deaths after correction = " , as.character(sum(brazil_cases_dat_fill$death_inc)) ,sep="")) 

### Recalculate cumulative totals
brazil_cases_dat_fill <- mutate(group_by(brazil_cases_dat_fill,city_ibge_code), case_cum=cumsum(case_inc))
brazil_cases_dat_fill <- mutate(group_by(brazil_cases_dat_fill,city_ibge_code), death_cum=cumsum(death_inc))

## Encoding city names
#brazil_cases_dat_fill$city <- as.character(brazil_cases_dat_fill$city)
#Encoding(brazil_cases_dat_fill$city) <- "UTF-8"

### Keep only cumulative totals
brazil_cases_dat_fill <- brazil_cases_dat_fill[c(1,2,3,4,7,8)]



### get data in the format date(yyyy-mm-dd),Area_Name,State,confirmed,city_ibge_code for cases data.table
### get data in the format date(yyyy-mm-dd),Area_Name,State,deaths,city_ibge_code for deaths data.table

names(brazil_cases_dat_fill)[1] <- "date"
names(brazil_cases_dat_fill)[3] <- "Area_Name"
names(brazil_cases_dat_fill)[4] <- "State"
names(brazil_cases_dat_fill)[2] <- "City_ibge_code"
names(brazil_cases_dat_fill)[5] <- "cases"
names(brazil_cases_dat_fill)[6] <- "deaths"


# order columns
brazil_cases_dat_fill <- brazil_cases_dat_fill[with(brazil_cases_dat_fill, order(date, Area_Name, State,City_ibge_code,cases,deaths )), ]

## Getting case data in right format

brazil_cases_dat_output <- reshape2::dcast(brazil_cases_dat_fill, brazil_cases_dat_fill$State + brazil_cases_dat_fill$Area_Name + 
                            brazil_cases_dat_fill$City_ibge_code~brazil_cases_dat_fill$date, value.var = "cases")

## substrings of column names to get correct data format from yyyy-mm-dd to Xdd_mm_yyyy
names(brazil_cases_dat_output)[4:ncol(brazil_cases_dat_output)] <- paste("X",substring(names(brazil_cases_dat_output)[4:ncol(brazil_cases_dat_output)],9,10),"_",
                                                           substring(names(brazil_cases_dat_output)[4:ncol(brazil_cases_dat_output)],6,7),"_",
                                                           substring(names(brazil_cases_dat_output)[4:ncol(brazil_cases_dat_output)],1,4),sep="")
## Replace NA with 0
brazil_cases_dat_output[is.na(brazil_cases_dat_output)] <- 0


## Subset for output
brazil_cases_dat_output <- brazil_cases_dat_output[c(2,1,4:ncol(brazil_cases_dat_output),3)]
names(brazil_cases_dat_output)[1] <- "Area_Name"
names(brazil_cases_dat_output)[2] <- "State"
names(brazil_cases_dat_output)[ncol(brazil_cases_dat_output)] <- "City_ibge_code"
# IBGE code as number for back compatibility
brazil_cases_dat_output$City_ibge_code <- as.numeric(as.character(brazil_cases_dat_output$City_ibge_code))

fname <- paste0(dir_daily_data,"brazil_daily_cases_ibge_api_", today,".csv")
fname_RDS <- paste0(dir_formatted_case_data,"brazil_daily_cases_ibge_api.RDS")

write.csv(brazil_cases_dat_output,file = fname,row.names=FALSE)
### Saving as RDS file
saveRDS(brazil_cases_dat_output, file = fname_RDS) 



 
# ################
# ### Deaths data 
# ################
## Getting death data in right format

brazil_deaths_dat_output <- reshape2::dcast(brazil_cases_dat_fill, brazil_cases_dat_fill$State + brazil_cases_dat_fill$Area_Name + 
                            brazil_cases_dat_fill$City_ibge_code~brazil_cases_dat_fill$date, value.var = "deaths")

## substrings of column names to get correct data format from yyyy-mm-dd to Xdd_mm_yyyy
names(brazil_deaths_dat_output)[4:ncol(brazil_deaths_dat_output)] <- paste("X",substring(names(brazil_deaths_dat_output)[4:ncol(brazil_deaths_dat_output)],9,10),"_",
                                                           substring(names(brazil_deaths_dat_output)[4:ncol(brazil_deaths_dat_output)],6,7),"_",
                                                           substring(names(brazil_deaths_dat_output)[4:ncol(brazil_deaths_dat_output)],1,4),sep="")
## Replace NA with 0
brazil_deaths_dat_output[is.na(brazil_deaths_dat_output)] <- 0


## Subset for output
brazil_deaths_dat_output <- brazil_deaths_dat_output[c(2,1,4:ncol(brazil_deaths_dat_output),3)]
names(brazil_deaths_dat_output)[1] <- "Area_Name"
names(brazil_deaths_dat_output)[2] <- "State"
names(brazil_deaths_dat_output)[ncol(brazil_deaths_dat_output)] <- "City_ibge_code"
# IBGE code as number for back compatibility
brazil_deaths_dat_output$City_ibge_code <- as.numeric(as.character(brazil_deaths_dat_output$City_ibge_code))

fname <- paste0(dir_daily_data,"brazil_daily_deaths_ibge_api_", today,".csv")
fname_RDS <- paste0(dir_formatted_death_data,"brazil_daily_deaths_ibge_api.RDS")

## Save csv file
write.csv(brazil_deaths_dat_output,file = fname,row.names=FALSE)
### Saving as RDS file
saveRDS(brazil_deaths_dat_output, file = fname_RDS) 



