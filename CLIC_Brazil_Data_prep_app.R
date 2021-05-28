### 
## This code carries out data pre-processing steps needed for the app in order to speed up the load time
###
## Paul Mee 11-May-2021
###

###############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))

# Required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(mapview)) install.packages("mapview", repos = "http://cran.us.r-project.org")
if(!require(shiny.i18n)) install.packages("shiny.i18n", repos = "http://cran.us.r-project.org")
if(!require(markdown)) install.packages("markdown", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(gsubfn)) install.packages("gsubfn", repos = "http://cran.us.r-project.org")
if(!require(rgdal)) install.packages("rgdal", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(rdrop2)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")
if(!require(plotROC)) install.packages("plotROC", repos = "http://cran.us.r-project.org")
if(!require(EpiEstim)) install.packages("EpiEstim", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(zoo)) install.packages("zoo", repos = "http://cran.us.r-project.org")
if(!require(openxlsx)) install.packages("openxlsx", repos = "http://cran.us.r-project.org")
if(!require(shinyBS)) install.packages("shinyBS", repos = "http://cran.us.r-project.org")
if(!require(thematic)) install.packages("thematic", repos = "http://cran.us.r-project.org")
if(!require(bslib)) install.packages("bslib", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")






DateUntil <- Sys.Date()

cut_off_date = "2020-04-01"

# Read data
fname <-  paste0(dir_data_objects,"Brazil_BigStandard_results.RData")
load(fname)

# Calculate total cases
c_dat <- BigStandard$standardised_incidence

c_dat <- BigStandard$standardised_incidence

date_max <- max(c_dat$date_end)
c_dat <- c_dat %>% dplyr::filter(c_dat$date_end==date_max)
total <- sum(c_dat$cum_cases)
rm(c_dat)

peakDF        <- readRDS(paste0(dir_peak_data,"Peak.rds"))
all_plot_data <- readRDS(paste0(dir_Rt_data,"Brazil_rt_prediction-current.RDS"))


# Subset of columns from Rt_data
Rt_vars <- c("Date","city_state","Rt_Smooth","Rt_Smooth_LCI", "Rt_Smooth_UCI")
all_plot_data <- all_plot_data[Rt_vars]


# Covariates 
load(paste0(dir_covariates,"Brazil_mun_covs.RData"))
covar_dat <- SDI
rm(SDI)
covar_vars <-  c("Area_Name","popden", "SDI_index","Piped_water","Sewage_or_septic", "Travel_time" )
covar_dat <- covar_dat[covar_vars]
names(covar_dat)[1] <- "Area"
names(covar_dat)[3] <- "sdi"
names(covar_dat)[4] <- "piped_water"
names(covar_dat)[5] <- "sewage_or_septic"
names(covar_dat)[6] <- "travel_time"


load(paste0(dir_peak_data,"AUCplot.rdata"))
load(paste0(dir_peak_data,"AUCDF.rdata"))


# Look at max Rt value by group
max_Rt_vals <- all_plot_data %>% group_by(city_state) %>% top_n(1,Rt_Smooth)

# city/state combinations
city_states <- sort(unique(as.character(all_plot_data$city_state)))

# date  omit NAs
peakDF <- peakDF[!is.na(peakDF$X), ]
peakDF <- peakDF[!is.na(peakDF$PredictProb), ]

# make sf object
peakSF <- st_as_sf(peakDF, coords = c("X", "Y"))




load(file.path(paste0(dir_app_data,"Trends_plots_test.RData")))

# Load State names and abbreviations
states <- readRDS(paste0(dir_app_data,"statesBR.RDS")) %>%
  rename(Region="UF")

Brazil_cases  <- BigStandard$standardised_incidence

Brazil_cases_sp <- Brazil_cases
rm(Brazil_cases)

# Add full State names
Brazil_cases_sp %<>% inner_join(states)

# date trim
Brazil_cases_sp <- Brazil_cases_sp[Brazil_cases_sp$date_end <= DateUntil, ]
Brazil_cases_sp <- Brazil_cases_sp[!is.na(Brazil_cases_sp$X), ]

# trim to just latest number of cumulative cases / incidence
Brazil_cases_cum_cases <- data.table(aggregate(cum_cases ~ Area + X + Y,
                                               data = Brazil_cases_sp, FUN = max))

# extract dates from cv data
min_date <- as.Date(min(Brazil_cases_sp$date_end),"%Y-%m-%d")
max_date <- as.Date(max(Brazil_cases_sp$date_end),"%Y-%m-%d")

Brazil_cases_time <- aggregate(cum_cases ~ date_end,
                               data = Brazil_cases_sp, FUN = sum)

Brazil_cases_sp2   <- Brazil_cases_sp[Brazil_cases_sp$cum_cases > 50, ]
Brazil_cases_areas <- aggregate(Area ~ date_end, data = Brazil_cases_sp2,
                                FUN = length)
rm(Brazil_cases_sp2)

x_dat <- re.route.origin(BigStandard$standardised_incidence)

# and add intervention timing data
x_dat <- district.start.date.find(x_dat, BigStandard$Intervention)



# Filter to only those areas with >50 cum_cases
# x_dat %<>% dplyr::filter(cum_cases > 400)

x_dat$Area   <- as.character(x_dat$Area)
# x_dat$Region <- str_sub(x_dat$Area, start= -2)
x_dat %<>% inner_join(states) %>%
  data.table()




timeSUM <- cbind(aggregate(standardised_cases ~ Days_since_start,
                           data=x_dat, FUN=quantile, probs = 0.33)[, 2],
                 aggregate(standardised_cases ~ Days_since_start,
                           data=x_dat, FUN=quantile, probs = 0.5)[, 2],
                 aggregate(standardised_cases ~ Days_since_start,
                           data=x_dat, FUN=quantile, probs = 0.66)[, 2])

z_dat <- re.route.origin(BigStandard$standardised_incidence,
                         Zerotrim = FALSE)

# and add intervention timing data
z_dat <- district.start.date.find(z_dat, BigStandard$Intervention)



# all interventions
int_opts <- colnames(z_dat)[grepl("start", colnames(z_dat))]
int_opts <- int_opts[!int_opts == "Days_since_start"]


# loop through interventions aggregating at the area level
int_first <- matrix(NA, nrow = length(unique(z_dat$Area)),
                    ncol = length(int_opts))
for(i in 1:length(int_opts)){
  int_first[, i] = aggregate(as.formula(paste0(int_opts[i],
                                               " ~ Area")),
                             data = z_dat, FUN = min)[, 2]
}
colnames(int_first)= gsub("_start", "", int_opts)


# reformat into a data frame
int_first <- data.frame(Area = sort(unique(z_dat$Area)),
                        int_first)

# now reshape into long format
Int_long <- reshape(int_first,
                    times = colnames(int_first)[-1],
                    varying = list(2:ncol(int_first)),
                    direction = "long")

# formatting
Int_long$time <- as.character(Int_long$time)
Int_long$time <- gsub("_", " ", Int_long$time)

# reordering to maintain alphabetical order
Int_long$time <- factor(Int_long$time,
                        levels  = sort(unique(Int_long$time)),
                        ordered = TRUE)

# standardise intervention column name
colnames(Int_long)[3] <- "Intervention_type"



# trim to just latest number of cumulative cases / incidence
popup <- Brazil_cases_cum_cases$cum_cases

my_bks <- c(0, round(exp(seq(log1p(0),
                             log1p(max(Brazil_cases_cum_cases$cum_cases)),
                             length = 5))))

pal <- colorNumeric("YlOrBr", NULL)

# Create size for circle markers
Brazil_cases_cum_cases$size <- log1p(Brazil_cases_cum_cases$cum_cases*3)

# make sf object
db <- st_as_sf(Brazil_cases_cum_cases, coords = c("X", "Y"))

# set crs
st_crs(db)   <- 4326

# Convert to spatial points data frame
spatial      <- as(db, "Spatial")

areas <- as.character(Brazil_cases_sp$Area[Brazil_cases_sp$cum_cases > 100])

data_available <- data.table(areas=unique(sort(areas[!is.na(areas)])))

### adding intervention plot data 

load(paste0(dir_app_data,"inter_plot_files.RDS"))


save.image( file = paste0(dir_app_data,"app_files.RDS"))