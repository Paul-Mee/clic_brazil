

##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))


### libraries required
require(ggplot2)
require(sf)
require(tmap)
require(gridExtra)
require(dplyr)
require(stringr)


fname <-  paste0(dir_data_objects,"Brazil_BigStandard_results.RData")
print(fname)

load(fname)

Brazil_cases = BigStandard$standardised_incidence
Brazil_deaths = BigStandard$standardised_deaths



# preprocessing to re-route beginign of the epidemic depending on chosen area
c_dat = re.route.origin(BigStandard$standardised_incidence, daysSince = 0.01, Zerotrim = FALSE)

# and add intervention timing data
c_dat = district.start.date.find(c_dat, BigStandard$Intervention)



# all interventions
int_opts <- colnames(c_dat)[grepl("start", colnames(c_dat))]
int_opts = int_opts[!int_opts == "Days_since_start"]


# loop through interventions aggregating at the area level
int_first <- matrix(NA, nrow = length(unique(c_dat$Area)), ncol = length(int_opts))
for(i in 1:length(int_opts)){
  int_first[, i] = aggregate(as.formula(paste0(int_opts[i], " ~ Area")), data = c_dat, FUN = min)[, 2]
}
colnames(int_first)= gsub("_start", "", int_opts)


# reformat into a data frame
int_first = data.frame(Area = sort(unique(c_dat$Area)),
                       int_first)

# now reshape into long format
Int_long <- reshape(int_first,
                    times = colnames(int_first)[-1],
                    varying = list(2:ncol(int_first)),
                    direction = "long")

# formatting
Int_long$time = as.character(Int_long$time)
Int_long$time = gsub("_", " ", Int_long$time)

# reordering to maintain alphabetical order
Int_long$time <- factor(Int_long$time,
                        levels = sort(unique(Int_long$time)),ordered = TRUE)

# standardise intervention column name
colnames(Int_long)[3] = "Intervention_type"



rm(list= ls()[!(ls() %in% c('Int_long','dir_app_data','log_fil_dir'))])

save.image( file = paste0(dir_app_data,"inter_plot_files.RDS"))

