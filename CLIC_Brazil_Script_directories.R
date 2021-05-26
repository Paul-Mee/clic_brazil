##############
### List of directories used by other scripts
### Pulled out as a separate file to prevent conflicts between different copies of scripts
##############

dir_source_data <-  "C:/github/data/brazil_io/"
## This directory will grow in size and should ideally be written direct to the cloud
dir_daily_data <-  "C:/CADDE_dropbox/Dropbox/daily_covid_data/"
dir_formatted_case_data <-  "C:/CADDE_dropbox/Dropbox/COVID_cities/input_data/"
dir_formatted_death_data <-  "C:/CADDE_dropbox/Dropbox/COVID_cities/input_data/"
dir_scripts <- "C:/github/clic_brazil/"
dir_pop_age_dist <- "C:/github/clic_brazil/pop_age_dist/"
dir_case_age_dist <- "C:/github/clic_brazil/case_age_dist/"
dir_interventions <- "C:/github/clic_brazil/interventions/"
dir_ibge_data <-  "C:/github/clic_brazil/ibge/"
dir_covariates <- "C:/github/clic_brazil/covariates/"
dir_data_objects <- "C:/CADDE_dropbox/Dropbox/COVID_cities/input_data/"
epi_filter_file_sources <- "C:/github/clic_brazil/epifilter/R files/main/"
dir_Rt_data <-  "C:/CADDE_dropbox/Dropbox/COVID_cities/input_data/"
dir_peak_data <- "C:/CADDE_dropbox/Dropbox/COVID_cities/input_data/"
dir_app_data <- "C:/CADDE_dropbox/Dropbox/COVID_cities/input_data/"
dir_geo_data <-  "C:/github/clic_brazil/geo_data/"
dir_results <-  "C:/github/data/results/"

if(Sys.info()[['user']]=="eidenale"){
   # dir_source_data          <- "C:/github/data/brazil_io/"
   # dir_daily_data           <- "C:/CADDE_dropbox/Dropbox/daily_covid_data"
   dir_formatted_case_data  <- "C:/Users/eidenale/Dropbox/COVID_cities/input_data"
   dir_formatted_death_data <- "C:/Users/eidenale/Dropbox/COVID_cities/input_data"
   dir_scripts              <- "C:/Users/eidenale/Documents/clic_brazil/"
   dir_pop_age_dist         <- "C:/Users/eidenale/Documents/clic_brazil/pop_age_dist/"
   dir_case_age_dist        <- "C:/Users/eidenale/Documents/clic_brazil/case_age_dist/"
   dir_interventions        <- "C:/Users/eidenale/Documents/clic_brazil/interventions/"
   dir_ibge_data            <- "C:/Users/eidenale/Documents/clic_brazil/ibge/"
   dir_covariates           <- "C:/Users/eidenale/Documents/clic_brazil/covariates/"
   dir_data_objects         <- "C:/Users/eidenale/Dropbox/COVID_cities/input_data/"
   epi_filter_file_sources  <- "C:/Users/eidenale/Documents/clic_brazil/epifilter/R files/main/"
   dir_Rt_data              <- "C:/Users/eidenale/Dropbox/COVID_cities/input_data"
   dir_peak_data            <- "C:/Users/eidenale/Dropbox/COVID_cities/input_data"
   dir_app_data             <- "C:/Users/eidenale/Dropbox/COVID_cities/input_data"
   # dir_geo_data             <- "C:/github/clic_brazil/geo_data/"
   # dir_results              <- "C:/github/data/results/"
}
