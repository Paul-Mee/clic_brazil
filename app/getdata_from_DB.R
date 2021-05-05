##
## LONDON SCHOOL OF HYGIENE AND TROPICAL MEDICINE (LSHTM)
##
## ############################################################################
##
## DISCLAIMER: 
## This script has been developed for research purposes only. 
## The script is provided without any warranty of any kind, either express or 
## implied. The entire risk arising out of the use or performance of the sample
## script and documentation remains with you. 
## In no event shall LSHTM, its author, or anyone else involved in the 
## creation, production, or delivery of the script be liable for any damages 
## whatsoever (including, without limitation, damages for loss of business 
## profits, business interruption, loss of business information, or other 
## pecuniary loss) arising out of the use of or inability to use the sample
## scripts or documentation, even if LSHTM has been advised of the
## possibility of such damages. 
##
## ############################################################################
##
## DESCRIPTION
## Shiny App for the visualisation of local area COVID19 data
##
## Version control: GitHub
## Initially created on 22 April 2020
##
##
## Written by: Oliver Brady, Paul Mee, Felipe J Colon-Gonzalez and
## Neal Alexander
## For any problems with this code, please contact: Oliver.Brady@lshtm.ac.uk
## 
## ############################################################################

# options(rsconnect.max.bundle.size=50000000000)

# Required packages
if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(gghighlight)) install.packages("gghighlight", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
# if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
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
# if(!require(scico)) install.packages("scico", repos = "http://cran.us.r-project.org")
if(!require(sf)) install.packages("sf", repos = "http://cran.us.r-project.org")
if(!require(rdrop2)) install.packages("rdrop2", repos = "http://cran.us.r-project.org")
# if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(plotROC)) install.packages("plotROC", repos = "http://cran.us.r-project.org")

# Load global variables
app_title <- "COVID-19 Local Information Comparison (CLIC Brazil)"

options(shiny.sanitize.errors = TRUE)

myDir <- "~/Documents/GitHub/lacpt"
setwd(myDir)

source(file.path(myDir, "input_data", "OB_standardisation_functions.R"))

Measure   <- "Age standardised incidence"
DateUntil <- Sys.Date()

drop_auth(rdstoken = "~/Documents/GitHub/downloader_lacpt/token.rds")
# drop_acc()

# Search and download latest version available on site
target <- gsub("-", "_", DateUntil)
target <- "BigStandard_even_days"
sear   <- drop_search(target)
try({
    drop_download(sear$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target2 <- "current_total_cases"
sear2   <- drop_search(target2)
try({
    drop_download(sear2$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target3 <- "Trends_plots2020_06_23"
sear3   <- drop_search(target3)
try({
    drop_download(sear3$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target4 <- "peak.rds"
sear4   <- drop_search(target4)
try({
    drop_download(sear4$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target5 <- "AUCplot.rdata"
sear5   <- drop_search(target5)
try({
    drop_download(sear5$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target6 <- "AUCDF.rdata"
sear6   <- drop_search(target6)
try({
    drop_download(sear6$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target7 <- "Brazil_rt_prediction."
sear7   <- drop_search(target7, mode="filename")
try({
    drop_download(sear7$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target8 <- "tmp_Brazil_rt_prediction_select_cities."
sear8   <- drop_search(target8, mode="filename")
try({
    drop_download(sear8$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

target9 <- gsub("-", "_", DateUntil)
target9 <- "Brazil_BigStandard_results_red_size2021_04_15"
sear9   <- drop_search(target9)
try({
    drop_download(sear9$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})


target10 <- "Trends_plots_test2021_04_27"
sear10   <- drop_search(target10)
try({
    drop_download(sear10$matches[[1]]$metadata$path_display,
                  local_path = 'input_data/',
                  overwrite=TRUE)
})

