### 
## This code deploys the app to the CMMID server each day - ported from Felipe's machine
###
## Paul Mee 8th June 2021
###


###############
### Directory set up
### Update this with your local directories
###############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))
setwd(dir_app)

library(rsconnect)

deployApp(appName="lacpt", 
          account="cmmid-lshtm", 
          forceUpdate = getOption("rsconnect.force.update.apps", TRUE),
          launch.browser = FALSE)