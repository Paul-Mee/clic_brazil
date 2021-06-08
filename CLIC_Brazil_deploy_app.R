### 
## This code deploys the app to the CMMID server each day - ported from Felipe's machine
###
## Paul Mee 8th June 2021
###

setwd("C:/CADDE_dropbox/Dropbox/COVID_cities/lacpt")
library(rsconnect)
deployApp(appName="lacpt", 
          account="cmmid-lshtm", 
          forceUpdate = getOption("rsconnect.force.update.apps", TRUE),
          launch.browser = FALSE)