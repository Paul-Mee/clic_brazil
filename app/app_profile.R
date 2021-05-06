if(!require(profvis)) install.packages("profvis", repos = "http://cran.us.r-project.org")

setwd("./app")

profvis({
       source(file.path( "app.R"))
       })


profvis({
  source(file.path( "app_test_PM.R"))
})