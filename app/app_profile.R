if(!require(profvis)) install.packages("profvis", repos = "http://cran.us.r-project.org")

setwd("./app")

profvis({
       source(file.path( "app.R"))
       })


profvis({
  source(file.path( "app_test_PM.R"))
})

start_time <- Sys.time()
x_dat <- readRDS("./input_data/x_dat.RDS")
end_time <- Sys.time()
end_time - start_time

x_dat <- readRDS("./input_data/x_dat.RDS")

start_time <- Sys.time()
profvis({
  x_dat <- readRDS("./input_data/x_dat.RDS")
})
end_time <- Sys.time()
end_time - start_time