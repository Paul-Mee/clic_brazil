### 
## Utility file to plot death data for selected date ranges and municipalities 
###
## Paul Mee 17-Sep-2021


# installing required libraries
library("dplyr")
library("ggplot2")

###############
### Directory set up
### Update this with your local directories
###############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))

# Read latest data
fname <-  paste0(dir_data_objects,"Brazil_BigStandard_results.RData")
load(fname)

c_dat <- BigStandard$standardised_incidence

# calculating daily death count from difference of consecutive cumualtive counts by group


inc_cum_dat   <- c_dat %>%
  group_by(Area)  %>%
  mutate(death_count=cum_deaths-lag(cum_deaths,default=first(cum_deaths)))

inc_cum_dat   <- inc_cum_dat  %>% 
    group_by(Area)  %>%
    mutate(case_count=cum_cases-lag(cum_cases,default=first(cum_cases)))


## Plot data 

## Select Municipality 
select_city_state = "Manaus_AM"

plot_data <- inc_cum_dat [ which(  (inc_cum_dat$Area==select_city_state ) ), ]


## Plot for most recent n days
plot_days <- 100 

plot_data <- plot_data [ which (plot_data$date_end >= ( max(plot_data$date_end) - plot_days) ), ]

### Plot deaths

plot_title <- paste0("Daily deaths for ", select_city_state, " last ",as.character(plot_days), " days")
p1 <- ggplot(data=plot_data,aes(x = date_end, y = death_count, ))+ 
  geom_line() +
  xlab("Date") +
  ylab("Daily deaths") +
  ggtitle(label = plot_title ) +
  scale_x_date(date_breaks = "2 week", date_labels="%d-%b") 
p1

ggsave(paste0(dir_results,"death_counts.png"),plot=p1, width=40, height=16, units="cm")

### Plot cases

plot_title <- paste0("Daily cases for ", select_city_state, " last ",as.character(plot_days), " days")
p2 <- ggplot(data=plot_data,aes(x = date_end, y = case_count, ))+ 
  geom_line() +
  xlab("Date") +
  ylab("Daily cases") +
  ggtitle(label = plot_title ) +
  scale_x_date(date_breaks = "2 week", date_labels="%d-%b") 
p2

ggsave(paste0(dir_results,"case_counts.png"),plot=p2, width=40, height=16, units="cm")



