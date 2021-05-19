# Get values for time of intervention in a particular place 

##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

load(paste0(dir_app_data,"app_files.RDS"))


tmp_place <- "São João das Missões_MG"
#tmp_place <- "São Caetano do Sul_SP"
#tmp_place <- "Manaus_AM"

# precompute a variable that states whether interventions in the local area were later or earlier than the mean
E_L <- Int_long[Int_long$Area == tmp_place, "Intervention_type"] >= aggregate(Intervention_type ~ time, Int_long,FUN = mean)$Intervention_type
Int_long$PlotCol = "black"
Int_long$PlotCol[Int_long$Intervention_type >= 0] = "red"


### Fix variable names for time and intervention type

names(Int_long)[3] <- "tmp"
names(Int_long)[2] <- "Intervention_type"
names(Int_long)[3] <- "time"

# new cumulative plot
# x axis is going to be time (days between intervention deployed and outbreak arrival)
# y axis is going to be number of municipalities

# filtering and ordering
IL_Emergency <- Int_long[Int_long$Intervention_type == "Emergency declared", ]
IL_Emergency = IL_Emergency[order(IL_Emergency$time), ]
IL_Emergency$Order = 1:nrow(IL_Emergency)

IL_Transport <- Int_long[Int_long$Intervention_type == "Transport restrictions", ]
IL_Transport = IL_Transport[order(IL_Transport$time), ]
IL_Transport$Order = 1:nrow(IL_Transport)

IL_Retail <- Int_long[Int_long$Intervention_type == "Industry Retail Service restrictions", ]
IL_Retail = IL_Retail[order(IL_Retail$time), ]
IL_Retail$Order = 1:nrow(IL_Retail)

IL_School <- Int_long[Int_long$Intervention_type == "School closure", ]
IL_School = IL_School[order(IL_School$time), ]
IL_School$Order = 1:nrow(IL_School)


emergency_time <- IL_Emergency %>% filter(Area==tmp_place)  %>% select(time)
emergency_time <- emergency_time[1,1]
transport_time <- IL_Transport %>% filter(Area==tmp_place)  %>% select(time)
transport_time <- transport_time[1,1]
retail_time <- IL_Retail %>% filter(Area==tmp_place)  %>% select(time)
retail_time <- retail_time[1,1]
school_time <- IL_School %>% filter(Area==tmp_place)  %>% select(time)
school_time <- school_time[1,1]




# plots

Emergency_plot <- ggplot(IL_Emergency, aes(x = time, y = Order)) +
  geom_line(aes(colour = PlotCol),size = 1, show.legend = FALSE) +  
  scale_colour_manual(values=c("black", "red")) +
  geom_vline(xintercept = emergency_time, linetype = "longdash", colour = "Black") +
  xlab("Days since first cases detected locally") +
  ylab("Cumulative municipalities") +
  ggtitle("Emergency declared") +
  theme_bw()

Transport_plot <- ggplot(IL_Transport, aes(x = time, y = Order)) +
  geom_line(aes(colour = PlotCol),size = 1, show.legend = FALSE) + 
  scale_colour_manual(values=c("black", "red")) +
  geom_vline(xintercept = transport_time, linetype = "longdash", colour = "Black") +
  xlab("Days since first cases detected locally") +
  ylab("Cumulative municipalities") +
  ggtitle("Transport restrictions") +
  theme_bw()

Retail_plot <- ggplot(IL_Retail, aes(x = time, y = Order)) +
  geom_line(aes(colour = PlotCol),size = 1, show.legend = FALSE) + 
  scale_colour_manual(values=c("black", "red")) +
  geom_vline(xintercept = retail_time, linetype = "longdash", colour = "Black") +
  xlab("Days since first cases detected locally") +
  ylab("Cumulative municipalities") +
  ggtitle("Industry, retail and service restrictions") +
  theme_bw()

School_plot <- ggplot(IL_School, aes(x = time, y = Order)) +
  geom_line(aes(colour = PlotCol),size = 1, show.legend = FALSE) + 
  scale_colour_manual(values=c("black", "red")) +
  geom_vline(xintercept = school_time, linetype = "longdash", colour = "Black") +
  xlab("Days since first cases detected locally") +
  ylab("Cumulative municipalities") +
  ggtitle("School closure") +
  theme_bw()


library(ggpubr)

combined_plot <- ggarrange(Emergency_plot, Transport_plot, Retail_plot, School_plot,
                           ncol = 2, nrow = 2)

combined_plot




