### 
## This code generates the Trends plots
###
## Paul Mee 11-May-2021
###

##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))

# libraries
require(ggplot2) ## plotting
require(data.table) ## optimise merge



# direct read


# load in pre-computed Big Standard dataset
fname <-  paste0(dir_data_objects,"Brazil_BigStandard_results.RData")
load(fname)


# preprocessing to re-route beginign of the epidemic depending on chosen area
c_dat = as.data.table(re.route.origin(BigStandard$standardised_incidence))
## Merge covariates
# Get covariates data 
load(paste0(dir_covariates,"Brazil_mun_covs.RData"))
names(SDI)[names(SDI) == 'Area_Name'] <- 'Area'
names(SDI)[names(SDI) == 'SDI_index'] <- 'SDI'
SDI <- as.data.table(SDI)
## Join data tables
c_dat <- copy(c_dat[SDI,  on = "Area"])
# drop if date_end is NA
c_dat <- c_dat [!is.na(c_dat$date_end), ]
## back to data frame - for back compatibility
c_dat <- as.data.frame(c_dat)


# and add intervention timign data
c_dat = district.start.date.find(c_dat, BigStandard$Intervention)


Geographic_scale_opts <- c("National", unique(c_dat$Region))

# exclude state = DF because it only has one municipality
Geographic_scale_opts = Geographic_scale_opts[Geographic_scale_opts != "DF"]


# Names list for storage
Trends_plot_list <- list()
length(Trends_plot_list) <-  length(Geographic_scale_opts)
names(Trends_plot_list) <- Geographic_scale_opts

for(i in 1:length(Geographic_scale_opts)){
  print(i)
  print(Geographic_scale_opts[i])
  if(Geographic_scale_opts[i] != "National"){
    trend_c_DT <- c_dat[c_dat$Region == Geographic_scale_opts[i], ]
  }else{
    trend_c_DT  <- as.data.table(c_dat)
  }

  
  # and remove municipalities with NAs in their covariate data
  trend_c_DT = trend_c_DT[!is.na(trend_c_DT$SDI), ]
  
  
  ### Assigns each municipality to a quartile for population density , SDI , Piped water , Sewage and travel file 
  
  popdenDF<-aggregate(popden ~ Area, max, data=trend_c_DT)
  #head(popdenDF)
  popdenQuartile<-quantile(popdenDF$popden, probs=(0:4)/4)
  popdenDF$popdenQuartile <- cut(popdenDF$popden, popdenQuartile, include.lowest=TRUE)
  Q_labels <- paste0("Q", 1:4)
  Q_labels[1] = paste0(Q_labels[1], " (Lowest density)")
  Q_labels[4] = paste0(Q_labels[4], " (Highest density)")
  levels(popdenDF$popdenQuartile) <- Q_labels
  popdenDF$popdenQuartile<-as.character(popdenDF$popdenQuartile)
  #table(popdenDF$popdenQuartile, exclude=NULL)
  
  SDIDF<-aggregate(SDI ~ Area, max, data=trend_c_DT)
  #head(SDIDF)
  SDIQuartile<-quantile(SDIDF$SDI, probs=(0:4)/4)
  SDIDF$SDIQuartile <- cut(SDIDF$SDI, SDIQuartile, include.lowest=TRUE)
  Q_labels <- paste0("Q", 1:4)
  Q_labels[1] = paste0(Q_labels[1], " (Least developed)")
  Q_labels[4] = paste0(Q_labels[4], " (Most developed)")
  levels(SDIDF$SDIQuartile) <- Q_labels
  SDIDF$SDIQuartile<-as.character(SDIDF$SDIQuartile)
  #table(SDIDF$SDIQuartile, exclude=NULL)
  
  PipedDF<-aggregate(Piped_water ~ Area, max, data=trend_c_DT)
  PipedQuartile<-quantile(PipedDF$Piped_water, probs=(0:4)/4)
  PipedDF$PipedQuartile <- cut(PipedDF$Piped_water, PipedQuartile, include.lowest=TRUE)
  Q_labels <- paste0("Q", 1:4)
  Q_labels[1] = paste0(Q_labels[1], " (Least piped water)")
  Q_labels[4] = paste0(Q_labels[4], " (Most piped water)")
  levels(PipedDF$PipedQuartile) <- Q_labels
  PipedDF$PipedQuartile<-as.character(PipedDF$PipedQuartile)
  
  SewDF<-aggregate(Sewage_or_septic ~ Area, max, data=trend_c_DT)
  SewQuartile<-quantile(SewDF$Sewage_or_septic, probs=(0:4)/4)
  SewDF$SewQuartile <- cut(SewDF$Sewage_or_septic, SewQuartile, include.lowest=TRUE)
  Q_labels <- paste0("Q", 1:4)
  Q_labels[1] = paste0(Q_labels[1], " (Least sewerage)")
  Q_labels[4] = paste0(Q_labels[4], " (Most sewerage)")
  levels(SewDF$SewQuartile) <- Q_labels
  SewDF$SewQuartile<-as.character(SewDF$SewQuartile)
  
  TravDF<-aggregate(Travel_time ~ Area, max, data=trend_c_DT)
  TravQuartile<-quantile(TravDF$Travel_time, probs=(0:4)/4)
  TravDF$TravQuartile <- cut(TravDF$Travel_time, TravQuartile, include.lowest=TRUE)
  Q_labels <- paste0("Q", 1:4)
  Q_labels[1] = paste0(Q_labels[1], " (Least accessible)")
  Q_labels[4] = paste0(Q_labels[4], " (Most accessible)")
  levels(TravDF$TravQuartile) <- Q_labels
  TravDF$TravQuartile<-as.character(TravDF$TravQuartile)
  
  
  ## Merge all to a single data frame
  



    AreaProfilesDT<-merge(x=trend_c_DT, y=popdenDF, by="Area", all.x=T, all.y=F)
    AreaProfilesDT<-merge(x=AreaProfilesDT, y=SDIDF, by="Area", all.x=T, all.y=F)
    AreaProfilesDT<-merge(x=AreaProfilesDT, y=PipedDF, by="Area", all.x=T, all.y=F)
    AreaProfilesDT<-merge(x=AreaProfilesDT, y=SewDF, by="Area", all.x=T, all.y=F)
    AreaProfilesDT<-merge(x=AreaProfilesDT, y=TravDF, by="Area", all.x=T, all.y=F)
  
  

  
  ### Defines time points for a box plot every inc_days 
  
  inc_days = 50 
  
  TimePointsVector<-as.numeric(names(table(trend_c_DT$Days_since_start %/% inc_days)))
  TimePointsVector<-inc_days*TimePointsVector[TimePointsVector>0]
  
  QuartileTimeDT<-AreaProfilesDT[AreaProfilesDT$Days_since_start %in% TimePointsVector,
                                 c("Area", "Days_since_start", "standardised_cases", "popdenQuartile", "SDIQuartile", "PipedQuartile",
                                   "SewQuartile", "TravQuartile")]
  #dim(QuartileTimeDF)
  #head(QuartileTimeDF)
  
  # filter out timepoints that don't have all 4 quartiles
  missingQs = table(QuartileTimeDT$Days_since_start, QuartileTimeDT$popdenQuartile)
  missingQsDelete = as.numeric(rownames(missingQs)[apply(missingQs, 1, function(x) any(x == 0))])
  QuartileTimeDT = QuartileTimeDT[!(QuartileTimeDT$Days_since_start %in% missingQsDelete), ]
  
  # head(table(popdenTimeDF$Days_since_start, popdenTimeDF$popdenQuartile))
  p3 <- ggplot(QuartileTimeDT, 
               aes(x=factor(Days_since_start), 
                   y=standardised_cases, 
                   group=interaction(factor(Days_since_start), factor(popdenQuartile)))) + 
    geom_boxplot(aes(fill=factor(popdenQuartile)), outlier.shape=NA) +
    scale_fill_brewer(palette="BuPu",name = "Area population density\n (Quartiles)") +
    labs(x = "Days since start of the outbreak", y = "Cumulative cases per 1,000 people (log scale)") +
    scale_y_log10()
  
  p4 <- ggplot(QuartileTimeDT, 
               aes(x=factor(Days_since_start), 
                   y=standardised_cases, 
                   group=interaction(factor(Days_since_start), factor(SDIQuartile)))
  ) + 
    geom_boxplot(aes(fill=factor(SDIQuartile)), outlier.shape=NA) +
    labs(x = "Days since start of the outbreak", y = "Cumulative cases per 1,000 people (log scale)") +
    scale_fill_brewer(palette="BuPu",name = "Area Socio-demographic\n index (Quartiles)") +
    scale_y_log10()
  
  p5 <- ggplot(QuartileTimeDT, 
               aes(x=factor(Days_since_start), 
                   y=standardised_cases, 
                   group=interaction(factor(Days_since_start), factor(PipedQuartile)))
  ) + 
    geom_boxplot(aes(fill=factor(PipedQuartile)), outlier.shape=NA) +
    labs(x = "Days since start of the outbreak", y = "Cumulative cases per 1,000 people (log scale)") +
    scale_fill_brewer(palette="BuPu",name = "Proportion of households \nwith piped water (Quartiles)") +
    scale_y_log10()
  
  p6 <- ggplot(QuartileTimeDT, 
               aes(x=factor(Days_since_start), 
                   y=standardised_cases, 
                   group=interaction(factor(Days_since_start), factor(SewQuartile)))
  ) + 
    geom_boxplot(aes(fill=factor(SewQuartile)), outlier.shape=NA) +
    labs(x = "Days since start of the outbreak", y = "Cumulative cases per 1,000 people (log scale)") +
    scale_fill_brewer(palette="BuPu",name = "Proportion of households \nconnected to the sewerage \nnetwork (Quartiles)") +
    scale_y_log10()
  
  p7 <- ggplot(QuartileTimeDT, 
               aes(x=factor(Days_since_start), 
                   y=standardised_cases, 
                   group=interaction(factor(Days_since_start), factor(TravQuartile)))
  ) + 
    geom_boxplot(aes(fill=factor(TravQuartile)), outlier.shape=NA) +
    labs(x = "Days since start", y = "Incidence per 1,000 people (log scale, outliers omitted") +
    scale_fill_brewer(palette="BuPu",name = "Travel time to largest \n city in the state \n(Quartiles)") +
    scale_y_log10()
  
  # tie up into a named list for storage
  
  Trends_plot_list[[i]] <- list(Density = p3,
                                SDI = p4,
                                Sewerage = p6,
                                Travel_time = p7)

}

# save pre-compiled plots
cur_filename <- paste0(dir_app_data,"Trends_plots_test.RData")
save(Trends_plot_list, file = cur_filename)




