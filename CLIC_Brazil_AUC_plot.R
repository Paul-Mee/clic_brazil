
####
#### Code to call the Peak Prediction application 
####

require(MASS)
require(survival)
require(ggplot2)
require(scales)
require(ROCR)
require(plotROC)

library(pROC, pos = "package:base")

##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads functions to be used for standardisation
source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))
this_date <- Sys.Date()


StateRegionDF<-read.csv(
  paste0(dir_geo_data,"brazil_states_regions.csv"),
  stringsAsFactors = FALSE, header=T)
#head(StateRegionDF)

# DateSuffixVector<-rev(c(
#   "2021_07_21",
#   "2021_06_21",
#   "2021_05_21",
#   "2021_04_21",
#   "2021_03_21",
#   "2021_02_21",
#   "2021_01_21",
#   "2020_12_21",
#   "2020_11_21",
#   "2020_10_21",
#   "2020_09_21",
#   "2020_08_21",
#   "2020_07_21"
# ))
DateSuffixVector<-rev(c(
  as.character(this_date)
 ))

# summary(junk)

# # https://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
# substrRight <- function(x, n){substr(x, nchar(x)-n+1, nchar(x))}
# substrRight(x=c("aaa", "bbb", "ccc"), 2)

ResultsDF<-data.frame(DateSuffix=character(), Region=character(), AUC=numeric(), 
                      threshold=numeric(), specificity=numeric(), sensitivity=numeric(),
                 stringsAsFactors=FALSE) 
#ResultsDF

RegionVector<-sort(unique(StateRegionDF$Region))



source(paste0(dir_scripts,"CLIC_Brazil_peak_pred_function.R"))


for(j in 1:length(DateSuffixVector)){
# for(j in 1:1){
   # PredictDF<-AUCfn("2020_07_21", FolderName="C:\\Users\\eidenale\\Dropbox\\COVID_cities\\CC_Intermediate_data_obj")
   # PredictDF<-AUCfn(
   #    FolderName="C:\\Users\\eidenale\\Dropbox\\COVID_cities\\CC_Intermediate_data_obj_archive")
   
   PredictDF<-AUCfn(
      FolderName=dir_app_data, 
      dir_script=dir_scripts, 
      dir_data=dir_data_objects, 
      dir_covar=dir_covariates,
      TestNE=F, verbose=T, 
      LatestDate=as.Date(DateSuffixVector[j], format="%Y_%m_%d"))
  
   dim(PredictDF)
   PredictDF<-merge(x=PredictDF, y=StateRegionDF, by="State", all.x=T, all.y=F)
   dim(PredictDF)
   
   # print(head(PredictDF))
 
   # https://stackoverflow.com/questions/53423655/roc-curve-using-plotroc-package-and-geom-roc-transforming-data-to-m1-markers
   defaultW <- getOption("warn") 
   options(warn = -1) 
   PredictDF$RecordObserved<-sign(PredictDF[, "EventsObserved"])
   print("table(PredictDF$RecordObserved) from all Brazil:")
   print( table(PredictDF$RecordObserved))
   rocfit<-NULL
   try(rocfit <- roc(
     response = PredictDF$RecordObserved, 
     predictor= PredictDF[, "PredictProb"]))
   if(!is.null(rocfit)){
      print(unlist(list("AUC from pROC package"=auc(rocfit))))
      # print(coords(rocfit, x = "b"))
      # coords(rocfit, x = "b")[c("threshold", "specificity", "sensitivity")]
      coordsAUC<-coords(rocfit, x = "b")
      options(warn = defaultW)

      AUCplot <- ggplot(PredictDF,
         aes(d=sign(EventsObserved), m=PredictProb)) + geom_roc()
      AUCplot
      AUCDF<-calc_auc(AUCplot)
      AUCDF
      print(round(AUCDF[1, "AUC"], 3))
   
      ResultsDF<-rbind(
        ResultsDF, 
        data.frame(DateSuffix=DateSuffixVector[j], Region="BR", AUC=AUCDF[1, "AUC"],
             threshold  =coordsAUC["threshold"], 
             specificity=coordsAUC["specificity"], 
             sensitivity=coordsAUC["sensitivity"]))

      for(i in 1:length(RegionVector)){
      
         print(RegionVector[i])
         # https://stackoverflow.com/questions/53423655/roc-curve-using-plotroc-package-and-geom-roc-transforming-data-to-m1-markers
         defaultW <- getOption("warn") 
         options(warn = -1) 
         rocfit<-NULL
         print("table(PredictDF$RecordObserved) from this region:")
         print( table(PredictDF[PredictDF$Region==RegionVector[i], "RecordObserved"]))
         try(rocfit <- roc(
           response =     PredictDF[PredictDF$Region==RegionVector[i], "RecordObserved"], 
           predictor=     PredictDF[PredictDF$Region==RegionVector[i], "PredictProb"]))
           # response =sign(PredictDF[PredictDF$Region==RegionVector[i], "EventsObserved"]), 
         if(!is.null(rocfit)){
            # print(unlist(list("AUC from pROC package"=auc(rocfit))))
            # print(coords(rocfit, x = "b"))
            # coords(rocfit, x = "b")[c("threshold", "specificity", "sensitivity")]
            coordsAUC<-coords(rocfit, x = "b")
            options(warn = defaultW)

            AUCplot <- ggplot(PredictDF[PredictDF$Region==RegionVector[i],],
               aes(d=sign(EventsObserved), m=PredictProb)) + geom_roc()
            AUCDF<-calc_auc(AUCplot)
   
            # print(paste0("AUC for ", RegionVector[i], "=", round(AUCDF[1, "AUC"], 3)))
      
            ResultsDF<-rbind(
              ResultsDF,
              data.frame(
                DateSuffix=DateSuffixVector[j], Region=RegionVector[i], AUC=AUCDF[1, "AUC"],
                threshold  =coordsAUC["threshold"], 
                specificity=coordsAUC["specificity"], 
                sensitivity=coordsAUC["sensitivity"]))
         }else{
            print("No results added for this region/date combination.")
         }
      }
      print(ResultsDF)

   }else{
      print("No results added for this date")
   }
} 


# write.csv(ResultsDF, file="AUC results.csv", row.names = FALSE)

# summary stats for paper
# round(tapply(
#   ResultsDF[ResultsDF$Region!="BR", "sensitivity"], 
#   ResultsDF[ResultsDF$Region!="BR", "Region"     ], mean), 2)
# round(tapply(
#   ResultsDF[ResultsDF$Region!="BR", "specificity"], 
#   ResultsDF[ResultsDF$Region!="BR", "Region"     ], mean), 2)
# round(tapply(
#   ResultsDF[ResultsDF$Region!="BR", "AUC"], 
#   ResultsDF[ResultsDF$Region!="BR", "Region"     ], mean), 2)
# round(tapply(
#   ResultsDF[ResultsDF$Region!="BR", "AUC"], 
#   ResultsDF[ResultsDF$Region!="BR", "Region"     ], min), 2)
# round(tapply(
#   ResultsDF[ResultsDF$Region!="BR", "AUC"], 
#   ResultsDF[ResultsDF$Region!="BR", "Region"     ], max), 2)

# run from here if reusing AUC
# but need RegionVector, see above

ResultsDF<-read.csv("AUC results.csv",
              stringsAsFactors = FALSE)
ResultsDF

ResultsDF$Date<-as.Date(ResultsDF$DateSuffix, "%Y_%m_%d")
head(ResultsDF)

# lacptDF<-read.csv("C:\\Users\\eidenale\\work\\project\\other\\Zika\\CADDE\\COVID\\Manaus\\data_covid19_lacpt_2021-02-18.csv", stringsAsFactors = FALSE)
# lacptDF<-read.csv("C:\\Users\\eidenale\\work\\project\\other\\Zika\\CADDE\\COVID\\Manaus\\data_covid19_lacpt_2021-03-27.csv", stringsAsFactors = FALSE)
# lacptDF<-read.csv("C:\\Users\\eidenale\\work\\project\\other\\Zika\\CADDE\\COVID\\Manaus\\data-2021-07-01.csv", stringsAsFactors = FALSE)
#lacptDF<-read.csv("C:\\Users\\eidenale\\work\\project\\other\\Zika\\CADDE\\COVID\\Manaus\\data-2021-08-11.csv", stringsAsFactors = FALSE)
#dim(lacptDF)
# head(lacptDF[,1:10])
# head(lacptDF[,c("city_state", "area", "region", "date_end", "cum_cases")])
#head(lacptDF[,c("area", "region", "date_end", "cum_cases")])
# "region" is really state
#names(lacptDF)<-ifelse(names(lacptDF)=="region", "State", names(lacptDF))
# names(lacptDF)
# dim(lacptDF)
# lacptDF<-merge(x=lacptDF, y=StateRegionDF, by="State", all.x=T, all.y=F)
# dim(lacptDF)

lacptDF<-lacptDF[, c("date_end", "cum_cases", "area", "Region", "date_end", "cum_cases")]

lacptAggDF<-aggregate(cum_cases ~ date_end + Region, FUN=sum, data=lacptDF)

        lacptAggDF$Date<-as.Date(lacptAggDF$date_end, "%Y-%m-%d")
   head(lacptAggDF)
summary(lacptAggDF$Date)

# all Brazil
lacptAggBRDF<-aggregate(cum_cases ~ Date, FUN=sum, data=lacptAggDF[,c("cum_cases", "Date")])
lacptAggBRDF$DailyCases<-c(lacptAggBRDF$cum_cases[1], diff(lacptAggBRDF$cum_cases))
head(lacptAggBRDF)
lacptAggBRDF$Region<-"BR"
# loop through regions and append
for(i in 1:length(RegionVector)){
   print(RegionVector[i])
   lacptAggScratchDF<-aggregate(cum_cases ~ Date, FUN=sum, 
      data=lacptAggDF[lacptAggDF$Region==RegionVector[i], c("cum_cases", "Date")])
   lacptAggScratchDF$DailyCases<-c(lacptAggScratchDF$cum_cases[1], diff(lacptAggScratchDF$cum_cases))
   head(lacptAggScratchDF)
   lacptAggScratchDF$Region<-RegionVector[i]
   lacptAggBRDF<-rbind(lacptAggBRDF, lacptAggScratchDF)
}

# replace the original DF
lacptAggDF<-lacptAggBRDF

plot(x=lacptAggDF[lacptAggDF$Region=="BR", "Date"], 
     y=lacptAggDF[lacptAggDF$Region=="BR", "DailyCases"], type="b")

# summary(lacptAggDF$DailyCases)
# head(lacptAggDF)
# head(lacptAggDF$Date, 50)

# make a DF with all the days from first date to last date in the file
min(lacptAggDF$Date)
max(lacptAggDF$Date)
LengthPeriodScalar<-as.numeric(max(lacptAggDF$Date)-min(lacptAggDF$Date))
LengthPeriodScalar
DateVector<-min(lacptAggDF$Date)+seq(0, LengthPeriodScalar, by=1)
head(DateVector)
min(DateVector)
max(DateVector)

# vector of regions  including "BR" (the whole country)
RegionLongVector<-sort(unique(lacptAggDF$Region))

GridDF<-data.frame(Date=DateVector)
dim(GridDF)
for(i in 1:length(RegionLongVector)){
   print(RegionLongVector[i])
   GridCasesScratchDF<-merge(
     x=GridDF, 
     y=lacptAggDF[lacptAggDF$Region==RegionLongVector[i],c("Region", "Date", "DailyCases")], 
     by="Date", all.x=T, all.y=T)
   GridCasesScratchDF$DailyCases<-ifelse(is.na(GridCasesScratchDF$DailyCases), 0, GridCasesScratchDF$DailyCases)
   GridCasesScratchDF$Region    <-RegionLongVector[i]
   print( dim(GridCasesScratchDF))
   print(head(GridCasesScratchDF))
   if(i==1){
      GridCasesDF<-GridCasesScratchDF
   }else{
      GridCasesDF<-rbind(GridCasesDF, GridCasesScratchDF)
   }
}
 dim(GridCasesDF)
head(GridCasesDF)

WindowScalar<-14

# work out rolling cases for each region, then append
for(i in 1:length(RegionLongVector)){
   print(RegionLongVector[i])
   GridCasesScratchDF<-GridCasesDF[GridCasesDF$Region==RegionLongVector[i],]
   GridCasesScratchDF<-GridCasesScratchDF[order(GridCasesScratchDF$Date),]
   GridCasesScratchDF$DailyCasesRolling<-NA
   for(j in WindowScalar:nrow(GridCasesScratchDF)){
     GridCasesScratchDF[j, "DailyCasesRolling"]<-mean(
       GridCasesScratchDF[I(j-(WindowScalar-1)):j, "DailyCases"])
   }
   # print(head(GridCasesScratchDF, 20))
     
   if(i==1){
      GridCasesRollingDF<-GridCasesScratchDF
   }else{
      GridCasesRollingDF<-rbind(GridCasesRollingDF, GridCasesScratchDF)
   }
}
 dim(GridCasesRollingDF)
head(GridCasesRollingDF)

GridCasesDF<-GridCasesRollingDF

plot(
  x=GridCasesDF[GridCasesDF$Region=="BR", "Date"], 
  y=GridCasesDF[GridCasesDF$Region=="BR", "DailyCasesRolling"], type="b")

GridCasesDF<-GridCasesDF[GridCasesDF$Date>=min(ResultsDF$Date) & GridCasesDF$Date<=max(ResultsDF$Date),]
head(GridCasesDF)

ggplot(ResultsDF, aes(x=Date)) +
  geom_line(aes(y=AUC)) +
  facet_wrap(vars(Region))

# https://www.r-graph-gallery.com/line-chart-dual-Y-axis-ggplot2.html
# http://www.sthda.com/english/wiki/ggplot2-axis-ticks-a-guide-to-customize-tick-marks-and-labels
# https://stackoverflow.com/questions/30018187/changing-tick-intervals-when-x-axis-values-are-dates/30020683

MinCasesScalar<-min(GridCasesDF$DailyCasesRolling, na.rm=T)
MaxCasesScalar<-max(GridCasesDF$DailyCasesRolling, na.rm=T)
RangeCasesScalar <- MaxCasesScalar-MinCasesScalar

GridCasesDF$log10DailyCasesRolling<-log10(GridCasesDF$DailyCasesRolling)
MinCasesLog10Scalar<-min(GridCasesDF$log10DailyCasesRolling, na.rm=T)
MaxCasesLog10Scalar<-max(GridCasesDF$log10DailyCasesRolling, na.rm=T)
RangeCasesLog10Scalar <- MaxCasesLog10Scalar-MinCasesLog10Scalar

# MinOriginalScale<-0.75
# MaxOriginalScale<-0.8
MinOriginalScale<-0.90
MaxOriginalScale<-1.05

   hist(MinOriginalScale + ((GridCasesDF$DailyCasesRolling-MinCasesScalar) / (RangeCasesScalar/(MaxOriginalScale-MinOriginalScale))))
summary(MinOriginalScale + ((GridCasesDF$DailyCasesRolling-MinCasesScalar) / (RangeCasesScalar/(MaxOriginalScale-MinOriginalScale))))

# png("AUC_Cases_Date.png", height=480, width=480*16/9)
ggplot(ResultsDF, aes(x=Date)) +
  geom_line(aes(y=AUC)) +
  geom_hline(yintercept=0.7, linetype="dashed") +
  geom_line(data=GridCasesDF, 
     aes(y=MinOriginalScale + ((DailyCasesRolling-MinCasesScalar) / (RangeCasesScalar/(MaxOriginalScale-MinOriginalScale))))) +
  scale_y_continuous(
    name = "AUC", breaks=seq(0.6, 0.9, by=0.1),
    sec.axis = sec_axis(~((.-MinOriginalScale)*(RangeCasesScalar/(MaxOriginalScale-MinOriginalScale)))+MinCasesScalar, 
       name="daily cases", breaks=c(0, 4, 8)*10000)
  ) +
  scale_x_date(breaks=as.Date(
    c("2020-08-01", "2020-10-01", "2020-12-01", "2021-02-01")), 
    date_labels = "%d-%b-%y"
  ) +
  facet_wrap(vars(Region))
#      aes(y=MinOriginalScale + (DailyCasesRolling / (RangeScalar/(MaxOriginalScale-MinOriginalScale))))) +
#     c("2020-08-01", "2020-09-01", "2020-10-01", "2020-11-01", "2020-12-01", "2021-01-01", "2021-02-01")), 
# dev.off()
  

# log10 cases

# hist(MinOriginalScale + ((GridCasesDF$log10DailyCasesRolling-MinCasesLog10Scalar) / 
#                            (RangeLog10Scalar/(MaxOriginalScale-MinOriginalScale))))
hist(MinOriginalScale + ((GridCasesDF$log10DailyCasesRolling-MinCasesLog10Scalar) / 
                           (RangeCasesLog10Scalar/(MaxOriginalScale-MinOriginalScale))))
hist(GridCasesDF$DailyCasesRolling)
range(GridCasesDF$DailyCasesRolling)

# Log10CasesFunctionOfAUC<-function(AUC, MinAUCScale, MaxAUCScale, minLog10Cases, maxLog10Cases){
#    ((AUC-MinOriginalScale)*(RangeLog10Scalar/(MaxOriginalScale-MinOriginalScale)))+MinOriginalScale
# }
Log10CasesFunctionOfAUC<-function(AUC, MinAUCScale, MaxAUCScale, minLog10Cases, maxLog10Cases){
   ((AUC-MinOriginalScale)*((maxLog10Cases-minLog10Cases)/(MaxOriginalScale-MinOriginalScale)))+MinOriginalScale
}

AUCFunctionOfLog10Cases<-function(log10Cases, MinAUCScale, MaxAUCScale, minLog10Cases, maxLog10Cases){
  MinAUCScale + ((log10Cases-minLog10Cases) / 
                           ((maxLog10Cases-minLog10Cases)/(MaxAUCScale-MinAUCScale)))
}
hist(AUCFunctionOfLog10Cases(GridCasesDF$log10DailyCasesRolling, 
     MinAUCScale=MinOriginalScale, MaxAUCScale=MaxOriginalScale, 
     minLog10Cases=MinCasesLog10Scalar, maxLog10Cases=MaxCasesLog10Scalar))

BreakVector<-AUCFunctionOfLog10Cases(log10(c(2000, 10000, 50000)), 
     MinAUCScale=MinOriginalScale, MaxAUCScale=MaxOriginalScale, 
     minLog10Cases=MinCasesLog10Scalar, maxLog10Cases=MaxCasesLog10Scalar)
BreakVector
LabelVector=c("2", "10", "50")

AUCFunctionOfLog10Cases(c(1, 1.8), 
     MinAUCScale=MinOriginalScale, MaxAUCScale=MaxOriginalScale, 
     minLog10Cases=MinCasesLog10Scalar, maxLog10Cases=MaxCasesLog10Scalar)

AUCFunctionOfLog10Cases(MinCasesLog10Scalar, 
     MinAUCScale=MinOriginalScale, MaxAUCScale=MaxOriginalScale, 
     minLog10Cases=MinCasesLog10Scalar, maxLog10Cases=MaxCasesLog10Scalar)

Log10CasesFunctionOfAUC(c(0.9, 1), 
     MinAUCScale=MinOriginalScale, MaxAUCScale=MaxOriginalScale, 
     minLog10Cases=MinCasesLog10Scalar, maxLog10Cases=MaxCasesLog10Scalar)

png("AUC_Cases_Date.png", height=480, width=480*16/9)
ggplot(ResultsDF, aes(x=Date)) +
  geom_line(aes(y=AUC),
     colour=c("blue")) +
  geom_hline(yintercept=0.7, linetype="dashed") +
  geom_line(data=GridCasesDF, 
     aes(y=MinOriginalScale + (
       (log10DailyCasesRolling-MinCasesLog10Scalar) / (RangeCasesLog10Scalar/(MaxOriginalScale-MinOriginalScale))
       )),
     colour="darkgreen"
     ) +
  scale_y_continuous(
    name = "AUC (blue line)\n", breaks=seq(0.6, 0.9, by=0.1),
    sec.axis = sec_axis(
       ~((.-MinOriginalScale)*(RangeCasesLog10Scalar/(MaxOriginalScale-MinOriginalScale)))+MinCasesLog10Scalar, 
       name="thousands of daily cases (log scale, green line)\n", 
       breaks=log10(c(1000, 10000, 50000)), labels=c("1", "10", "50"))
  ) +
  scale_x_date(breaks=as.Date(
    c("2020-08-01", "2020-11-01", "2021-02-01", "2021-05-01")), 
    date_labels = "%d-%b-%y"
  ) +
  theme(axis.text=element_text(size=11),
        axis.title=element_text(size=13)) +
  facet_wrap(vars(Region)) + 
     theme(strip.text = element_text(size=12))
dev.off()

#     c("2020-08-01", "2020-10-01", "2020-12-01", "2021-02-01")), 


# , margin = margin(t = 0, r = 20, b = 0, l = 0)

#   , breaks=log10(c(10000, 40000, 80000)))
#   scale_color_manual(values="blue") +
# breaks=seq(0.6, 2.4, by=0.4)
# , labels=LabelVector