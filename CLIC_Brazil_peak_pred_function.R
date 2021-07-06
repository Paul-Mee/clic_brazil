
# general work flow for git:
# 0) CTRL-SHIFT-F1 to get to the "Environment" pane, then "Pull" (blue arrow)
# 1) edit file
# 2) save locally (as normal)
# 3) in the "Environment" pane, select the file under "Staged" then "Commit"
# 4) in the resulting window, having put a comment for the commit, "Push" (or can push in another window)

# for this particular task of debugging the peak prediction
#
# edit "CLIC_Brazil_Script_directories.R" so that for Neal the folders are set right for him, 
#    by enclosing them in an "if" to be called if he is the user: DONE (at least 1st version)
# then run "CLIC_Brazil_peak_pred_implementation.R" which is a wrapper for the current file


# function to help calculate AUC for different datasets

# based on "PM_peak_batch_v2 alpha-test.R"

 AUCfn <-function(FolderName, TestNE=F){
    
    # use TestNE=T to have a smaller subset of the data (NE region) for the Cox models

# https://stackoverflow.com/questions/47932246/rscript-detect-if-r-script-is-being-called-sourced-from-another-script

# if (sys.nframe() == 0L) {
#     print("The script is being run directly.")
    RoutineUseScalar<-F
# }else{
#     print("The script is being called from another.")
#     RoutineUseScalar<-T
# }

# stop("just avoiding running the whole script while testing.")
    
## libraries
    
require(data.table)
require(survival)
require(ROCR)
require(plotROC)
    
require(coxme)

source(paste0(dir_scripts,"CLIC_Brazil_standardisation_functions.R"))
        


    ## simplified data load
# fname <-  paste0(FolderName,"Brazil_BigStandard_results.RData")
# print(fname)

fname <-  paste0(dir_data_objects,"Brazil_BigStandard_results.RData")
print(fname)
    
load(fname)


### Merge covariates data 



# preprocessing to re-route beginign of the epidemic depending on chosen area
c_dat = as.data.table(re.route.origin(BigStandard$standardised_incidence))

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

detach(package:data.table)


# and add intervention timign data
# c_dat = district.start.date.find(c_dat, BigStandard$Intervention)
# sort(names(c_dat))
# class(c_dat)
AreaProfilesDF <- district.start.date.find(c_dat, BigStandard$Intervention)

# print(head(names(AreaProfilesDF)))

AreaProfilesDF$Days_since_start<-as.numeric(AreaProfilesDF$Days_since_start)

summary(AreaProfilesDF$Days_since_start)

# # will look at different subsets of the data
# # # define the last days in each subset
# # Say the smallest one will be up to day 90, then by 30 days up to the max (i.e. omitting none, the default analysis)
# 
# LastDayStepScalar<-30
# # LastDayStepScalar<-7
# LastDayMinScalar <-90
# Days_since_startMaxScalar<-max(AreaProfilesDF$Days_since_start)
# LastDaySubsetVector<-seq(LastDayMinScalar, LastDayStepScalar*(max(AreaProfilesDF$Days_since_start)%/%LastDayStepScalar), by=LastDayStepScalar)
# if(Days_since_startMaxScalar != max(LastDaySubsetVector)){
#    LastDaySubsetVector <- c(LastDaySubsetVector, Days_since_startMaxScalar)
# }
# LastDaySubsetVector
# 
# # for routine use then only keep the last entry
# if(RoutineUseScalar){
#    LastDaySubsetVector<-tail(LastDaySubsetVector,1)
# }

# AUCVector<-rep(NA, length(LastDaySubsetVector))

# later will save the results under names distinguished by the following suffices
# ObjectNameSuffixVector<-ifelse(LastDaySubsetVector==max(LastDaySubsetVector), "", as.character(LastDaySubsetVector))
# ObjectNameSuffixVector


table(is.na(AreaProfilesDF$popden))

# ReferenceAreaScalar<-"São Paulo_SP"
ReferenceAreaScalar<-"Guarulhos_SP"
# ReferenceAreaScalar<-"Piedade_SP"

# AreaProfilesWholeDF<-AreaProfilesDF


# for(j in 1:length(LastDaySubsetVector)){
   # print(dim(AreaProfilesWholeDF))
   # AreaProfilesDF<-AreaProfilesWholeDF[AreaProfilesWholeDF$Days_since_start<=LastDaySubsetVector[j],]
   # print(dim(AreaProfilesDF))

# sort(names(AreaProfilesDF))

# table(AreaProfilesDF$Region, exclude=NULL)

AreaProfilesSubsetDF<-AreaProfilesDF[AreaProfilesDF$Area == ReferenceAreaScalar,]


AreaProfilesSubsetDF<-AreaProfilesSubsetDF[order(as.numeric(AreaProfilesSubsetDF$Days_since_start)),]

AreaProfilesSubsetDF$standardised_casesDaily<-c(0, diff(AreaProfilesSubsetDF$standardised_cases))

head(AreaProfilesSubsetDF[,c("Days_since_start", "standardised_cases", "standardised_casesDaily")])

# peak number of daily cases
max(AreaProfilesSubsetDF$standardised_casesDaily)
max(AreaProfilesSubsetDF$Days_since_start)

# latest number of cases
AreaProfilesSubsetDF[which.max(AreaProfilesSubsetDF$Days_since_start),"standardised_casesDaily"]

# difference between peak and latest number of cases
max(AreaProfilesSubsetDF$standardised_casesDaily)-AreaProfilesSubsetDF[which.max(AreaProfilesSubsetDF$Days_since_start),"standardised_casesDaily"]

# distribution of latest days across municipalities
AreaProfilesLatestDayDF<-aggregate(Days_since_start ~ Area, max, data=AreaProfilesDF)
head(AreaProfilesLatestDayDF)
summary(AreaProfilesLatestDayDF$Days_since_start)
# order from high to low in terms of max days
AreaProfilesLatestDayDF<-AreaProfilesLatestDayDF[rev(order(as.numeric(AreaProfilesLatestDayDF$Days_since_start))),]
head(AreaProfilesLatestDayDF)

# ggplot(AreaProfilesDF[AreaProfilesDF$Area %in% AreaProfilesLatestDayDF[1:10, "Area"],], 
#    aes(x = Days_since_start, y = log10(standardised_cases), group = Area))  + 
#    geom_line(aes(color=rank(Area)))

# order the main DF by area and day
AreaProfilesDF<-AreaProfilesDF[order(AreaProfilesDF$Area, as.numeric(AreaProfilesDF$Days_since_start)),]
head(AreaProfilesDF[,c("Area", "Days_since_start")])
# View(AreaProfilesDF)
# sort(names(AreaProfilesDF))
# View(AreaProfilesDF[AreaProfilesDF$Area=="Adamantina_SP",])

AreaVector<-as.character(sort(unique(AreaProfilesDF$Area)))
length(AreaVector)

AreaProfilesDailyDF<-data.frame(
   Area=character(),
   Days_since_start=integer(), 
   standardised_cases=double(),
   standardised_casesDaily=double(),
      stringsAsFactors=FALSE)

# calculate new cases by day, by differencing the cumulative cases
for(i in 1:length(AreaVector)){
# for(i in 1:10){
   # print(i)
   AreaProfilesDailysubsetDF<-AreaProfilesDF[
      AreaProfilesDF$Area==AreaVector[i], 
      c("Area", "Days_since_start", "standardised_cases")]
   AreaProfilesDailysubsetDF$standardised_casesDaily<-c(0, diff(AreaProfilesDailysubsetDF$standardised_cases))
   # print(head(AreaProfilesDailysubsetDF))
   AreaProfilesDailyDF<-rbind(AreaProfilesDailyDF, AreaProfilesDailysubsetDF)
   # print(dim(AreaProfilesDailyDF))
   }

AreaProfilesDailyDF<-AreaProfilesDailyDF[,c("Area", "Days_since_start", "standardised_casesDaily")]
# head(AreaProfilesDailyDF)
# View(AreaProfilesDailyDF)

# merge back
dim(AreaProfilesDF)
names(AreaProfilesDF)
AreaProfilesDF<-merge(
   x=AreaProfilesDF, y=AreaProfilesDailyDF, 
   by=c("Area", "Days_since_start"), all.x=T, all.y=T)
dim(AreaProfilesDF)
names(AreaProfilesDF)
AreaProfilesDF<-AreaProfilesDF[order(AreaProfilesDF$Area, as.numeric(AreaProfilesDF$Days_since_start)),]
# View(AreaProfilesDF[, c("Area", "Days_since_start", "standardised_cases", "standardised_casesDaily")])

SubsetVector<-AreaProfilesDF$Area %in% as.character(AreaProfilesLatestDayDF[1:10, "Area"])
table(SubsetVector)
# head(AreaProfilesDF[SubsetVector, c("Area", "Days_since_start", "standardised_cases", "standardised_casesDaily")])

# set the index manually to see one area at a time
   # ggplot(AreaProfilesDF[AreaProfilesDF$Area %in% as.character(AreaProfilesLatestDayDF[8, "Area"]),], 
   #    aes(x = Days_since_start, y = log10(standardised_casesDaily), group = Area))  + 
   #    geom_line(aes(color=rank(Area)))

# how many have more days that the chosen test municipality
table(AreaProfilesLatestDayDF$Days_since_start>max(AreaProfilesSubsetDF$Days_since_start))
table(AreaProfilesLatestDayDF$Days_since_start>max(AreaProfilesSubsetDF$Days_since_start)+14)

# https://stats.stackexchange.com/questions/22974/how-to-find-local-peaks-valleys-in-a-series-of-data
find_peaks <- function (x, m = 1){
    shape <- diff(sign(diff(x, na.pad = FALSE)))
    pks <- sapply(which(shape < 0), FUN = function(i){
       z <- i - m + 1
       z <- ifelse(z > 0, z, 1)
       w <- i + m + 1
       w <- ifelse(w < length(x), w, length(x))
       if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
    })
     pks <- unlist(pks)
     pks
}
find_peaks(c(0, 1, 2, 3, 2, 1, 0))
find_peaks(c(0, 1, 2, 3, 3, 2, 1, 0))
find_peaks(c(0, 1, 2, 3, 3, 3, 2, 1, 0))

# what happens if the first value is higher than the second
find_peaks(c(1, 0, 1, 2, 3, 3, 3, 2, 1, 0))
# ...not defined as a peak

# ...and if the last is the highest...
find_peaks(c(0, 1, 2, 3, 3, 3, 2, 1, 4))
# ...not defined as a peak either

# https://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value
repeat.before = function(x) {   # repeats the last non NA value. Keeps leading NA
    ind = which(!is.na(x))      # get positions of nonmissing values
    if(is.na(x[1]))             # if it begins with a missing, add the 
          ind = c(1,ind)        # first position to the indices
    rep(x[ind], times = diff(   # repeat the values at these indices
       c(ind, length(x) + 1) )) # diffing the indices + length yields how often 
}                               # they need to be repeated



#plot(x=AreaProfilesSubsetDF$Days_since_start, y=AreaProfilesSubsetDF$standardised_casesDaily, type="l")
peaksVector<-find_peaks(AreaProfilesSubsetDF$standardised_casesDaily)

#AreaProfilesSCDF<-AreaProfilesDF[AreaProfilesDF$Area=="São Caetano do Sul_SP",]
#AreaProfilesSCDF<-AreaProfilesSCDF[order(as.numeric(AreaProfilesSCDF$Days_since_start)),]
# plot(x=AreaProfilesSCDF$Days_since_start, 
#      y=AreaProfilesSCDF$standardised_casesDaily, type="l")

#peaksVector<-find_peaks(AreaProfilesSCDF$standardised_casesDaily)
# points(x=AreaProfilesSCDF$Days_since_start[peaksVector], 
#        y=AreaProfilesSCDF$standardised_casesDaily[peaksVector], col="red")

#AreaProfilesSCPeakDF<-AreaProfilesSCDF[peaksVector,c("Days_since_start", "standardised_casesDaily")]
#AreaProfilesSCPeakDF$diff<-c(999, diff(AreaProfilesSCPeakDF$standardised_casesDaily))

# pick out new records
#while(any(AreaProfilesSCPeakDF$diff<0)){
#   AreaProfilesSCPeakDF<-AreaProfilesSCPeakDF[AreaProfilesSCPeakDF$diff>=0,]
#   AreaProfilesSCPeakDF$diff<-c(999, diff(AreaProfilesSCPeakDF$standardised_casesDaily))
#}
#AreaProfilesSCPeakDF$peak<-T
#AreaProfilesSCPeakDF

# points(x=AreaProfilesSCPeakDF$Days_since_start, 
#        y=AreaProfilesSCPeakDF$standardised_casesDaily, col="blue", pch=2)


#AreaProfilesSCDF<-merge(
 #  x=AreaProfilesSCDF, 
 #  y=AreaProfilesSCPeakDF[,c("Days_since_start", "peak")], 
  # by="Days_since_start", all.x=T, all.y=F)

# View(AreaProfilesSCDF)
# AreaProfilesSCDF[,c("Days_since_start", "standardised_cases", "standardised_casesDaily", "peak")]

#AreaProfilesSCDF$CurrentRecord<-ifelse(
  # AreaProfilesSCDF$peak, 
  # AreaProfilesSCDF$standardised_casesDaily, NA
  # )

#AreaProfilesSCDF$RecordAtStartOfDay<-repeat.before(AreaProfilesSCDF$CurrentRecord)
#AreaProfilesSCDF$RecordAtStartOfDay<-c(
 #  NA, 
 #  AreaProfilesSCDF$RecordAtStartOfDay[1:I(length(AreaProfilesSCDF$RecordAtStartOfDay)-1)]
  # )
#
#AreaProfilesSCDF$standardised_casesYesterday<-c(
  # NA, 
 #  AreaProfilesSCDF$standardised_casesDaily[1:I(length(AreaProfilesSCDF$standardised_casesDaily)-1)]
  # )

#AreaProfilesSCDF[,c("Days_since_start", "standardised_casesDaily", "standardised_casesYesterday", "peak", "RecordAtStartOfDay")]

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# head(substrRight(AreaProfilesDF$Area, 2))
# View(AreaProfilesDF[substrRight(as.character(AreaProfilesDF$Area), 2)=="SP",])

# São Caetano do Sul_SP


#rm(AreaProfilesSCDF)

# https://stats.stackexchange.com/questions/353692/modelling-recurrent-events-using-cox-regression-in-r

AreaRecordDF<-data.frame(
   Area=character(),
   Days_since_start=integer(), 
   DayYesterday    =integer(), 
   status          =integer(),
   RecordAtStartOfDay         =double(),
   standardised_casesDaily    =double(),
   standardised_casesYesterday=double(),
      stringsAsFactors=FALSE) 

#    event           =integer(),


for(i in 1:length(AreaVector)){
# for(i in 1:50){
   
   # print(i)

   AreaProfilesSubsetDF<-AreaProfilesDF[AreaProfilesDF$Area == AreaVector[i],]
   AreaProfilesSubsetDF<-AreaProfilesSubsetDF[order(as.numeric(AreaProfilesSubsetDF$Days_since_start)),]

   peaksVector<-find_peaks(AreaProfilesSubsetDF$standardised_casesDaily)

   if(length(peaksVector)>0){

      AreaProfilesPeakDF<-AreaProfilesSubsetDF[peaksVector,c("Days_since_start", "standardised_casesDaily")]
      AreaProfilesPeakDF$diff<-c(999, diff(AreaProfilesPeakDF$standardised_casesDaily))

      while(any(AreaProfilesPeakDF$diff<0)){
         AreaProfilesPeakDF<-AreaProfilesPeakDF[AreaProfilesPeakDF$diff>=0,]
         AreaProfilesPeakDF$diff<-c(999, diff(AreaProfilesPeakDF$standardised_casesDaily))
      }
      AreaProfilesPeakDF$peak<-T

   # require at least two peaks
   if(I(dim(AreaProfilesPeakDF)[1])>=2){

      AreaProfilesSubsetDF<-merge(
         x=AreaProfilesSubsetDF, 
         y=AreaProfilesPeakDF[,c("Days_since_start", "peak")], 
         by="Days_since_start", all.x=T, all.y=F)

      AreaProfilesSubsetDF$CurrentRecord<-ifelse(
         AreaProfilesSubsetDF$peak, 
         AreaProfilesSubsetDF$standardised_casesDaily, NA
         )

      AreaProfilesSubsetDF$RecordAtStartOfDay<-repeat.before(AreaProfilesSubsetDF$CurrentRecord)
      AreaProfilesSubsetDF$RecordAtStartOfDay<-c(
         NA, 
         AreaProfilesSubsetDF$RecordAtStartOfDay[1:I(length(AreaProfilesSubsetDF$RecordAtStartOfDay)-1)]
         )

      AreaProfilesSubsetDF$standardised_casesYesterday<-c(
         NA, 
         AreaProfilesSubsetDF$standardised_casesDaily[1:I(length(AreaProfilesSubsetDF$standardised_casesDaily)-1)]
         )
      
      AreaProfilesSubsetDF$DayYesterday<-c(
         NA, 
         AreaProfilesSubsetDF$Days_since_start[1:I(length(AreaProfilesSubsetDF$Days_since_start)-1)]
         )

      AreaProfilesSubsetDF$GapToRecord<-AreaProfilesSubsetDF$RecordAtStartOfDay - AreaProfilesSubsetDF$standardised_casesYesterday

      AreaProfilesSubsetDF$status<-ifelse(
         is.na(AreaProfilesSubsetDF$peak), 0, as.numeric(AreaProfilesSubsetDF$peak)
         )
      
      # AreaProfilesSubsetDF$event <-NA
      
      AreaRecordDF<-rbind(AreaRecordDF,
            AreaProfilesSubsetDF[,c("Area", "Days_since_start", "DayYesterday", "status",
               "standardised_casesDaily", "standardised_casesYesterday",
               "RecordAtStartOfDay", "GapToRecord")])
      }
   }
}

AreaRecordDF$State<-substrRight(as.character(AreaRecordDF$Area), 2)   

# aggregate candidate predictor variables from the original DF, and merge in
AreaProfilesAggDF<-aggregate(cbind(popden, SDI) ~ Area, mean, na.rm=T, data=AreaProfilesDF) 
head(AreaProfilesAggDF) 

dim(AreaRecordDF) 
AreaRecordDF<-merge(x=AreaRecordDF, y=AreaProfilesAggDF, all.x=T, all.y=F, by="Area") 
dim(AreaRecordDF) 

class(AreaRecordDF$Area) 
AreaRecordDF$Area<-as.character(AreaRecordDF$Area)
class(AreaRecordDF$Area)
table(is.na(AreaRecordDF$Area))

# head(AreaRecordDF)

#AreaRecordSCDF<-AreaRecordDF[AreaRecordDF$Area=="São Caetano do Sul_SP",]
#AreaRecordSCDF<-AreaRecordSCDF[order(AreaRecordSCDF$Days_since_start),]
#AreaRecordSCDF[1:11,]

# merge back the daily cases lagged by 2
# AreaProfilesDF$Day2<-AreaProfilesDF$Days_since_start+2
# AreaProfilesDay2DF<-AreaProfilesDF[,c("Area", "Day2", "standardised_casesDaily")]
# names(AreaProfilesDay2DF)<-ifelse(names(AreaProfilesDay2DF)=="Day2", "Days_since_start", names(AreaProfilesDay2DF))
# names(AreaProfilesDay2DF)<-ifelse(names(AreaProfilesDay2DF)=="standardised_casesDaily", "standardised_casesDaily2", names(AreaProfilesDay2DF))
# summary(AreaProfilesDay2DF)

# lagScalar<-2
for(lagScalar in 2:7){
   AreaProfilesDF[,paste0("Day", lagScalar)]<-AreaProfilesDF$Days_since_start+lagScalar
   AreaProfilesLagDF<-AreaProfilesDF[,c("Area", paste0("Day", lagScalar), "standardised_casesDaily")]
   # summary(AreaProfilesLagDF)
   names(AreaProfilesLagDF)<-ifelse(names(AreaProfilesLagDF)==paste0("Day", lagScalar), "Days_since_start", names(AreaProfilesLagDF))
   # summary(AreaProfilesLagDF)
   names(AreaProfilesLagDF)<-ifelse(names(AreaProfilesLagDF)=="standardised_casesDaily", paste0("standardised_casesDaily", lagScalar), names(AreaProfilesLagDF))
   # summary(AreaProfilesLagDF)

   # dim(AreaRecordDF)
   AreaRecordDF<-merge(x=AreaRecordDF, y=AreaProfilesLagDF, 
                       by=c("Area", "Days_since_start"), all.x=T, all.y=F)
   # dim(AreaRecordDF)

   # # For day 7, save the DF for later use in prediction
   # # This will work for predicting 30 days ahead of the full dataset. 
   # ...to be checked...
   #
   # # For the test/training dataset evaluation it will not work.  
   # if(lagScalar==7){
   #    AreaProfilesLag7DF<-AreaProfilesLagDF
   # }

   AreaRecordDF[,paste0("GapToRecord", lagScalar)]<-
      AreaRecordDF[,"RecordAtStartOfDay"] - AreaRecordDF[,paste0("standardised_casesDaily", lagScalar)]
}

#AreaRecordSCDF<-AreaRecordDF[AreaRecordDF$Area=="São Caetano do Sul_SP",]
#AreaRecordSCDF<-AreaRecordSCDF[order(AreaRecordSCDF$Days_since_start),]
#AreaRecordSCDF[1:11,]
#AreaRecordSCDF[1:8, c("Days_since_start", 
#  "standardised_casesDaily", "standardised_casesYesterday", 
#   paste0("standardised_casesDaily", c("2", "3", "4")))]
#AreaRecordSCDF[1:8, c("Days_since_start", "RecordAtStartOfDay",
#   "standardised_casesDaily", "standardised_casesYesterday", 
#   paste0("GapToRecord", c("", "2", "3", "4")))]

# dim(AreaRecordSCDF)
# AreaRecordSCDF[((dim(AreaRecordSCDF)[1])-7):(dim(AreaRecordSCDF)[1]),c("Days_since_start", 
#    "standardised_casesDaily", "standardised_casesYesterday", 
#    paste0("standardised_casesDaily", c("2", "3", "4")))]
# AreaRecordSCDF[((dim(AreaRecordSCDF)[1])-7):(dim(AreaRecordSCDF)[1]),c("Days_since_start", "RecordAtStartOfDay",
#    "standardised_casesDaily", "standardised_casesYesterday", 
#    paste0("GapToRecord", c("", "2", "3", "4")))]

# AreaRecordDFtemp<-AreaRecordDF[order(c(as.character(AreaRecordDF$Area), AreaRecordDF$Days_since_start)),]
# head(AreaRecordDFtemp)
# table(is.na(AreaRecordDFtemp$Area))


# View(AreaProfilesDF[AreaProfilesDF$Area=="Adamantina_SP",])

# need to fix the following (if any):
# AreaRecordDF[AreaRecordDF$DayStart==AreaRecordDF$DayStop,]


# coxph(Surv(as.numeric(tstart),as.numeric(tstop),as.numeric(status))~ /
# codetype+gender+age+patientIMD+ /
# cluster(id)+strata(event),method="breslow", data=coxModel)

CompleteSubset<-!is.na(AreaRecordDF$Area)         & !is.na(AreaRecordDF$State)        & !is.na(AreaRecordDF$popden) & 
                !is.na(AreaRecordDF$SDI)          & !is.na(AreaRecordDF$GapToRecord)  & !is.na(AreaRecordDF$GapToRecord2) & 
                !is.na(AreaRecordDF$GapToRecord3) & !is.na(AreaRecordDF$GapToRecord4) & !is.na(AreaRecordDF$GapToRecord5) & 
                !is.na(AreaRecordDF$GapToRecord6) & !is.na(AreaRecordDF$GapToRecord7)
table(CompleteSubset)

# AreaCoxphNull<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     1 + frailty(Area),method="breslow", data=AreaRecordDF, subset=CompleteSubset)

AreaCoxphNull<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~
                    1, cluster(Area), method="breslow", data=AreaRecordDF, subset=CompleteSubset)
print("Fitted first cluster model.")

# AreaCoxphNull<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~
#                     (1|Area), data=AreaRecordDF, subset=CompleteSubset)
# print("Fitted first random effect model.")

summary(AreaCoxphNull)


table(is.na(AreaRecordDF$State))

print("dim(AreaRecordDF) before:")
print(dim(AreaRecordDF))

print("### test to remove rows where  DayYesterday = NA")
AreaRecordDF <- AreaRecordDF[!(is.na(AreaRecordDF$DayYesterday)) ,]

print("dim(AreaRecordDF) after:")
print(dim(AreaRecordDF))

# recalculate to make it same length as new DF
CompleteSubset<-!is.na(AreaRecordDF$Area)         & !is.na(AreaRecordDF$State)        & !is.na(AreaRecordDF$popden) & 
                !is.na(AreaRecordDF$SDI)          & !is.na(AreaRecordDF$GapToRecord)  & !is.na(AreaRecordDF$GapToRecord2) & 
                !is.na(AreaRecordDF$GapToRecord3) & !is.na(AreaRecordDF$GapToRecord4) & !is.na(AreaRecordDF$GapToRecord5) & 
                !is.na(AreaRecordDF$GapToRecord6) & !is.na(AreaRecordDF$GapToRecord7)

print("length(CompleteSubset):")
print(length(CompleteSubset))

if(TestNE){
   print("table(CompleteSubset):")
   print(table(CompleteSubset))
   print("Subsetting to NE region for testing purposes...")
   CompleteSubset<-CompleteSubset & 
      AreaRecordDF$State %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
   print("table(CompleteSubset):")
   print(table(CompleteSubset))
}

# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     as.factor(State) + frailty(Area),method="breslow", data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     as.factor(State), cluster(Area), method="breslow", data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     as.factor(State) + (1|Area), data=AreaRecordDF, subset=CompleteSubset)
# summary(AreaCoxph)

# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     popden + frailty(Area),method="breslow", data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     popden, cluster(Area), method="breslow", data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     popden + (1|Area), data=AreaRecordDF, subset=CompleteSubset)
# summary(AreaCoxph)

# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     SDI    + frailty(Area),method="breslow", data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     SDI, cluster(Area), method="breslow", data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
#                     SDI + (1|Area), data=AreaRecordDF, subset=CompleteSubset)
# summary(AreaCoxph)

# the following does not work for some reason
# anova(AreaCoxphNull, AreaCoxph)

# plot(survfit(AreaCoxph))
# 
# plot(survfit(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ as.factor(State), 
#              data=AreaRecordDF))

# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  status)~ 1 + frailty(Area),method="breslow", 
#                  data=AreaRecordDF, subset=CompleteSubset)

AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
                 status)~ 1, cluster(Area),method="breslow",
                 data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  status) ~ (1|Area), data=AreaRecordDF, subset=CompleteSubset)
summary(AreaCoxph)

# AreaCoxph2<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  status)~ GapToRecord + GapToRecord2 + frailty(Area),method="breslow", 
#                  data=AreaRecordDF, subset=CompleteSubset)
AreaCoxph2<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
                 status)~ GapToRecord + GapToRecord2, cluster(Area), method="breslow",
                 data=AreaRecordDF, subset=CompleteSubset)
# AreaCoxph2<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  status)~ GapToRecord + GapToRecord2 + (1|Area), 
#                  data=AreaRecordDF, subset=CompleteSubset)
summary(AreaCoxph2)

# anova(AreaCoxph2, AreaCoxph)
# anova(AreaCoxph, AreaCoxph2)

# https://stackoverflow.com/questions/58588833/why-do-i-get-an-error-in-anova-test-on-cox-models-in-r

# anovaCox<-function(model1, model2){
#    Df = sum(anova(model2)$Df, na.rm = T) - sum(anova(model1)$Df, na.rm = T)
#    Chisq = abs(as.numeric(logLik(model2) - logLik(model1)) * 2)
#    pval = pchisq(Chisq, Df, lower.tail=F)
#    print(unlist(list(Chisq=Chisq, Df=Df, pval=pval)))
# }
# anovaCox(AreaCoxph, AreaCoxph2)
# print("First use of anovaCox().")

anovaCox<-function(model1, model2, Wald=F){
   if(  "coxme" %in% class(model1) &   "coxme" %in% class(model2)){
      # coxme models
      # from coxme manual:
      # "The likelihood for a mixed effects Cox model can be viewed in two ways: the ordinarly partial
      # likelihood, where the random effects act only as a penalty or constraint, or a partial likelihood
      # where the random effect has been integrated out. Both are valid."
      #
      # opt for "Integrated" likelihood because the DF are easier to understand
      logLik1<-model2$loglik["Integrated"]
      logLik2<-model1$loglik["Integrated"]
      Chisq = abs(as.numeric(logLik2 - logLik1) * 2)
      
      Df = model2$df[1] - model1$df[1]
      pval = pchisq(Chisq, Df, lower.tail=F)
      print(unlist(list(Chisq=Chisq, Df=Df, pval=pval)))
   }else{
   if(! "coxme" %in% class(model1) & ! "coxme" %in% class(model2)){
      # assume they are usual cox models
      if(Wald){
         Df    = abs(summary(model2)$waldtest["df"]   - summary(model1)$waldtest["df"])
         Chisq = abs(summary(model2)$waldtest["test"] - summary(model1)$waldtest["test"])
         pval = pchisq(Chisq, Df, lower.tail=F)
         print(unlist(list(Chisq=Chisq, Df=Df, pval=pval)))
      }else{
         Df = sum(anova(model2)$Df, na.rm = T) - sum(anova(model1)$Df, na.rm = T)
         Chisq = abs(as.numeric(logLik(model2) - logLik(model1)) * 2)
         pval = pchisq(Chisq, Df, lower.tail=F)
         print(unlist(list(Chisq=Chisq, Df=Df, pval=pval)))
      }
   }else{
      stop("Models of different classes have been passed to the anovaCox function.")
   }
   }
}


# quit(save="ask")

if(Sys.info()[['user']]=="eidenale"){
   # https://stackoverflow.com/questions/49013427/r-saving-image-within-function-is-not-loading
   # save.image(file = "C:\\Users\\eidenale\\Downloads\\debug.RData")
   save(
      list = ls(all.names = TRUE), 
      file = "C:\\Users\\eidenale\\Downloads\\debug.RData", 
      envir =  environment())
   # load("C:\\Users\\eidenale\\Downloads\\debug.RData")
}

print("summary(AreaCoxph):")
print(summary(AreaCoxph))
# AreaCoxph$loglik
print("summary(AreaCoxph2):")
print(summary(AreaCoxph2))
# class(summary(AreaCoxph2))

# https://stackoverflow.com/questions/19226816/how-can-i-view-the-source-code-for-a-function
# methods(anova)
# getAnywhere(anova.coxme)
# getAnywhere(anova.coxmelist)
# require(coxme)

# https://www.python2.net/questions-175391.htm

# anova(AreaCoxph, AreaCoxph2)
anovaCox(AreaCoxph, AreaCoxph2, Wald=T)
# print("First use of anovaCox on coxme objects has been done.")
print("First use of anovaCox has been done.")


# AreaCoxph7<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord2 + GapToRecord3 + GapToRecord4 + GapToRecord5 + GapToRecord6 + GapToRecord7 + frailty(Area),
#                  method="breslow", data=AreaRecordDF, subset=CompleteSubset)
AreaCoxph7<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),as.numeric(status))~ 
                 GapToRecord + GapToRecord2 + GapToRecord3 + GapToRecord4 + GapToRecord5 + GapToRecord6 + GapToRecord7 +  (1|Area), 
                 data=AreaRecordDF, subset=CompleteSubset)
summary(AreaCoxph7)
anovaCox(AreaCoxph2, AreaCoxph7)

# AreaCoxph167<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + frailty(Area),
#                  method="breslow", data=AreaRecordDF, subset=CompleteSubset)
AreaCoxph167<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
                 as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + (1|Area),
                 data=AreaRecordDF, subset=CompleteSubset)
summary(AreaCoxph167)
anovaCox(AreaCoxph167, AreaCoxph7)

# keep those days but no longer use the subset


# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord2 + GapToRecord7 + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxph)
#
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord7 + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxph)

# AreaCoxphState<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + as.factor(State) + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxphState)
#
# AreaCoxphPopden<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + popden + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxphPopden)
#
# AreaCoxphSDI<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + SDI + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxphSDI)
#
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + as.factor(State) + SDI + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxph)

AreaCoxphFormula<-AreaCoxph$formula
AreaCoxphFormula

# comment out the following two because they seem to be overwritten immediately
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord2 + as.factor(State) + SDI + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxph)
# 
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord2 + SDI + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
# summary(AreaCoxph)
 
# re-do the following one with coxme
# AreaCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + frailty(Area),
#                  method="breslow", data=AreaRecordDF)
AreaCoxph<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
                 as.numeric(status))~ GapToRecord + (1|Area), data=AreaRecordDF)
summary(AreaCoxph)

# make new dataset to predict 30 days ahead
# pick out latest record for each municipality

AreaRecordMaxDF<-aggregate(Days_since_start ~ Area, max, data=AreaRecordDF)
# head(AreaRecordMaxDF)
dim(AreaRecordMaxDF)

AreaRecordPredictDF<-merge(
   x=AreaRecordDF[,c("Area", "Days_since_start", "status", 
                     "standardised_casesDaily",  "standardised_casesYesterday",
                     "RecordAtStartOfDay", "State", "SDI",
                     paste0("standardised_casesDaily", 2:6))], 
   y=AreaRecordMaxDF, all.x=F, all.y=T, 
   by=c("Area", "Days_since_start"))
dim(AreaRecordPredictDF)

table(AreaRecordPredictDF$status)

# start day for prediction is last day of existing data
# this is a new field, not merged in from "AreaRecordDF"
AreaRecordPredictDF$DayYesterday      <-AreaRecordPredictDF$Days_since_start

AreaRecordPredictDF$Days_since_start  <-AreaRecordPredictDF$DayYesterday+30
AreaRecordPredictDF$RecordAtStartOfDay<-ifelse(
   as.logical(AreaRecordPredictDF$status), 
              AreaRecordPredictDF$standardised_casesDaily, 
              AreaRecordPredictDF$RecordAtStartOfDay)

#now shuffle the case-by-day variables by one day
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesDaily6",
                               "standardised_casesDaily7", names(AreaRecordPredictDF))
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesDaily5",
                               "standardised_casesDaily6", names(AreaRecordPredictDF))
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesDaily4",
                               "standardised_casesDaily5", names(AreaRecordPredictDF))
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesDaily3",
                               "standardised_casesDaily4", names(AreaRecordPredictDF))
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesDaily2",
                               "standardised_casesDaily3", names(AreaRecordPredictDF))
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesYesterday",
                               "standardised_casesDaily2", names(AreaRecordPredictDF))
names(AreaRecordPredictDF)<-ifelse(
   names(AreaRecordPredictDF)=="standardised_casesDaily",
                               "standardised_casesYesterday", names(AreaRecordPredictDF))
# AreaRecordPredictDF$standardised_casesYesterday<-AreaRecordPredictDF$standardised_casesDaily

sort(names(AreaRecordPredictDF))

AreaRecordPredictDF$GapToRecord <-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesYesterday
AreaRecordPredictDF$GapToRecord2<-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesDaily2
AreaRecordPredictDF$GapToRecord3<-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesDaily3
AreaRecordPredictDF$GapToRecord4<-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesDaily4
AreaRecordPredictDF$GapToRecord5<-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesDaily5
AreaRecordPredictDF$GapToRecord6<-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesDaily6
AreaRecordPredictDF$GapToRecord7<-AreaRecordPredictDF$RecordAtStartOfDay-AreaRecordPredictDF$standardised_casesDaily7

SubsetNameVector<-c("Area", "status", "DayYesterday", "Days_since_start", "RecordAtStartOfDay", "standardised_casesYesterday", 
   paste0("GapToRecord", c("", as.character(2:7))), "State", "SDI")
SubsetNameVector
AreaRecordPredictDF<-AreaRecordPredictDF[,SubsetNameVector]

AreaRecordPredictDF[AreaRecordPredictDF$Area=="São Caetano do Sul_SP",]

AreaRecordPredictDF$Predict<-predict(AreaCoxph, newdata=AreaRecordPredictDF, type ="expected")
# head(AreaRecordPredictDF$Predict)
# prob of event, which is 1-survival prob
AreaRecordPredictDF$PredictProb<-1-exp(-AreaRecordPredictDF$Predict)

# plot(x=AreaRecordPredictDF$Days_since_start, y=AreaRecordPredictDF$PredictProb)

# merge back in the x and y

# sort(names(AreaProfilesDF))

XDF<-aggregate(X ~ Area, mean, data=AreaProfilesDF, na.rm=T)
YDF<-aggregate(Y ~ Area, mean, data=AreaProfilesDF, na.rm=T)

AreaRecordPredictDF<-merge(x=AreaRecordPredictDF, y=XDF, by="Area", all.x=T, all.y=F)   
AreaRecordPredictDF<-merge(x=AreaRecordPredictDF, y=YDF, by="Area", all.x=T, all.y=F)   

table(is.na(AreaRecordPredictDF$PredictProb))

# eqscplot(AreaRecordPredictDF$X, AreaRecordPredictDF$Y, 
#      col=ifelse(AreaRecordPredictDF$PredictProb<0.25, "steelblue1",
#          ifelse(AreaRecordPredictDF$PredictProb<0.50, "steelblue2",
#          ifelse(AreaRecordPredictDF$PredictProb<0.75, "steelblue3", "steelblue4"))), 
#      pch=20, cex=0.5)

saveRDS(AreaRecordPredictDF, file = (paste0(dir_peak_data,"Peak.rds")))

# evaluate performance by fitting a similar model to data with the last 30 days removed
names(AreaRecordMaxDF)<-ifelse(
   names(AreaRecordMaxDF)=="Days_since_start", "Days_since_startMax", names(AreaRecordMaxDF))
names(AreaRecordMaxDF)

dim(AreaRecordDF)
# make a new DF which will be subsetted for the training DF and further down for the test DF
AreaRecordTestTrainingDF<-merge(
   x=AreaRecordDF[,c("Area", "DayYesterday", "Days_since_start", "status", 
                     "standardised_casesDaily", "standardised_casesYesterday",  
                     "RecordAtStartOfDay", "GapToRecord", "GapToRecord6", "GapToRecord7", "State", "SDI", 
                     paste0("standardised_casesDaily", 2:6))], 
   y=AreaRecordMaxDF, all.x=T, all.y=F, 
   by=c("Area"))
dim(AreaRecordTestTrainingDF)
names(AreaRecordTestTrainingDF)
AreaRecordTrainingDF<-AreaRecordTestTrainingDF[
   AreaRecordTestTrainingDF$Days_since_start-AreaRecordTestTrainingDF$Days_since_startMax<=I(-30),]
dim(AreaRecordTrainingDF)
# AreaRecordTrainingDF[AreaRecordTrainingDF$Area=="São Caetano do Sul_SP",]

# AreaTrainingCoxph<-coxph(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
#                  as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + as.factor(State) + SDI + frailty(Area),
#                  method="breslow",
#                  data=AreaRecordTrainingDF)
AreaTrainingCoxph<-coxme(Surv(as.numeric(DayYesterday),as.numeric(Days_since_start),
                 as.numeric(status))~ GapToRecord + GapToRecord6 + GapToRecord7 + as.factor(State) + SDI + (1|Area),
                 data=AreaRecordTrainingDF)

# re-use the formula from above
# AreaTrainingCoxph<-coxph(AreaCoxphFormula,
#                  method="breslow",
#                  data=AreaRecordTrainingDF)
# summary(AreaTrainingCoxph)

# make the predictions for the next 30 days as if they were unknown
AreaRecordTrainingMaxDF<-aggregate(Days_since_start ~ Area, max, data=AreaRecordTrainingDF)

AreaRecordTrainingPredictDF<-merge(
   x=AreaRecordTrainingDF[,c(
      "Area", "Days_since_start", "status", 
      "standardised_casesDaily", "standardised_casesYesterday",
      "RecordAtStartOfDay", "State", "SDI",
                     paste0("standardised_casesDaily", 2:6))], 
   y=AreaRecordTrainingMaxDF, all.x=F, all.y=T, 
   by=c("Area", "Days_since_start"))
dim(AreaRecordTrainingPredictDF)


# make the prediction dataset along the lines done already for the genuinely unknown data
AreaRecordTrainingPredictDF$DayYesterday      <-AreaRecordTrainingPredictDF$Days_since_start
AreaRecordTrainingPredictDF$Days_since_start  <-AreaRecordTrainingPredictDF$DayYesterday+30
AreaRecordTrainingPredictDF$RecordAtStartOfDay<-ifelse(
   as.logical(AreaRecordTrainingPredictDF$status), 
              AreaRecordTrainingPredictDF$standardised_casesDaily, 
              AreaRecordTrainingPredictDF$RecordAtStartOfDay)

# AreaRecordTrainingPredictDF$standardised_casesYesterday<-AreaRecordTrainingPredictDF$standardised_casesDaily
# AreaRecordTrainingPredictDF$GapToRecord                <-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesYesterday

names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesDaily6",
                               "standardised_casesDaily7", names(AreaRecordTrainingPredictDF))
names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesDaily5",
                               "standardised_casesDaily6", names(AreaRecordTrainingPredictDF))
names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesDaily4",
                               "standardised_casesDaily5", names(AreaRecordTrainingPredictDF))
names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesDaily3",
                               "standardised_casesDaily4", names(AreaRecordTrainingPredictDF))
names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesDaily2",
                               "standardised_casesDaily3", names(AreaRecordTrainingPredictDF))
names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesYesterday",
                               "standardised_casesDaily2", names(AreaRecordTrainingPredictDF))
names(AreaRecordTrainingPredictDF)<-ifelse(
names(AreaRecordTrainingPredictDF)=="standardised_casesDaily",
                               "standardised_casesYesterday", names(AreaRecordTrainingPredictDF))

AreaRecordTrainingPredictDF$GapToRecord <-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesYesterday
AreaRecordTrainingPredictDF$GapToRecord2<-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesDaily2
AreaRecordTrainingPredictDF$GapToRecord3<-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesDaily3
AreaRecordTrainingPredictDF$GapToRecord4<-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesDaily4
AreaRecordTrainingPredictDF$GapToRecord5<-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesDaily5
AreaRecordTrainingPredictDF$GapToRecord6<-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesDaily6
AreaRecordTrainingPredictDF$GapToRecord7<-AreaRecordTrainingPredictDF$RecordAtStartOfDay-AreaRecordTrainingPredictDF$standardised_casesDaily7


# AreaRecordTrainingPredictDF<-AreaRecordTrainingPredictDF[,c(
#    "Area", "status", "DayYesterday", "Days_since_start", "RecordAtStartOfDay", "standardised_casesYesterday", "GapToRecord")]

# SubsetNameVector was defined above
AreaRecordTrainingPredictDF<-AreaRecordTrainingPredictDF[,SubsetNameVector]

# AreaRecordTrainingPredictDF[AreaRecordTrainingPredictDF$Area=="São Caetano do Sul_SP",]

AreaRecordTrainingPredictDF$Predict<-predict(AreaTrainingCoxph, newdata=AreaRecordTrainingPredictDF, type ="expected")
# prob of event, which is 1-survival prob
AreaRecordTrainingPredictDF$PredictProb<-1-exp(-AreaRecordTrainingPredictDF$Predict)
# hist(AreaRecordTrainingPredictDF$PredictProb)
# plot(x=AreaRecordTrainingPredictDF$Days_since_start, y=AreaRecordTrainingPredictDF$PredictProb)
summary(AreaRecordTrainingPredictDF$Predict)

# AreaRecordTrainingPredictDF[AreaRecordTrainingPredictDF$Area=="São Caetano do Sul_SP",]

# check there is one record per area
table(table(AreaRecordTrainingPredictDF$Area))
# View(AreaRecordTrainingPredictDF)

# make a test dataset out of the remaining records in the source data
AreaRecordTestDF<-AreaRecordTestTrainingDF[
   AreaRecordTestTrainingDF$Days_since_start-AreaRecordTestTrainingDF$Days_since_startMax>I(-30),]
dim(AreaRecordTestDF)
# AreaRecordTestDF[AreaRecordTestDF$Area=="São Caetano do Sul_SP",]

AreaRecordTestDF<-aggregate(status ~ Area, sum, data=AreaRecordTestDF, na.rm=T)
names(AreaRecordTestDF)<-ifelse(names(AreaRecordTestDF)=="status", "EventsObserved", names(AreaRecordTestDF))
 # head(AreaRecordTestDF)
table(AreaRecordTestDF$EventsObserved)
# AreaRecordTestDF[AreaRecordTestDF$Area=="São Caetano do Sul_SP",]

# merge into the TrainingDF
dim(AreaRecordTrainingPredictDF)
AreaRecordTrainingPredictDF<-merge(
   x=AreaRecordTrainingPredictDF, 
   y=AreaRecordTestDF, all.x = T, all.y = F, by="Area")
dim(AreaRecordTrainingPredictDF)

# plot(x=AreaRecordTrainingPredictDF$EventsObserved, 
#      y=AreaRecordTrainingPredictDF$Predict)

# cor(x=AreaRecordTrainingPredictDF$EventsObserved, y=AreaRecordTrainingPredictDF$Predict, 
#     use="complete.obs")

# names(AreaRecordTrainingPredictDF)

par(mfrow=c(1,1))

# https://afit-r.github.io/histograms
# ggplot(AreaRecordTrainingPredictDF, aes(x=PredictProb)) + 
#    geom_histogram() +
#         facet_grid(sign(EventsObserved) ~ .)

# practice ROC curve
# https://cran.r-project.org/web/packages/ROCR/ROCR.pdf
# data(ROCR.simple)
# head(ROCR.simple)
# names(ROCR.simple)
# class(ROCR.simple)
# pred <- prediction( ROCR.simple$predictions, ROCR.simple$labels)
# pred
# perf <- performance(pred,"tpr","fpr")
# perf
# plot(perf)
# plot(perf, colorize=TRUE)
# # ...fairly straightforward once you know what argument values to pass to performance()

# predSubsetVector<-!is.na(AreaRecordTrainingPredictDF$PredictProb) & !is.na(AreaRecordTrainingPredictDF$EventsObserved)
# pred <- prediction(
#    predictions=     AreaRecordTrainingPredictDF$PredictProb[predSubsetVector],
#         labels=sign(AreaRecordTrainingPredictDF$EventsObserved[predSubsetVector]))
# perf <- performance(pred,"tpr","fpr")
# plot(perf, colorize=TRUE)

# https://stackoverflow.com/questions/41523761/how-to-compute-auc-with-rocr-package
# auc_ROCR <- performance(pred, measure = "auc")
# auc_ROCR@y.values[[1]]

# practice ROC curve from the plotROC package, with ggplot
# https://cran.r-project.org/web/packages/plotROC/vignettes/examples.html

# D.ex <- rbinom(200, size = 1, prob = .5)
# M1 <- rnorm(200, mean = D.ex, sd = .65)
# test <- data.frame(D = D.ex, D.str = c("Healthy", "Ill")[D.ex + 1], 
#                    M1 = M1, stringsAsFactors = FALSE)
# basicplot <- ggplot(test, aes(d = D, m = M1)) + geom_roc()
# basicplot
# calc_auc(basicplot)

# predSubsetVector was calculated above (for the other ROC package)
# AUCplot <- ggplot(AreaRecordTrainingPredictDF[predSubsetVector,],
#     aes(d=sign(EventsObserved), m=PredictProb)) + geom_roc()
# AUCplot
#  AUCDF<-calc_auc(AUCplot)
#  AUCDF
# print(round(AUCDF[1, "AUC"], 3))
# 
#  AUCVector[j]<-AUCDF[1, "AUC"]
# 
# AUCplotName<-paste0("AUCplot", ObjectNameSuffixVector[j])
# print(AUCplotName)
# AUCDFName  <-paste0("AUCDF"  , ObjectNameSuffixVector[j])
# print(AUCDFName)

# if(Sys.info()[['user']]=="eidenale"){
#    # Neal
#    FolderName<-"C:/Users/eidenale/Dropbox/COVID_cities/CC_Intermediate_data_obj/"
#    # save(AUCplot, file = "C:/Users/eidenale/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCplot.rdata") 
#    # save(AUCDF  , file = "C:/Users/eidenale/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCDF.rdata") 
# }
# if(Sys.info()[['user']]=="phpupmee"){
#    # Paul
#    FolderName<-"C:/CADDE_dropbox/Dropbox/COVID_cities/CC_Intermediate_data_obj/"
#    # save(AUCplot, file = "C:/CADDE_dropbox/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCplot.rdata") 
#    # save(AUCDF  , file = "C:/CADDE_dropbox/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCDF.rdata") 
# }
# if(Sys.info()[['user']]=="eideobra"){
#    # Oli
#    FolderName<-"/Users/eideobra/Dropbox/COVID_cities/CC_Intermediate_data_obj/"
#    # save(AUCplot, file = "/Users/eideobra/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCplot.rdata") 
#    # save(AUCDF  , file = "/Users/eideobra/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCDF.rdata") 
# }

# save(AUCplot, file = paste0(FolderName, AUCplotName, ".rdata")) 
# save(AUCDF  , file = paste0(FolderName, AUCDFName  , ".rdata")) 
# write.csv(x=data.frame(AUC=AUCVector, LastDay=LastDaySubsetVector), 
#           file=paste0(FolderName, "AUCbyTime.csv"), row.names = FALSE)

return(PredictDF=AreaRecordTrainingPredictDF[predSubsetVector,])


}




