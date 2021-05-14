######################################################################
# Runs Peak estimation code
######################################################################


rm(list=ls())

##############
### Directory set up
### Update this with your local directories
##############
dir_scripts <- "C:/github/clic_brazil/"

source (paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads function to be used for peak prediction
source(paste0(dir_scripts,"CLIC_Brazil_peak_pred_function.R"))



### Calle AUCfn 
## arg1 = directory where Big Standard file is stored
####

AreaRecordTrainingPredictDF <- AUCfn(dir_data_objects)

#### Output

# # predSubsetVector was calculated above (for the other ROC package)
AUCplot <- ggplot(AreaRecordTrainingPredictDF[predSubsetVector,],
                  aes(d=sign(EventsObserved), m=PredictProb)) + geom_roc()
AUCplot
AUCDF<-calc_auc(AUCplot)
AUCDF
round(AUCDF[1, "AUC"], 3)

AUCVector[j]<-AUCDF[1, "AUC"]

AUCplotName<-paste0("AUCplot", ObjectNameSuffixVector[j])
print(AUCplotName)
AUCDFName  <-paste0("AUCDF"  , ObjectNameSuffixVector[j])
print(AUCDFName)



save(AUCplot, file = paste0(dir_peak_data, AUCplotName, ".rdata")) 
save(AUCDF  , file = paste0(dir_peak_data, AUCDFName  , ".rdata")) 
write.csv(x=data.frame(AUC=AUCVector, LastDay=LastDaySubsetVector), 
          file=paste0(dir_peak_data, "AUCbyTime.csv"), row.names = FALSE)
