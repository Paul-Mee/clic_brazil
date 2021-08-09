######################################################################
# Runs Peak estimation code
######################################################################


##############
### Directory set up
### Update this with your local directory
##############
dir_scripts <- "C:/github/clic_brazil/"

if(Sys.info()[['user']]=="eidenale"){
   dir_scripts <- "C:/Users/eidenale/Documents/clic_brazil/"
}

source(paste0(dir_scripts,"CLIC_Brazil_Script_directories.R"))

# loads function to be used for peak prediction
source(paste0(dir_scripts,"CLIC_Brazil_peak_pred_function.R"))

### Calle AUCfn 
## arg1 = directory where Big Standard file is stored
####

# AreaRecordTrainingPredictDF <- AUCfn(dir_data_objects)
TestingScalar<-TRUE # whether to use the testing option (subset of data in NE region)
AreaRecordTrainingPredictDF <- AUCfn(dir_data_objects, TestNE=TestingScalar, verbose=T)

# again with the whole dataset
AreaRecordTrainingPredictDF <- AUCfn(dir_data_objects, TestNE=F, verbose=T)

#### Output

predSubsetVector<-!is.na(AreaRecordTrainingPredictDF$PredictProb) & !is.na(AreaRecordTrainingPredictDF$EventsObserved)
pred <- prediction(
  predictions=     AreaRecordTrainingPredictDF$PredictProb[predSubsetVector], 
  labels=sign(AreaRecordTrainingPredictDF$EventsObserved[predSubsetVector]))
perf <- performance(pred,"tpr","fpr")

# # predSubsetVector was calculated above (for the other ROC package)
AUCplot <- ggplot(AreaRecordTrainingPredictDF[predSubsetVector,],
                  aes(d=sign(EventsObserved), m=PredictProb)) + geom_roc()
AUCplot
AUCDF<-calc_auc(AUCplot)
AUCDF
round(AUCDF[1, "AUC"], 3)

#AUCVector[j]<-AUCDF[1, "AUC"]
AUCVector<-AUCDF[1, "AUC"]

AUCplotName<-paste0("AUCplot")
print(AUCplotName)
AUCDFName  <-paste0("AUCDF")
print(AUCDFName)


save(AUCDF  , file = paste0(dir_peak_data, AUCDFName  , ".rdata")) 
 # write.csv(x=data.frame(AUC=AUCVector, LastDay=LastDaySubsetVector), 
 #          file=paste0(dir_peak_data, "AUCbyTime.csv"), row.names = FALSE)
