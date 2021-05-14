

AUCfn("C:/github/data/intermediate_data_objects","2021_03_31")


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

if(Sys.info()[['user']]=="eidenale"){
  # Neal
  FolderName<-"C:/Users/eidenale/Dropbox/COVID_cities/CC_Intermediate_data_obj/"
  # save(AUCplot, file = "C:/Users/eidenale/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCplot.rdata") 
  # save(AUCDF  , file = "C:/Users/eidenale/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCDF.rdata") 
}
if(Sys.info()[['user']]=="phpupmee"){
  # Paul
  FolderName<-"C:/CADDE_dropbox/Dropbox/COVID_cities/CC_Intermediate_data_obj/"
  # save(AUCplot, file = "C:/CADDE_dropbox/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCplot.rdata") 
  # save(AUCDF  , file = "C:/CADDE_dropbox/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCDF.rdata") 
}
if(Sys.info()[['user']]=="eideobra"){
  # Oli
  FolderName<-"/Users/eideobra/Dropbox/COVID_cities/CC_Intermediate_data_obj/"
  # save(AUCplot, file = "/Users/eideobra/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCplot.rdata") 
  # save(AUCDF  , file = "/Users/eideobra/Dropbox/COVID_cities/CC_Intermediate_data_obj/AUCDF.rdata") 
}

save(AUCplot, file = paste0(FolderName, AUCplotName, ".rdata")) 
save(AUCDF  , file = paste0(FolderName, AUCDFName  , ".rdata")) 
write.csv(x=data.frame(AUC=AUCVector, LastDay=LastDaySubsetVector), 
          file=paste0(FolderName, "AUCbyTime.csv"), row.names = FALSE)
