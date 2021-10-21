#  from "AUC plot.R":

   PredictDF<-AUCfn(
      FolderName="C:\\Users\\eidenale\\Dropbox\\COVID_cities\\CC_Intermediate_data_obj_archive", 
      dir_script=dir_scripts, 
      dir_data=dir_data_objects, 
      dir_covar=dir_covariates,
      TestNE=F, verbose=T, 
      LatestDate=as.Date(DateSuffixVector[j], format="%Y_%m_%d")) 
   
# LatestDate: for the app, this can be today's date
# TestNE=F  : should stay as "F".  If "T" then it takes a smaller subset of the data, for quicker testing
# verbose=T : can set to F (if T then it just gives more output)



