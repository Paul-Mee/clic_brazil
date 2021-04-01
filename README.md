# clic_brazil
Repository of files and data for the CLIC Brazil app and analyses
This contains the the source code for the batch processes run on a daily basis to update the data in the CLIC Brazil app 
and to carry out analyses  to identify factors associated with the time to arrival and initial intensity of the COVID-19 epidemic 
in different municipalities

There are two sets of scripts included in the repository . 
Set 1) Are the scripts required for downloading the source COVID-19 case and deaths data 
and processing it to produce the outputs presented in the app 

**CLIC_Brazil_Script_directories.R**  - This is used to hardcode the directory paths and will need to be edited to reflect your local
environment, You will need to edit the reference to this which is the header for each R script


CLIC_Brazil_Data_update_batch.R 
CLIC_Brazil_Dataload.R
CLIC_Brazil_Data_cleaning.R
CLIC_Brazil_standardisation_implementation.R
CLIC_Brazil_standardisation_functions.R

CLIC_Brazil_calculate_Rt_Estimates.R


CLIC_Brazil_multivar_functions.R
CLIC_Brazil_peak_prediction.R
CLIC_Brazil_regression_data_prep.R
CLIC_Brazil_Rt_regression_analysis.R
CLIC_Brazil_Script_directories.R
CLIC_Brazil_standardisation_functions.R
CLIC_Brazil_standardisation_implementation.R
CLIC_Brazil_threshold_Tobit_analysis.R
case_age_dist

covariates
epifilter
ibge
interventions
municipalities
pop_age_dist
