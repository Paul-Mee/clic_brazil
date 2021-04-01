# Covid-19 Local Information Comparison web application (CLIC Brazil) 

This repository contains the the source code for the batch processes run on a daily basis to update the data in the CLIC Brazil app 
(https://cmmid.github.io/visualisations/lacpt) and to carry out analyses  to identify factors associated with the time to arrival and initial intensity of the COVID-19 epidemic in different municipalities

There are two sets of scripts included in the repository . 

Set 1) Are the scripts required for downloading the source COVID-19 case and deaths data 
and processing it to produce the outputs presented in the app 

**CLIC_Brazil_Script_directories.R**  - This is used to hardcode the directory paths and will need to be edited to reflect your local
environment, You will need to edit the reference to this which is the header for each R script

**CLIC_Brazil_Data_update_batch.R** - This calls the 6 scripts used in the data processing and analysis pipeline. It can be run interactively or via a bash script or similar 

**CLIC_Brazil_Dataload.R** - Used to download the data from the brazil.IO repository and format it for later steps in the pipeline

**CLIC_Brazil_Data_cleaning.R** - Used for data cleaning to ensure consistency of place names etc. from the various data sources

**CLIC_Brazil_standardisation_implementation.R** - Carries out the 3rd administrative level (municipality) age/population standardisation to allow meaningful comparisons to be made between places

**CLIC_Brazil_standardisation_functions.R** - Functions called by the standardisation script and other scripts

**CLIC_Brazil_calculate_Rt_Estimates.R** - Script which uses the Epifilter package to calculate Rt estimates for each municipality 

**CLIC_Brazil_peak_prediction.R** - Analytical routine which predicts for each municipality the probability that a new peak incidence will be reached in the next 30 days based on preceding incidence data 

Set 2) Are analytical scripts which use the data derived in the daily batch process

**CLIC_Brazil_multivar_functions.R** - Functions written to automate some steps in the development of tabular outputs from univariable and multivariable linear regression analyses

**CLIC_Brazil_regression_data_prep.R** - Script to prepare data required in the Tobit regression and Rt regression analyses - this must be run prior to these scripts

**CLIC_Brazil_threshold_Tobit_analysis.R** - Script to carry out Tobit regression analyses to identify factors associated with the time taken for the COVID-19 epidemic to become established in a particular municipality from the date it was first identified in Brazil

**CLIC_Brazil_Rt_regression_analysis.R** - Script to carry out linear regression analyses to identify factors associated with intensity of the initial epidemic - assesses as the Mean Rt over a fixed time window 

**The following directories contain source data required by the scripts **

case_age_dist - age/case distribution data for COVID 19 from the ealry stages of the epidemic 

pop_age_dist - distributions of age/population for each municipality 

epifilter - sub-directory of scripts for the epifilter package 

ibge - data on the socio-demogrpahic characteristics of municipalities across Brazil drawn from the 2010 census

interventions - data on interventions collate by the CEPALobservatory and edited by Andreza de Souza Santos ( (http://www.nature.com/articles/s41597-021-00859-1  https://www.cnm.org.br/biblioteca/exibe/14729 )

municipalities - reference data on municipalities and micro regions within Brazil


