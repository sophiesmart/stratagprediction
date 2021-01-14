# stratagprediction
STRATA-G Clinical Prediction Model for Treatment Resistant Psychosis

This repository contains 12 scripts written by Dr Deborah Agbedjro and Dr Sophie Smart.
Dr Smart would like to acknowledge the support of the Supercomputing Wales project, which is part-funded by the European Regional Development Fund (ERDF) via Welsh Government and Ellis Pires at Cardiff University.
All code was accurate at the time of performing the analysis.

1_STRATAGData_21092020_DataPrep.R
Script for data cleaning: formatting variables, removing ones which don't meet inclusion criteria, selecting STRATA-G cohorts for analysis

2.1.1_STRATAGData_21092020_ExpModel.R
Script to load R packages onto Supercomputing Wales necessary for imputing missing data using MICE 

2.1.2_STRATAGData_21092020_ExpModel.R
Script to impute missing data using MICE

2.1.3_STRATAGData_21092020_ExpModel.sh
Script to run 2.1.2_STRATAGData_21092020_ExpModel.R on Supercomputing Wales

2.2_STRATAGData_21092020_ExpModel.R
Script for the STRATA-G explanatory model of treatment resistant psychosis

3.1.1_STRATAGData_21092020_PredModel.R
Script for the STRATA-G prediction model of treatment resistant psychosis

3.1.2_STRATAGData_21092020_PredModel.R
Script to perform cross-validation across cohorts of the STRATA-G prediction model

3.1.3_STRATAGData_21092020_PredModel.sh
Script to run 3.1.2_STRATAGData_21092020_PredModel.R on Supercomputing Wales

3.2.1_STRATAGData_21092020_PredModel.R
Script to load R packages onto Supercomputing Walesnecessary for cross-validation across cohorts and optimism-correction 

3.2.2_STRATAGData_21092020_PredModel.R
Script to perform repeated cross validation optimism correction of the STRATA-G prediction model

3.2.3_STRATAGData_21092020_PredModel.sh
Script to run 3.2.2_STRATAGData_21092020_PredModel.R on Supercomputing Wales

3.3.1_STRATAGData_21092020_PredModel.R
Script to produce a nomogram plot of the STRATA-G prediction model
