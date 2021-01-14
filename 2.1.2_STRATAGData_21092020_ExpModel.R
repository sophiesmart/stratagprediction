
# Add our local library to the libpath
.libPaths( c( Sys.getenv("R_LIBS_mpmss1") ) )

# Load libraries 
library(utils)
library(dplyr)
library(mice)

##############################################################################
# Read in data
##############################################################################

setwd("/nfshome/store02/users/c.mpmss1/STRATAG")
load("/nfshome/store02/users/c.mpmss1/STRATAG/STRATAGData_21092020_DataPrepped.RData")

##############################################################################
# Impute missing data using MICE
##############################################################################
#look at the website: http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html
imp100 = mice::mice(data_2_impute_exp, m=100, printFlag=TRUE, maxit = 10,seed=1000)

##############################################################################
# Save workspace
##############################################################################
save.image("/nfshome/store02/users/c.mpmss1/STRATAG/STRATAGData_21092020_ExplantoryModel_ImputeData.RData")
