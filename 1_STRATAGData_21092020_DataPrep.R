##########################################################################################
##########################################################################################
# STRATA-G PROTOCOL ######################################################################
##########################################################################################
##########################################################################################
# DATA PREPARATION & DESCRIPTION #########################################################
##########################################################################################
##########################################################################################
# 21st September 2020 ####################################################################
##########################################################################################
##########################################################################################
# Deborah Agbedjro: deborah.1.agbedjro@kcl.ac.uk
# Sophie Smart: SmartS1@cardiff.ac.uk
##########################################################################################
##########################################################################################

RStudioVersion <- RStudio.Version()

RStudioVersion$version
R.version.string

#install.packages("caret")
library(caret)
#install.packages("MatrixModels")
library(MatrixModels)
#install.packages("missForest")
library(missForest) #does not impute out-of-range values
#install.packages("doParallel")
library(doParallel)
#install.packages("glmnet")
library(glmnet)
#install.packages("e1071") # for lasso dealing with named levels of outcome
library(e1071)
#install.packages("pROC") # to compute the AUC
library(pROC)
#install.packages("gmodels")
library(gmodels) # for CrossTable
#install.packages("haven")
library(haven) # to read in .dta data
#install.packages("psych")
library(psych) # to check correlations between categorical variables
#install.packages("VIM")
library(VIM)
library(mice) #to use MICE
library(boot) #for diagnostics
library(dplyr) #for %>% command
library(tidyr) #for gather()
library(ggplot2)
library(broom) #for augment()
library(psych)
library(reshape2)
#install.packages("tableone") #for including both cat and cont variables in a stratified table
library(tableone)
library(Hmisc) #for correlations

options("scipen"=100, "digits"=4)
setwd("/Volumes/Imation USB/Oct2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/DataPrep")

##############################################################################
# Read in data
##############################################################################
dat <- haven::read_dta("/Volumes/Imation USB/Oct2020/STRATAG/stratamasterdatabase_21092020.dta")
subset <- as.data.frame(dat[c("idstrata","cohortstrata","strataclozeverbinary","strataeitherever","lengthfollowupyears","rf_baseline","rf_onset","rf_dup","rf_gender","rf_histsz","rf_histmh","rf_bmi","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_educationyears","rf_educationqual","rf_cannabis","rf_tobacco","rf_alcohol","rf_pansspos","rf_panssneg","rf_panssgen","rf_pansstotal","rf_saps","rf_sans","rf_bprstotal","rf_gaf","rf_ethnicity02","diagnosisFUstrata02")])
# change categorical variables to factors using value labels from STATA database

subset[,c("cohortstrata","strataclozeverbinary","strataeitherever","rf_gender","rf_histsz","rf_histmh","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_educationqual","rf_cannabis","rf_tobacco","rf_alcohol","rf_ethnicity02","diagnosisFUstrata02")] <- as_factor(subset[,c("cohortstrata","strataclozeverbinary","strataeitherever","rf_gender","rf_histsz","rf_histmh","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_educationqual","rf_cannabis","rf_tobacco","rf_alcohol","rf_ethnicity02","diagnosisFUstrata02")])

##############################################################################
# Data cleaning - participants part I
##############################################################################
# drop individuals where TR status is unknown
data<-subset[complete.cases(subset$strataeitherever),]

##############################################################################
# Data cleaning - variables part I
##############################################################################
# change reference level of categorical variables (the reference category needs to be the one with more patients for each categorical/binary variable to have stable OR estimates. in R the reference category by default is the first one listed in the levels() command)
summary(data$strataclozeverbinary) 
# Reference = No Cloz    

summary(data$strataeitherever) 
# Reference = Non-TR

#levels(data$rf_gender) #"Female" is reference
summary(data$rf_gender) # but "Female" has less people than "Male"
data$rf_gender<-factor(data$rf_gender, levels=c("Male","Female")) # this command reorders the levels
#summary(data$rf_gender)
# Reference = Male     

summary(data$rf_relationshipcurr) 
# Reference = Single

summary(data$rf_relationshipltime) 
# Reference = Single

summary(data$rf_livingcurr) 
data$rf_livingcurr<-factor(data$rf_livingcurr, levels=c("With Family","Alone","With Others")) 
# Reference = With Family

summary(data$rf_employment) 
# Reference = Unemployed

summary(data$rf_cannabis) 
# Reference = No

summary(data$rf_tobacco) 
data$rf_tobacco<-factor(data$rf_tobacco, levels=c("Yes","No"))
# Reference = Yes

summary(data$rf_alcohol) 
data$rf_alcohol<-factor(data$rf_alcohol, levels=c("Yes","No"))
# Reference = Yes

summary(data$rf_educationqual) 
data$rf_educationqual<-factor(data$rf_educationqual, levels=c("Further","None","Basic","Higher"))
# Reference = Further

summary(data$rf_ethnicity02) 
# Reference = European

summary(data$cohortstrata) 
data$cohortstrata <- droplevels(data$cohortstrata)
data$cohortstrata<-factor(data$cohortstrata, levels=c("Santander","AESOP London","Belfast","Bologna","GAP London","Istanbul","Lausanne","Oslo","Paris","Prague","Sao Paulo","UCL London"))

##############################################################################
# Data Description (for supplementary material)
##############################################################################
# Summary Statistics #######

# TR by cohort
table(data$strataeitherever)
table1 <- table(data$cohortstrata, data$strataeitherever)
write.table(table1, "sTable1.TRvsNTR.txt", sep="\t")

table2 <- table(data$cohortstrata, data$strataclozeverbinary)
write.table(table2, "sTable2.ClozvsNonCloz.txt", sep="\t")

# Variable by cohort and TR
factorVars <- c("cohortstrata","strataclozeverbinary","strataeitherever","rf_gender","rf_histsz","rf_histmh","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_educationqual","rf_cannabis","rf_tobacco","rf_alcohol","rf_ethnicity02","diagnosisFUstrata02")

vars <- c("cohortstrata","strataclozeverbinary","strataeitherever","lengthfollowupyears","rf_baseline","rf_onset","rf_dup","rf_gender","rf_histsz","rf_histmh","rf_bmi","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_educationyears","rf_educationqual","rf_cannabis","rf_tobacco","rf_alcohol","rf_pansspos","rf_panssneg","rf_panssgen","rf_pansstotal","rf_saps","rf_sans","rf_bprstotal","rf_gaf","rf_ethnicity02","diagnosisFUstrata02")

table <- CreateTableOne(data=data, strata="strataeitherever", vars=vars, factorVars=factorVars, includeNA=FALSE, test=FALSE, addOverall=TRUE)
tableallvars <- print(table, pDigits=8)
write.table(tableallvars, "sTable3.PredictorsbyTR.txt", sep="\t")

#missing data
aggr_plot <- aggr(data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

tablemissing <- print(as.matrix((apply(apply(data,2,is.na),2,sum)*100/2449)[apply(apply(data,2,is.na),2,sum)*100/2449>=00]))
write.table(tablemissing, "sTable4.MissingData.txt", sep="\t")

tablemissing2 <- apply(data,2,function(y) tapply( y  , data$cohortstrata , function(x) sum(is.na(x)))/summary(data$cohortstrata)*100)
tablemissing2 <- t(tablemissing2)
write.table(tablemissing2, "sTable4.MissingDatabyCohort.txt", sep="\t")

#correlations
#corrpearson <- cor(data[,c("rf_baseline","rf_onset","rf_dup","rf_bmi","rf_educationyears","rf_pansspos","rf_panssneg","rf_panssgen","rf_pansstotal","rf_saps","rf_sans","rf_bprstotal","rf_gaf")],use="pairwise.complete.obs") # only numeric (continuous) variables
#write.table(corrpearson, "sTable5.PearsonCorrelations.txt", sep="\t")

datacorr1 <- data[,c("rf_gender","rf_histsz","rf_histmh","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_cannabis","rf_tobacco","rf_alcohol","rf_educationqual","rf_ethnicity02")]
datacorr1 <- as.data.frame(lapply(datacorr1, as.integer))

datacorr2 <- data[,c("rf_baseline","rf_onset","rf_dup","rf_bmi","rf_educationyears","rf_pansspos","rf_panssneg","rf_panssgen","rf_pansstotal","rf_saps","rf_sans","rf_bprstotal","rf_gaf")]
datacorr2 <- as.data.frame(lapply(datacorr2, as.integer))

datacorr <- cbind(datacorr1,datacorr2)

matriz <- rcorr(as.matrix(datacorr), type=c("pearson"))
matriz$r
write.table(matriz$r, "sTable5.PearsonCorrelations.txt", sep="\t")

aov(data$rf_educationyears ~ data$rf_educationqual)
summary(aov(data$rf_educationyears ~ data$rf_educationqual))
TukeyHSD(aov(data$rf_educationyears ~ data$rf_educationqual))

##############################################################################
# Data cleaning - participants part II
##############################################################################
# remove cohorts where TR N < 5 AND TR % < 5

data_2_impute <- data[data$cohortstrata!="Sao Paulo",]
data_2_impute <- data_2_impute[data_2_impute$cohortstrata!="Oslo",]
data_2_impute$cohortstrata <- droplevels(data_2_impute$cohortstrata)

##############################################################################
# Data cleaning - variables part II
##############################################################################
# remove variables with +80% missingness
summary(data_2_impute$strataeitherever)
summary(data_2_impute$cohortstrata)
table(data_2_impute$cohortstrata, data_2_impute$strataeitherever)
tablemissing <- print(as.matrix((apply(apply(data_2_impute,2,is.na),2,sum)*100/2216)[apply(apply(data_2_impute,2,is.na),2,sum)*100/2216>=79]))
data_2_impute <- subset(data_2_impute, select = -c(rf_histsz,rf_histmh,rf_accommodation,rf_panssneg,rf_panssgen,rf_pansstotal) )


# remove variables correlated >0.8 (check for multicollinearity)
print(matriz$r>0.8)

data_2_imputecorr1 <- data[,c("rf_gender","rf_histsz","rf_histmh","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_accommodation","rf_employment","rf_cannabis","rf_tobacco","rf_alcohol","rf_educationqual","rf_ethnicity02")]
data_2_imputecorr1 <- as.data.frame(lapply(data_2_imputecorr1, as.integer))

data_2_imputecorr2 <- data[,c("rf_baseline","rf_onset","rf_dup","rf_bmi","rf_educationyears","rf_pansspos","rf_panssneg","rf_panssgen","rf_pansstotal","rf_saps","rf_sans","rf_bprstotal","rf_gaf")]
data_2_imputecorr2 <- as.data.frame(lapply(data_2_imputecorr2, as.integer))

data_2_imputecorr <- cbind(data_2_imputecorr1,data_2_imputecorr2)

matrizdata_2_impute <- rcorr(as.matrix(data_2_imputecorr), type=c("pearson"))
matrizdata_2_impute$r
print(matrizdata_2_impute$r>0.8)

#rf_baseline & rf_onset: 0.9554 so remove rf_baseline. Also remove rf_relationshipltime and rf_educationqual.
data_2_impute <- subset(data_2_impute, select = -c(rf_baseline, rf_relationshipltime, rf_educationqual) )

# check reference level of categorical variables
summary(data_2_impute$rf_gender)
# Reference = Male     
summary(data_2_impute$rf_relationshipcurr) 
# Reference = Single
summary(data_2_impute$rf_livingcurr) 
# Reference = With Family
summary(data_2_impute$rf_employment) 
# Reference = Unemployed
summary(data_2_impute$rf_cannabis) 
# Reference = No
summary(data_2_impute$rf_tobacco) 
# Reference = Yes
summary(data_2_impute$rf_alcohol) 
# Reference = Yes
summary(data_2_impute$rf_ethnicity02) 
# Reference = European
summary(data_2_impute$cohortstrata) 
# Reference = Santander


# remove variable without at least 10 events or non events per category
table(data_2_impute$rf_gender,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_relationshipcurr,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_livingcurr,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_employment,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_cannabis,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_tobacco,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_alcohol,data_2_impute$strataeitherever) # fine
table(data_2_impute$rf_ethnicity02,data_2_impute$strataeitherever) # fine


##############################################################################
# Data Description (for main text)
##############################################################################
# Summary Statistics #######

# TR by cohort
table(data_2_impute$strataeitherever)
table3 <- table(data_2_impute$cohortstrata, data_2_impute$strataeitherever)
write.table(table3, "sTable6.TRvsNTR.txt", sep="\t")

table4 <- table(data_2_impute$cohortstrata, data_2_impute$strataclozeverbinary)
write.table(table4, "sTable7.ClozvsNonCloz.txt", sep="\t")

# Variable by cohort and TR
factorVars2 <- c("cohortstrata","strataclozeverbinary","strataeitherever","rf_gender","rf_relationshipcurr","rf_livingcurr","rf_employment","rf_cannabis","rf_tobacco","rf_alcohol","rf_ethnicity02","diagnosisFUstrata02")

vars2 <- c("cohortstrata","strataclozeverbinary","strataeitherever","lengthfollowupyears","rf_onset","rf_dup","rf_gender","rf_bmi","rf_relationshipcurr","rf_relationshipltime","rf_livingcurr","rf_employment","rf_educationyears","rf_educationqual","rf_cannabis","rf_tobacco","rf_alcohol","rf_pansspos","rf_saps","rf_sans","rf_bprstotal","rf_gaf","rf_ethnicity02","diagnosisFUstrata02")

tableall2 <- CreateTableOne(data=data_2_impute, strata="strataeitherever", vars=vars2, factorVars=factorVars2, includeNA=FALSE, test=FALSE, addOverall=TRUE)
tableallvars2 <- print(tableall2, pDigits=8)
write.table(tableallvars2, "sTable8.PredictorsbyTR.txt", sep="\t")

#missing data
aggr_plot2 <- aggr(data_2_impute, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data_2_impute), cex.axis=.7, gap=3, ylab=c("Histogram of missing data 2","Pattern"))

tablemissing2 <- print(as.matrix((apply(apply(data_2_impute,2,is.na),2,sum)*100/2216)[apply(apply(data_2_impute,2,is.na),2,sum)*100/2216>=00]))
write.table(tablemissing2, "tablemissing2.txt", sep="\t")

##############################################################################
# Datafiles for analysis
##############################################################################

names(data_2_impute)

#explanatory model
data_2_impute_exp <- subset(data_2_impute, select = -c(idstrata,strataclozeverbinary,diagnosisFUstrata02) )
data_2_impute_exp <- data_2_impute_exp[,c("strataeitherever",setdiff(names(data_2_impute_exp),"strataeitherever"))]
write.csv(data_2_impute_exp,'data_2_impute_exp.csv')

#prediction model - original
data_2_impute_pred <- subset(data_2_impute, select = -c(idstrata,cohortstrata,strataclozeverbinary,diagnosisFUstrata02,lengthfollowupyears) )
write.csv(data_2_impute_pred,'data_2_impute_pred.csv')

#prediction model - clozapine outcome only
data_2_impute_predCLOZ <- subset(data_2_impute, select = -c(idstrata,cohortstrata,strataeitherever,diagnosisFUstrata02,lengthfollowupyears) )
data_2_impute_predCLOZ <- data_2_impute_predCLOZ[complete.cases(data_2_impute_predCLOZ$strataclozeverbinary),]
write.csv(data_2_impute_predCLOZ,'data_2_impute_predCLOZ.csv')

#prediction model - schizophrenia cases only
data_2_impute_predSZ <- data_2_impute[data_2_impute$diagnosisFUstrata02=="SZ",]
data_2_impute_predSZ <- subset(data_2_impute_predSZ, select = -c(idstrata,cohortstrata,strataclozeverbinary,diagnosisFUstrata02,lengthfollowupyears) )
write.csv(data_2_impute_predSZ,'data_2_impute_predSZ.csv')

##############################################################################
# Save workspace
##############################################################################

rm(factorVars, factorVars2, table1, table2, table3, table4, vars, vars2)
rm(corrpearson, dat, data, subset, table, tableall2, tableallvars, tableallvars2, tablemissing, tablemissing2)
rm(matriz, matrizdata_2_impute, RStudioVersion, datacorr, datacorr1, datacorr2, data_2_imputecorr, data_2_imputecorr1, data_2_imputecorr2, aggr_plot, aggr_plot2)
save.image("STRATAGData_21092020_DataPrepped.RData")

##############################################################################
##############################################################################
##############################################################################
# END OF SCRIPT ##############################################################
##############################################################################
##############################################################################
##############################################################################