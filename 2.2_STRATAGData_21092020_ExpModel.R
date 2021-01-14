##########################################################################################
##########################################################################################
# STRATA-G PROTOCOL ######################################################################
##########################################################################################
##########################################################################################
# EXPLANATORY MODEL ######################################################################
##########################################################################################
##########################################################################################
# 21st September 2020 ####################################################################
##########################################################################################
##########################################################################################
# Deborah Agbedjro: deborah.1.agbedjro@kcl.ac.uk
# Sophie Smart: SmartS1@cardiff.ac.uk
##########################################################################################
##########################################################################################

library(mice) 
library(VIM)
library(dplyr)
library(tidyr)
library(broom)
library(boot)
library(sandwich)
library(psfmi)


options("scipen"=100, "digits"=4)
setwd("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/ExpModel")


##############################################################################
# Read in data
##############################################################################
#data_2_impute_exp <- as.data.frame(read.csv("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/DataPrep/data_2_impute_exp.csv"))

#load("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/DataPrep/STRATAGData_21092020_DataPrepped.RData")

##############################################################################
# Impute missing data using MICE
##############################################################################
#look at the website: http://web.maths.unsw.edu.au/~dwarton/missingDataLab.html

#Run on Hawk. We acknowledge the support of the Supercomputing Wales project, which is part-funded by the European Regional Development Fund (ERDF) via Welsh Government.

#imp100 = mice(data_2_impute_exp, m=100, printFlag=TRUE, maxit = 10,seed=1000)

load("STRATAGData_21092020_ExplantoryModel_ImputeData.RData")

##############################################################################
# Check missing data imputation
##############################################################################
#Check missing data patterns
md.pattern(data_2_impute_exp)
aggr_plot <- aggr(data_2_impute_exp, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data_2_impute_exp), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
marginplot(data_2_impute_exp[c("rf_onset","rf_dup")])

#Summary of imputed data
summary(imp100)

#Check logged events
imp100$loggedEvents

#Check for out of range values in continous variables
names(data_2_impute_exp)

summary(data_2_impute_exp$rf_onset)  #range 7.45 - 63.00
summary(imp100$imp$rf_onset) #in range
summary(data_2_impute_exp$rf_dup)  #range 0 - 6993
summary(imp100$imp$rf_dup) #in range
summary(data_2_impute_exp$rf_bmi)  #range 14.6 - 54.3
summary(imp100$imp$rf_bmi) #max 54.4
summary(data_2_impute_exp$rf_educationyears)  #range 0 - 24.0
summary(imp100$imp$rf_educationyears) #in range
summary(data_2_impute_exp$rf_pansspos)  #possible range 7 - 49
summary(imp100$imp$rf_pansspos) #in range
summary(data_2_impute_exp$rf_saps)  #range 0 - 20
summary(imp100$imp$rf_saps) #in range
summary(data_2_impute_exp$rf_sans)  #range 0 - 25.8
summary(imp100$imp$rf_sans) #max 25.81 
summary(data_2_impute_exp$rf_bprstotal)  #possible range 24 - 168
summary(imp100$imp$rf_bprstotal) #in range
summary(data_2_impute_exp$rf_gaf)  #range 1 - 100
summary(imp100$imp$rf_gaf) #in range

#Visualise missing data imputation
pdf("densityplot_21092020.pdf") 
densityplot(imp100)
dev.off()

pdf("stripplot_21092020.pdf") 
stripplot(imp100, pch = 20, cex = 1.2)
dev.off()

#Checking single logistic regressions after imputation for the first five imputed datasets

#The plot on the top left is a plot of the jackknife deviance residuals against the fitted values.

#The plot on the top right is a normal QQ plot of the standardized deviance residuals. The dotted 
#line is the expected line if the standardized residuals are normally distributed, i.e. it is the line 
#with intercept 0 and slope 1.

#The bottom two panels are plots of the Cook statistics. On the left is a plot of the Cook statistics 
#against the standardized leverages. In general there will be two dotted lines on this plot. The horizontal line
#is at 8/(n-2p) where n is the number of observations and p is the number of parameters estimated. Points above
#this line may be points with high influence on the model. The vertical line is at 2p/(n-2p) and points to the
#right of this line have high leverage compared to the variance of the raw residual at that point. If all points
#are below the horizontal line or to the left of the vertical line then the line is not shown.

#The final plot again shows the Cook statistic this time plotted against case number enabling us to find which
#observations are influential.

#First imputed dataset
data_imputed<-data_2_impute_exp
data_imputed$lengthfollowupyears[is.na(data_2_impute_exp$lengthfollowupyears)]<-imp100$imp$lengthfollowupyears[,1]
data_imputed$rf_onset[is.na(data_2_impute_exp$rf_onset)]<-imp100$imp$rf_onset[,1]
data_imputed$rf_dup[is.na(data_2_impute_exp$rf_dup)]<-imp100$imp$rf_dup[,1]
data_imputed$rf_gender[is.na(data_2_impute_exp$rf_gender)]<-imp100$imp$rf_gender[,1]
data_imputed$rf_bmi[is.na(data_2_impute_exp$rf_bmi)]<-imp100$imp$rf_bmi[,1]
data_imputed$rf_relationshipcurr[is.na(data_2_impute_exp$rf_relationshipcurr)]<-imp100$imp$rf_relationshipcurr[,1]
data_imputed$rf_livingcurr[is.na(data_2_impute_exp$rf_livingcurr)]<-imp100$imp$rf_livingcurr[,1]
data_imputed$rf_employment[is.na(data_2_impute_exp$rf_employment)]<-imp100$imp$rf_employment[,1]
data_imputed$rf_educationyears[is.na(data_2_impute_exp$rf_educationyears)]<-imp100$imp$rf_educationyears[,1]
data_imputed$rf_cannabis[is.na(data_2_impute_exp$rf_cannabis)]<-imp100$imp$rf_cannabis[,1]
data_imputed$rf_tobacco[is.na(data_2_impute_exp$rf_tobacco)]<-imp100$imp$rf_tobacco[,1]
data_imputed$rf_alcohol[is.na(data_2_impute_exp$rf_alcohol)]<-imp100$imp$rf_alcohol[,1]
data_imputed$rf_pansspos[is.na(data_2_impute_exp$rf_pansspos)]<-imp100$imp$rf_pansspos[,1]
data_imputed$rf_saps[is.na(data_2_impute_exp$rf_saps)]<-imp100$imp$rf_saps[,1]
data_imputed$rf_sans[is.na(data_2_impute_exp$rf_sans)]<-imp100$imp$rf_sans[,1]
data_imputed$rf_bprstotal[is.na(data_2_impute_exp$rf_bprstotal)]<-imp100$imp$rf_bprstotal[,1]
data_imputed$rf_gaf[is.na(data_2_impute_exp$rf_gaf)]<-imp100$imp$rf_gaf[,1]
data_imputed$rf_ethnicity02[is.na(data_2_impute_exp$rf_ethnicity02)]<-imp100$imp$rf_ethnicity02[,1]

imp1<-data.frame(data_imputed$strataeitherever,data_imputed$lengthfollowupyears,data_imputed$cohortstrata,
                 data_imputed$rf_onset,
                 data_imputed$rf_dup,data_imputed$rf_gender,data_imputed$rf_bmi,data_imputed$rf_relationshipcurr,
                 data_imputed$rf_livingcurr,
                 data_imputed$rf_employment,data_imputed$rf_educationyears,
                 data_imputed$rf_cannabis,data_imputed$rf_tobacco,data_imputed$rf_alcohol,
                 data_imputed$rf_pansspos,data_imputed$rf_saps,data_imputed$rf_sans,data_imputed$rf_bprstotal,
                 data_imputed$rf_gaf,data_imputed$rf_ethnicity02)

exp1<-with(data=data_imputed, glm(strataeitherever~lengthfollowupyears + cohortstrata + rf_onset +
                                  rf_dup + rf_gender + rf_bmi + rf_relationshipcurr + rf_livingcurr + 
                                  rf_employment + rf_educationyears + 
                                  rf_cannabis + rf_tobacco + rf_alcohol + rf_pansspos + rf_saps + rf_sans + 
                                  rf_bprstotal + rf_gaf + rf_ethnicity02,
                                  family = binomial))

# Predict the probability (p) of TRS positivity
probabilities <- predict(exp1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "nonTRS", "TRS")
head(predicted.classes)
# Remove qualitative variables from the original data frame and bind the logit values to the data
# Select only numeric predictors
mydata1 <- data_imputed %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata1)
# Bind the logit and tidying the data for plot
mydata1 <- mydata1 %>%  
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)
#Create the scatter plots:
ggplot(mydata1, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
#Influential values
plot(exp1, which = 4, id.n = 3)
# Extract model results
model.data <- augment(exp1) %>% 
  mutate(index = 1:n()) 
# The data for the top 3 largest values, according to the Cook's distance, can be displayed as follow:
model.data %>% top_n(3, .cooksd)
#Plot the standardized residuals:
ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = strataeitherever), alpha = .5) +
  theme_bw()
# Filter potential influential data points with abs(.std.res) > 3:
model.data %>% 
  filter(abs(.std.resid) > 3) # ?? points which are influential - A tibble: 0 x 48
#  http://www.sthda.com/english/articles/36-classification-methods-essentials/148-logistic-regression-assumptions-and-diagnostics-in-r/
# Diagnostics plots
glm.diag.plots(exp1, glmdiag = glm.diag(exp1),
               iden = FALSE, labels = NULL, ret = FALSE)

#-----------------------------------------------------------------------------
# Calculate robust standard errors ####
#-----------------------------------------------------------------------------

cov.m1 <- vcovHC(exp1, type = "HC0")

std.err <- sqrt(diag(cov.m1))

q.val <- qnorm(0.975)

r.est <- cbind(
  Estimate = coef(exp1)
  , "Robust SE" = std.err
  , z = (coef(exp1)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(exp1)/std.err), lower.tail = FALSE)
  , LL = coef(exp1) - q.val  * std.err
  , UL = coef(exp1) + q.val  * std.err
)
#The model output using robust standard errors is:
r.est

write.table(r.est, "sTable9.MICEResiduals1.txt", sep="\t")
#The estimates and standard errors are fairly similar to those calculated using Stata but the intercept may be different

# Second imputed dataset
data_imputed2<-data_2_impute_exp
data_imputed2$lengthfollowupyears[is.na(data_2_impute_exp$lengthfollowupyears)]<-imp100$imp$lengthfollowupyears[,2]
data_imputed2$rf_onset[is.na(data_2_impute_exp$rf_onset)]<-imp100$imp$rf_onset[,2]
data_imputed2$rf_dup[is.na(data_2_impute_exp$rf_dup)]<-imp100$imp$rf_dup[,2]
data_imputed2$rf_gender[is.na(data_2_impute_exp$rf_gender)]<-imp100$imp$rf_gender[,2]
data_imputed2$rf_bmi[is.na(data_2_impute_exp$rf_bmi)]<-imp100$imp$rf_bmi[,2]
data_imputed2$rf_relationshipcurr[is.na(data_2_impute_exp$rf_relationshipcurr)]<-imp100$imp$rf_relationshipcurr[,2]
data_imputed2$rf_livingcurr[is.na(data_2_impute_exp$rf_livingcurr)]<-imp100$imp$rf_livingcurr[,2]
data_imputed2$rf_employment[is.na(data_2_impute_exp$rf_employment)]<-imp100$imp$rf_employment[,2]
data_imputed2$rf_educationyears[is.na(data_2_impute_exp$rf_educationyears)]<-imp100$imp$rf_educationyears[,2]
data_imputed2$rf_cannabis[is.na(data_2_impute_exp$rf_cannabis)]<-imp100$imp$rf_cannabis[,2]
data_imputed2$rf_tobacco[is.na(data_2_impute_exp$rf_tobacco)]<-imp100$imp$rf_tobacco[,2]
data_imputed2$rf_alcohol[is.na(data_2_impute_exp$rf_alcohol)]<-imp100$imp$rf_alcohol[,2]
data_imputed2$rf_pansspos[is.na(data_2_impute_exp$rf_pansspos)]<-imp100$imp$rf_pansspos[,2]
data_imputed2$rf_saps[is.na(data_2_impute_exp$rf_saps)]<-imp100$imp$rf_saps[,2]
data_imputed2$rf_sans[is.na(data_2_impute_exp$rf_sans)]<-imp100$imp$rf_sans[,2]
data_imputed2$rf_bprstotal[is.na(data_2_impute_exp$rf_bprstotal)]<-imp100$imp$rf_bprstotal[,2]
data_imputed2$rf_gaf[is.na(data_2_impute_exp$rf_gaf)]<-imp100$imp$rf_gaf[,2]
data_imputed2$rf_ethnicity02[is.na(data_2_impute_exp$rf_ethnicity02)]<-imp100$imp$rf_ethnicity02[,2]

imp2<-data.frame(data_imputed2$strataeitherever,data_imputed2$lengthfollowupyears,data_imputed2$cohortstrata,
                 data_imputed2$rf_onset,
                 data_imputed2$rf_dup,data_imputed2$rf_gender,data_imputed2$rf_bmi,data_imputed2$rf_relationshipcurr,
                 data_imputed2$rf_livingcurr,data_imputed2$rf_employment,data_imputed2$rf_educationyears,
                 data_imputed2$rf_cannabis,data_imputed2$rf_tobacco,data_imputed2$rf_alcohol,
                 data_imputed2$rf_pansspos,data_imputed2$rf_saps,data_imputed2$rf_sans,data_imputed2$rf_bprstotal,
                 data_imputed2$rf_gaf,data_imputed2$rf_ethnicity02)

exp2<-with(data=data_imputed2, glm(strataeitherever~lengthfollowupyears + cohortstrata + rf_onset +
                                     rf_dup + rf_gender + rf_bmi + rf_relationshipcurr + rf_livingcurr + 
                                     rf_employment + rf_educationyears + 
                                     rf_cannabis + rf_tobacco + rf_alcohol + rf_pansspos + rf_saps + rf_sans + 
                                     rf_bprstotal + rf_gaf + rf_ethnicity02,
                                   family = binomial))

probabilities <- predict(exp2, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "nonTRS", "TRS")
head(predicted.classes)

mydata2 <- data_imputed2 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata2)

mydata2 <- mydata2 %>%  
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata2, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(exp2, which = 4, id.n = 3)

model.data <- augment(exp2) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = strataeitherever), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3) 

glm.diag.plots(exp2, glmdiag = glm.diag(exp2),
               iden = FALSE, labels = NULL, ret = FALSE)

cov.m2 <- vcovHC(exp2, type = "HC0")

std.err <- sqrt(diag(cov.m2))

q.val <- qnorm(0.975)

r.est2 <- cbind(
  Estimate = coef(exp2)
  , "Robust SE" = std.err
  , z = (coef(exp2)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(exp2)/std.err), lower.tail = FALSE)
  , LL = coef(exp2) - q.val  * std.err
  , UL = coef(exp2) + q.val  * std.err
)
#The model output using robust standard errors is:
r.est2

write.table(r.est2, "sTable10.MICEResiduals2.txt", sep="\t")

# Third imputed dataset
data_imputed3<-data_2_impute_exp
data_imputed3$lengthfollowupyears[is.na(data_2_impute_exp$lengthfollowupyears)]<-imp100$imp$lengthfollowupyears[,3]
data_imputed3$rf_onset[is.na(data_2_impute_exp$rf_onset)]<-imp100$imp$rf_onset[,3]
data_imputed3$rf_dup[is.na(data_2_impute_exp$rf_dup)]<-imp100$imp$rf_dup[,3]
data_imputed3$rf_gender[is.na(data_2_impute_exp$rf_gender)]<-imp100$imp$rf_gender[,3]
data_imputed3$rf_bmi[is.na(data_2_impute_exp$rf_bmi)]<-imp100$imp$rf_bmi[,3]
data_imputed3$rf_relationshipcurr[is.na(data_2_impute_exp$rf_relationshipcurr)]<-imp100$imp$rf_relationshipcurr[,3]
data_imputed3$rf_livingcurr[is.na(data_2_impute_exp$rf_livingcurr)]<-imp100$imp$rf_livingcurr[,3]
data_imputed3$rf_employment[is.na(data_2_impute_exp$rf_employment)]<-imp100$imp$rf_employment[,3]
data_imputed3$rf_educationyears[is.na(data_2_impute_exp$rf_educationyears)]<-imp100$imp$rf_educationyears[,3]
data_imputed3$rf_cannabis[is.na(data_2_impute_exp$rf_cannabis)]<-imp100$imp$rf_cannabis[,3]
data_imputed3$rf_tobacco[is.na(data_2_impute_exp$rf_tobacco)]<-imp100$imp$rf_tobacco[,3]
data_imputed3$rf_alcohol[is.na(data_2_impute_exp$rf_alcohol)]<-imp100$imp$rf_alcohol[,3]
data_imputed3$rf_pansspos[is.na(data_2_impute_exp$rf_pansspos)]<-imp100$imp$rf_pansspos[,3]
data_imputed3$rf_saps[is.na(data_2_impute_exp$rf_saps)]<-imp100$imp$rf_saps[,3]
data_imputed3$rf_sans[is.na(data_2_impute_exp$rf_sans)]<-imp100$imp$rf_sans[,3]
data_imputed3$rf_bprstotal[is.na(data_2_impute_exp$rf_bprstotal)]<-imp100$imp$rf_bprstotal[,3]
data_imputed3$rf_gaf[is.na(data_2_impute_exp$rf_gaf)]<-imp100$imp$rf_gaf[,3]
data_imputed3$rf_ethnicity02[is.na(data_2_impute_exp$rf_ethnicity02)]<-imp100$imp$rf_ethnicity02[,3]

imp3<-data.frame(data_imputed3$strataeitherever,data_imputed3$lengthfollowupyears,data_imputed3$cohortstrata,
                 data_imputed3$rf_onset,
                 data_imputed3$rf_dup,data_imputed3$rf_gender,data_imputed3$rf_bmi,data_imputed3$rf_relationshipcurr,
                 data_imputed3$rf_livingcurr,
                 data_imputed3$rf_employment,data_imputed3$rf_educationyears,
                 data_imputed3$rf_cannabis,data_imputed3$rf_tobacco,data_imputed3$rf_alcohol,
                 data_imputed3$rf_pansspos,data_imputed3$rf_saps,data_imputed3$rf_sans,data_imputed3$rf_bprstotal,
                 data_imputed3$rf_gaf,data_imputed3$rf_ethnicity02)

exp3<-with(data=data_imputed3, glm(strataeitherever~lengthfollowupyears + cohortstrata + rf_onset +
                                     rf_dup + rf_gender + rf_bmi + rf_relationshipcurr + rf_livingcurr + rf_employment +
                                     rf_educationyears + 
                                     rf_cannabis + rf_tobacco + rf_alcohol + rf_pansspos + rf_saps + rf_sans + 
                                     rf_bprstotal + rf_gaf + rf_ethnicity02,
                                   family = binomial))

probabilities <- predict(exp3, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "nonTRS", "TRS")
head(predicted.classes)

mydata3 <- data_imputed3 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata3)

mydata3 <- mydata3 %>%  
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata3, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(exp3, which = 4, id.n = 3)

model.data <- augment(exp3) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = strataeitherever), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3) 

glm.diag.plots(exp3, glmdiag = glm.diag(exp3),
               iden = FALSE, labels = NULL, ret = FALSE)

cov.m3 <- vcovHC(exp3, type = "HC0")

std.err <- sqrt(diag(cov.m3))

q.val <- qnorm(0.975)

r.est3 <- cbind(
  Estimate = coef(exp3)
  , "Robust SE" = std.err
  , z = (coef(exp3)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(exp3)/std.err), lower.tail = FALSE)
  , LL = coef(exp3) - q.val  * std.err
  , UL = coef(exp3) + q.val  * std.err
)
#The model output using robust standard errors is:
r.est3

write.table(r.est3, "sTable11.MICEResiduals3.txt", sep="\t")

# Fourth imputed dataset
data_imputed4<-data_2_impute_exp
data_imputed4$lengthfollowupyears[is.na(data_2_impute_exp$lengthfollowupyears)]<-imp100$imp$lengthfollowupyears[,4]
data_imputed4$rf_onset[is.na(data_2_impute_exp$rf_onset)]<-imp100$imp$rf_onset[,4]
data_imputed4$rf_dup[is.na(data_2_impute_exp$rf_dup)]<-imp100$imp$rf_dup[,4]
data_imputed4$rf_gender[is.na(data_2_impute_exp$rf_gender)]<-imp100$imp$rf_gender[,4]
data_imputed4$rf_bmi[is.na(data_2_impute_exp$rf_bmi)]<-imp100$imp$rf_bmi[,4]
data_imputed4$rf_relationshipcurr[is.na(data_2_impute_exp$rf_relationshipcurr)]<-imp100$imp$rf_relationshipcurr[,4]
data_imputed4$rf_livingcurr[is.na(data_2_impute_exp$rf_livingcurr)]<-imp100$imp$rf_livingcurr[,4]
data_imputed4$rf_employment[is.na(data_2_impute_exp$rf_employment)]<-imp100$imp$rf_employment[,4]
data_imputed4$rf_educationyears[is.na(data_2_impute_exp$rf_educationyears)]<-imp100$imp$rf_educationyears[,4]
data_imputed4$rf_cannabis[is.na(data_2_impute_exp$rf_cannabis)]<-imp100$imp$rf_cannabis[,4]
data_imputed4$rf_tobacco[is.na(data_2_impute_exp$rf_tobacco)]<-imp100$imp$rf_tobacco[,4]
data_imputed4$rf_alcohol[is.na(data_2_impute_exp$rf_alcohol)]<-imp100$imp$rf_alcohol[,4]
data_imputed4$rf_pansspos[is.na(data_2_impute_exp$rf_pansspos)]<-imp100$imp$rf_pansspos[,4]
data_imputed4$rf_saps[is.na(data_2_impute_exp$rf_saps)]<-imp100$imp$rf_saps[,4]
data_imputed4$rf_sans[is.na(data_2_impute_exp$rf_sans)]<-imp100$imp$rf_sans[,4]
data_imputed4$rf_bprstotal[is.na(data_2_impute_exp$rf_bprstotal)]<-imp100$imp$rf_bprstotal[,4]
data_imputed4$rf_gaf[is.na(data_2_impute_exp$rf_gaf)]<-imp100$imp$rf_gaf[,4]
data_imputed4$rf_ethnicity02[is.na(data_2_impute_exp$rf_ethnicity02)]<-imp100$imp$rf_ethnicity02[,4]

imp4<-data.frame(data_imputed4$strataeitherever,data_imputed4$lengthfollowupyears,data_imputed4$cohortstrata,
                 data_imputed4$rf_onset,
                 data_imputed4$rf_dup,data_imputed4$rf_gender,data_imputed4$rf_bmi,data_imputed4$rf_relationshipcurr,
                 data_imputed4$rf_livingcurr,data_imputed4$rf_employment,data_imputed4$rf_educationyears,
                 data_imputed4$rf_cannabis,data_imputed4$rf_tobacco,data_imputed4$rf_alcohol,
                 data_imputed4$rf_pansspos,data_imputed4$rf_saps,data_imputed4$rf_sans,data_imputed4$rf_bprstotal,
                 data_imputed4$rf_gaf,data_imputed4$rf_ethnicity02)

exp4<-with(data=data_imputed4, glm(strataeitherever~lengthfollowupyears + cohortstrata + rf_onset +
                                     rf_dup + rf_gender + rf_bmi + rf_relationshipcurr + rf_livingcurr +
                                     rf_employment + rf_educationyears + 
                                     rf_cannabis + rf_tobacco + rf_alcohol + rf_pansspos + rf_saps + rf_sans + 
                                     rf_bprstotal + rf_gaf + rf_ethnicity02,
                                   family = binomial))

probabilities <- predict(exp4, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "nonTRS", "TRS")
head(predicted.classes)

mydata4 <- data_imputed4 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata4)

mydata4 <- mydata4 %>%  
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata4, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(exp4, which = 4, id.n = 3)

model.data <- augment(exp4) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = strataeitherever), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3) 

glm.diag.plots(exp4, glmdiag = glm.diag(exp4),
               iden = FALSE, labels = NULL, ret = FALSE)

cov.m4 <- vcovHC(exp4, type = "HC0")

std.err <- sqrt(diag(cov.m4))

q.val <- qnorm(0.975)

r.est4 <- cbind(
  Estimate = coef(exp4)
  , "Robust SE" = std.err
  , z = (coef(exp4)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(exp4)/std.err), lower.tail = FALSE)
  , LL = coef(exp4) - q.val  * std.err
  , UL = coef(exp4) + q.val  * std.err
)
#The model output using robust standard errors is:
r.est4

write.table(r.est4, "sTable12.MICEResiduals4.txt", sep="\t")

# Fifth imputed dataset
data_imputed5<-data_2_impute_exp
data_imputed5$lengthfollowupyears[is.na(data_2_impute_exp$lengthfollowupyears)]<-imp100$imp$lengthfollowupyears[,5]
data_imputed5$rf_onset[is.na(data_2_impute_exp$rf_onset)]<-imp100$imp$rf_onset[,5]
data_imputed5$rf_dup[is.na(data_2_impute_exp$rf_dup)]<-imp100$imp$rf_dup[,5]
data_imputed5$rf_gender[is.na(data_2_impute_exp$rf_gender)]<-imp100$imp$rf_gender[,5]
data_imputed5$rf_bmi[is.na(data_2_impute_exp$rf_bmi)]<-imp100$imp$rf_bmi[,5]
data_imputed5$rf_relationshipcurr[is.na(data_2_impute_exp$rf_relationshipcurr)]<-imp100$imp$rf_relationshipcurr[,5]
data_imputed5$rf_livingcurr[is.na(data_2_impute_exp$rf_livingcurr)]<-imp100$imp$rf_livingcurr[,5]
data_imputed5$rf_employment[is.na(data_2_impute_exp$rf_employment)]<-imp100$imp$rf_employment[,5]
data_imputed5$rf_educationyears[is.na(data_2_impute_exp$rf_educationyears)]<-imp100$imp$rf_educationyears[,5]
data_imputed5$rf_cannabis[is.na(data_2_impute_exp$rf_cannabis)]<-imp100$imp$rf_cannabis[,5]
data_imputed5$rf_tobacco[is.na(data_2_impute_exp$rf_tobacco)]<-imp100$imp$rf_tobacco[,5]
data_imputed5$rf_alcohol[is.na(data_2_impute_exp$rf_alcohol)]<-imp100$imp$rf_alcohol[,5]
data_imputed5$rf_pansspos[is.na(data_2_impute_exp$rf_pansspos)]<-imp100$imp$rf_pansspos[,5]
data_imputed5$rf_saps[is.na(data_2_impute_exp$rf_saps)]<-imp100$imp$rf_saps[,5]
data_imputed5$rf_sans[is.na(data_2_impute_exp$rf_sans)]<-imp100$imp$rf_sans[,5]
data_imputed5$rf_bprstotal[is.na(data_2_impute_exp$rf_bprstotal)]<-imp100$imp$rf_bprstotal[,5]
data_imputed5$rf_gaf[is.na(data_2_impute_exp$rf_gaf)]<-imp100$imp$rf_gaf[,5]
data_imputed5$rf_ethnicity02[is.na(data_2_impute_exp$rf_ethnicity02)]<-imp100$imp$rf_ethnicity02[,5]

imp5<-data.frame(data_imputed5$strataeitherever,data_imputed5$lengthfollowupyears,data_imputed5$cohortstrata,
                 data_imputed5$rf_onset,
                 data_imputed5$rf_dup,data_imputed5$rf_gender,data_imputed5$rf_bmi,data_imputed5$rf_relationshipcurr,
                 data_imputed5$rf_livingcurr,data_imputed5$rf_employment,data_imputed5$rf_educationyears,
                 data_imputed5$rf_cannabis,data_imputed5$rf_tobacco,data_imputed5$rf_alcohol,
                 data_imputed5$rf_pansspos,data_imputed5$rf_saps,data_imputed5$rf_sans,data_imputed5$rf_bprstotal,
                 data_imputed5$rf_gaf,data_imputed5$rf_ethnicity02)

exp5<-with(data=data_imputed5, glm(strataeitherever~lengthfollowupyears + cohortstrata + rf_onset +
                                     rf_dup + rf_gender + rf_bmi + rf_relationshipcurr + rf_livingcurr + 
                                     rf_employment + rf_educationyears + 
                                     rf_cannabis + rf_tobacco + rf_alcohol + rf_pansspos + rf_saps + rf_sans + 
                                     rf_bprstotal + rf_gaf + rf_ethnicity02,
                                   family = binomial))

probabilities <- predict(exp5, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "nonTRS", "TRS")
head(predicted.classes)

mydata5 <- data_imputed5 %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata5)

mydata5 <- mydata5 %>%  
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata5, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(exp5, which = 4, id.n = 3)

model.data <- augment(exp5) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = strataeitherever), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3) 

glm.diag.plots(exp5, glmdiag = glm.diag(exp5),
               iden = FALSE, labels = NULL, ret = FALSE)

cov.m5 <- vcovHC(exp5, type = "HC0")

std.err <- sqrt(diag(cov.m5))

q.val <- qnorm(0.975)

r.est5 <- cbind(
  Estimate = coef(exp5)
  , "Robust SE" = std.err
  , z = (coef(exp5)/std.err)
  , "Pr(>|z|) "= 2 * pnorm(abs(coef(exp5)/std.err), lower.tail = FALSE)
  , LL = coef(exp5) - q.val  * std.err
  , UL = coef(exp5) + q.val  * std.err
)
#The model output using robust standard errors is:
r.est5

write.table(r.est5, "sTable13.MICEResiduals5.txt", sep="\t")

##############################################################################
# Univariable Models (plus checking distributions)
##############################################################################
#lengthfollowupyears
#no missing data so use data_2_impute_exp
lengthfollowupyears = with(data=data_2_impute_exp, glm(strataeitherever~lengthfollowupyears,
                                                                    family = binomial))
glm.diag.plots(lengthfollowupyears, glmdiag = glm.diag(lengthfollowupyears),
               iden = FALSE, labels = NULL, ret = FALSE)
lengthfollowupyears_sum <- summary(lengthfollowupyears)
lengthfollowupyears_table <- round(lengthfollowupyears_sum$coefficients,4)
write.table(lengthfollowupyears_table, "lengthfollowupyears_table.txt", sep="\t")

#cohortstrata
#no missing data so use data_2_impute_exp
cohortstrata = with(data=data_2_impute_exp, glm(strataeitherever~cohortstrata,
                                                       family = binomial))
glm.diag.plots(cohortstrata, glmdiag = glm.diag(cohortstrata),
               iden = FALSE, labels = NULL, ret = FALSE)
cohortstrata_sum <- summary(cohortstrata)
cohortstrata_table <- round(cohortstrata_sum$coefficients,4)
write.table(cohortstrata_table, "cohortstrata_table.txt", sep="\t")

#rf_onset
#missing data so use imp100
rf_onset = with(data=imp100, glm(strataeitherever~rf_onset,
                                                  family = binomial))
rf_onset_combFit = pool(rf_onset)
rf_onset_sum <- summary(rf_onset_combFit)
rf_onset_table <- rf_onset_sum
write.table(rf_onset_table, "rf_onset_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_onset1 = with(data=data_imputed, glm(strataeitherever~rf_onset,
                                           family = binomial))
glm.diag.plots(exprf_onset1, glmdiag = glm.diag(exprf_onset1),
               iden = FALSE, labels = NULL, ret = FALSE)
#Influential values
plot(exprf_onset1, which = 4, id.n = 3)
# Extract model results
model.data_onset1 <- augment(exprf_onset1) %>% 
  mutate(index = 1:n()) 
# The data for the top 3 largest values, according to the Cook's distance, can be displayed as follow:
model.data_onset1 %>% top_n(3, .cooksd)
# Plot the standardized residuals:
ggplot(model.data_onset1, aes(index, .std.resid)) + 
  geom_point(aes(color = strataeitherever), alpha = .5) +
  theme_bw()
# Filter potential influential data points with abs(.std.res) > 3:
model.data_onset1 %>% 
  filter(abs(.std.resid) > 3)# 0 points which are influential
#second imputation
exprf_onset2 = with(data=data_imputed2, glm(strataeitherever~rf_onset,
                                            family = binomial))
glm.diag.plots(exprf_onset2, glmdiag = glm.diag(exprf_onset2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_onset3 = with(data=data_imputed3, glm(strataeitherever~rf_onset,
                                            family = binomial))
glm.diag.plots(exprf_onset3, glmdiag = glm.diag(exprf_onset3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_onset4 = with(data=data_imputed4, glm(strataeitherever~rf_onset,
                                            family = binomial))
glm.diag.plots(exprf_onset4, glmdiag = glm.diag(exprf_onset4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_onset5 = with(data=data_imputed5, glm(strataeitherever~rf_onset,
                                            family = binomial))
glm.diag.plots(exprf_onset5, glmdiag = glm.diag(exprf_onset5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_dup
rf_dup = with(data=imp100, glm(strataeitherever~rf_dup,
                                 family = binomial))
rf_dup_combFit = pool(rf_dup)
rf_dup_sum <- summary(rf_dup_combFit)
rf_dup_table <- rf_dup_sum
write.table(rf_dup_table, "rf_dup_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_dup1 = with(data=data_imputed, glm(strataeitherever~rf_dup,
                                           family = binomial))
glm.diag.plots(exprf_dup1, glmdiag = glm.diag(exprf_dup1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_dup2 = with(data=data_imputed2, glm(strataeitherever~rf_dup,
                                            family = binomial))
glm.diag.plots(exprf_dup2, glmdiag = glm.diag(exprf_dup2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_dup3 = with(data=data_imputed3, glm(strataeitherever~rf_dup,
                                            family = binomial))
glm.diag.plots(exprf_dup3, glmdiag = glm.diag(exprf_dup3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_dup4 = with(data=data_imputed4, glm(strataeitherever~rf_dup,
                                            family = binomial))
glm.diag.plots(exprf_dup4, glmdiag = glm.diag(exprf_dup4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_dup5 = with(data=data_imputed5, glm(strataeitherever~rf_dup,
                                            family = binomial))
glm.diag.plots(exprf_dup5, glmdiag = glm.diag(exprf_dup5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_gender
rf_gender = with(data=imp100, glm(strataeitherever~rf_gender,
                               family = binomial))
rf_gender_combFit = pool(rf_gender)
rf_gender_sum <- summary(rf_gender_combFit)
rf_gender_table <- rf_gender_sum
write.table(rf_gender_table, "rf_gender_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_gender1 = with(data=data_imputed, glm(strataeitherever~rf_gender,
                                         family = binomial))
glm.diag.plots(exprf_gender1, glmdiag = glm.diag(exprf_gender1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_gender2 = with(data=data_imputed2, glm(strataeitherever~rf_gender,
                                          family = binomial))
glm.diag.plots(exprf_gender2, glmdiag = glm.diag(exprf_gender2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_gender3 = with(data=data_imputed3, glm(strataeitherever~rf_gender,
                                          family = binomial))
glm.diag.plots(exprf_gender3, glmdiag = glm.diag(exprf_gender3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_gender4 = with(data=data_imputed4, glm(strataeitherever~rf_gender,
                                          family = binomial))
glm.diag.plots(exprf_gender4, glmdiag = glm.diag(exprf_gender4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_gender5 = with(data=data_imputed5, glm(strataeitherever~rf_gender,
                                          family = binomial))
glm.diag.plots(exprf_gender5, glmdiag = glm.diag(exprf_gender5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_bmi
rf_bmi = with(data=imp100, glm(strataeitherever~rf_bmi,
                                  family = binomial))
rf_bmi_combFit = pool(rf_bmi)
rf_bmi_sum <- summary(rf_bmi_combFit)
rf_bmi_table <- rf_bmi_sum
write.table(rf_bmi_table, "rf_bmi_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_bmi1 = with(data=data_imputed, glm(strataeitherever~rf_bmi,
                                            family = binomial))
glm.diag.plots(exprf_bmi1, glmdiag = glm.diag(exprf_bmi1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_bmi2 = with(data=data_imputed2, glm(strataeitherever~rf_bmi,
                                             family = binomial))
glm.diag.plots(exprf_bmi2, glmdiag = glm.diag(exprf_bmi2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_bmi3 = with(data=data_imputed3, glm(strataeitherever~rf_bmi,
                                             family = binomial))
glm.diag.plots(exprf_bmi3, glmdiag = glm.diag(exprf_bmi3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_bmi4 = with(data=data_imputed4, glm(strataeitherever~rf_bmi,
                                             family = binomial))
glm.diag.plots(exprf_bmi4, glmdiag = glm.diag(exprf_bmi4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_bmi5 = with(data=data_imputed5, glm(strataeitherever~rf_bmi,
                                             family = binomial))
glm.diag.plots(exprf_bmi5, glmdiag = glm.diag(exprf_bmi5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_relationshipcurr
rf_relationshipcurr = with(data=imp100, glm(strataeitherever~rf_relationshipcurr,
                               family = binomial))
rf_relationshipcurr_combFit = pool(rf_relationshipcurr)
rf_relationshipcurr_sum <- summary(rf_relationshipcurr_combFit)
rf_relationshipcurr_table <- rf_relationshipcurr_sum
write.table(rf_relationshipcurr_table, "rf_relationshipcurr_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_relationshipcurr1 = with(data=data_imputed, glm(strataeitherever~rf_relationshipcurr,
                                         family = binomial))
glm.diag.plots(exprf_bmi1, glmdiag = glm.diag(exprf_bmi1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_relationshipcurr2 = with(data=data_imputed2, glm(strataeitherever~rf_relationshipcurr,
                                          family = binomial))
glm.diag.plots(exprf_relationshipcurr2, glmdiag = glm.diag(exprf_relationshipcurr2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_relationshipcurr3 = with(data=data_imputed3, glm(strataeitherever~rf_relationshipcurr,
                                          family = binomial))
glm.diag.plots(exprf_relationshipcurr3, glmdiag = glm.diag(exprf_relationshipcurr3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_relationshipcurr4 = with(data=data_imputed4, glm(strataeitherever~rf_relationshipcurr,
                                          family = binomial))
glm.diag.plots(exprf_relationshipcurr4, glmdiag = glm.diag(exprf_relationshipcurr4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_relationshipcurr5 = with(data=data_imputed5, glm(strataeitherever~rf_relationshipcurr,
                                          family = binomial))
glm.diag.plots(exprf_relationshipcurr5, glmdiag = glm.diag(exprf_relationshipcurr5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_livingcurr
rf_livingcurr = with(data=imp100, glm(strataeitherever~rf_livingcurr,
                                            family = binomial))
rf_livingcurr_combFit = pool(rf_livingcurr)
rf_livingcurr_sum <- summary(rf_livingcurr_combFit)
rf_livingcurr_table <- rf_livingcurr_sum
write.table(rf_livingcurr_table, "rf_livingcurr_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_livingcurr1 = with(data=data_imputed, glm(strataeitherever~rf_livingcurr,
                                                      family = binomial))
glm.diag.plots(exprf_livingcurr1, glmdiag = glm.diag(exprf_livingcurr1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_livingcurr2 = with(data=data_imputed2, glm(strataeitherever~rf_livingcurr,
                                                       family = binomial))
glm.diag.plots(exprf_livingcurr2, glmdiag = glm.diag(exprf_livingcurr2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_livingcurr3 = with(data=data_imputed3, glm(strataeitherever~rf_livingcurr,
                                                       family = binomial))
glm.diag.plots(exprf_livingcurr3, glmdiag = glm.diag(exprf_livingcurr3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_livingcurr4 = with(data=data_imputed4, glm(strataeitherever~rf_livingcurr,
                                                       family = binomial))
glm.diag.plots(exprf_livingcurr4, glmdiag = glm.diag(exprf_livingcurr4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_livingcurr5 = with(data=data_imputed5, glm(strataeitherever~rf_livingcurr,
                                                       family = binomial))
glm.diag.plots(exprf_livingcurr5, glmdiag = glm.diag(exprf_livingcurr5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_employment
rf_employment = with(data=imp100, glm(strataeitherever~rf_employment,
                                         family = binomial))
rf_employment_combFit = pool(rf_employment)
rf_employment_sum <- summary(rf_employment_combFit)
rf_employment_table <- rf_employment_sum
write.table(rf_employment_table, "rf_employment_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_employment1 = with(data=data_imputed, glm(strataeitherever~rf_employment,
                                                   family = binomial))
glm.diag.plots(exprf_employment1, glmdiag = glm.diag(exprf_employment1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_employment2 = with(data=data_imputed2, glm(strataeitherever~rf_employment,
                                                    family = binomial))
glm.diag.plots(exprf_employment2, glmdiag = glm.diag(exprf_employment2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_employment3 = with(data=data_imputed3, glm(strataeitherever~rf_employment,
                                                    family = binomial))
glm.diag.plots(exprf_employment3, glmdiag = glm.diag(exprf_employment3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_employment4 = with(data=data_imputed4, glm(strataeitherever~rf_employment,
                                                    family = binomial))
glm.diag.plots(exprf_employment4, glmdiag = glm.diag(exprf_employment4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_employment5 = with(data=data_imputed5, glm(strataeitherever~rf_employment,
                                                    family = binomial))
glm.diag.plots(exprf_employment5, glmdiag = glm.diag(exprf_employment5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_educationyears
rf_educationyears = with(data=imp100, glm(strataeitherever~rf_educationyears,
                                      family = binomial))
rf_educationyears_combFit = pool(rf_educationyears)
rf_educationyears_sum <- summary(rf_educationyears_combFit)
rf_educationyears_table <- rf_educationyears_sum
write.table(rf_educationyears_table, "rf_educationyears_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_educationyears1 = with(data=data_imputed, glm(strataeitherever~rf_educationyears,
                                                family = binomial))
glm.diag.plots(exprf_educationyears1, glmdiag = glm.diag(exprf_educationyears1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_educationyears2 = with(data=data_imputed2, glm(strataeitherever~rf_educationyears,
                                                 family = binomial))
glm.diag.plots(exprf_educationyears2, glmdiag = glm.diag(exprf_educationyears2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_educationyears3 = with(data=data_imputed3, glm(strataeitherever~rf_educationyears,
                                                 family = binomial))
glm.diag.plots(exprf_educationyears3, glmdiag = glm.diag(exprf_educationyears3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_educationyears4 = with(data=data_imputed4, glm(strataeitherever~rf_educationyears,
                                                 family = binomial))
glm.diag.plots(exprf_educationyears4, glmdiag = glm.diag(exprf_educationyears4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_educationyears5 = with(data=data_imputed5, glm(strataeitherever~rf_educationyears,
                                                 family = binomial))
glm.diag.plots(exprf_educationyears5, glmdiag = glm.diag(exprf_educationyears5),
               iden = FALSE, labels = NULL, ret = FALSE)


#rf_cannabis
rf_cannabis = with(data=imp100, glm(strataeitherever~rf_cannabis,
                                         family = binomial))
rf_cannabis_combFit = pool(rf_cannabis)
rf_cannabis_sum <- summary(rf_cannabis_combFit)
rf_cannabis_table <- rf_cannabis_sum
write.table(rf_cannabis_table, "rf_cannabis_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_cannabis1 = with(data=data_imputed, glm(strataeitherever~rf_cannabis,
                                                   family = binomial))
glm.diag.plots(exprf_cannabis1, glmdiag = glm.diag(exprf_cannabis1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_cannabis2 = with(data=data_imputed2, glm(strataeitherever~rf_cannabis,
                                                    family = binomial))
glm.diag.plots(exprf_cannabis2, glmdiag = glm.diag(exprf_cannabis2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_cannabis3 = with(data=data_imputed3, glm(strataeitherever~rf_cannabis,
                                                    family = binomial))
glm.diag.plots(exprf_cannabis3, glmdiag = glm.diag(exprf_cannabis3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_cannabis4 = with(data=data_imputed4, glm(strataeitherever~rf_cannabis,
                                                    family = binomial))
glm.diag.plots(exprf_cannabis4, glmdiag = glm.diag(exprf_cannabis4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_cannabis5 = with(data=data_imputed5, glm(strataeitherever~rf_cannabis,
                                                    family = binomial))
glm.diag.plots(exprf_cannabis5, glmdiag = glm.diag(exprf_cannabis5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_tobacco
rf_tobacco = with(data=imp100, glm(strataeitherever~rf_tobacco,
                                    family = binomial))
rf_tobacco_combFit = pool(rf_tobacco)
rf_tobacco_sum <- summary(rf_tobacco_combFit)
rf_tobacco_table <- rf_tobacco_sum
write.table(rf_tobacco_table, "rf_tobacco_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_tobacco1 = with(data=data_imputed, glm(strataeitherever~rf_tobacco,
                                              family = binomial))
glm.diag.plots(exprf_tobacco1, glmdiag = glm.diag(exprf_tobacco1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_tobacco2 = with(data=data_imputed2, glm(strataeitherever~rf_tobacco,
                                               family = binomial))
glm.diag.plots(exprf_tobacco2, glmdiag = glm.diag(exprf_tobacco2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_tobacco3 = with(data=data_imputed3, glm(strataeitherever~rf_tobacco,
                                               family = binomial))
glm.diag.plots(exprf_tobacco3, glmdiag = glm.diag(exprf_tobacco3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_tobacco4 = with(data=data_imputed4, glm(strataeitherever~rf_tobacco,
                                               family = binomial))
glm.diag.plots(exprf_tobacco4, glmdiag = glm.diag(exprf_tobacco4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_tobacco5 = with(data=data_imputed5, glm(strataeitherever~rf_tobacco,
                                               family = binomial))
glm.diag.plots(exprf_tobacco5, glmdiag = glm.diag(exprf_tobacco5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_alcohol
rf_alcohol = with(data=imp100, glm(strataeitherever~rf_alcohol,
                                   family = binomial))
rf_alcohol_combFit = pool(rf_alcohol)
rf_alcohol_sum <- summary(rf_alcohol_combFit)
rf_alcohol_table <- rf_alcohol_sum
write.table(rf_alcohol_table, "rf_alcohol_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_alcohol1 = with(data=data_imputed, glm(strataeitherever~rf_alcohol,
                                             family = binomial))
glm.diag.plots(exprf_alcohol1, glmdiag = glm.diag(exprf_alcohol1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_alcohol2 = with(data=data_imputed2, glm(strataeitherever~rf_alcohol,
                                              family = binomial))
glm.diag.plots(exprf_alcohol2, glmdiag = glm.diag(exprf_alcohol2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_alcohol3 = with(data=data_imputed3, glm(strataeitherever~rf_alcohol,
                                              family = binomial))
glm.diag.plots(exprf_alcohol3, glmdiag = glm.diag(exprf_alcohol3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_alcohol4 = with(data=data_imputed4, glm(strataeitherever~rf_alcohol,
                                              family = binomial))
glm.diag.plots(exprf_alcohol4, glmdiag = glm.diag(exprf_alcohol4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_alcohol5 = with(data=data_imputed5, glm(strataeitherever~rf_alcohol,
                                              family = binomial))
glm.diag.plots(exprf_alcohol5, glmdiag = glm.diag(exprf_alcohol5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_pansspos
rf_pansspos = with(data=imp100, glm(strataeitherever~rf_pansspos,
                                   family = binomial))
rf_pansspos_combFit = pool(rf_pansspos)
rf_pansspos_sum <- summary(rf_pansspos_combFit)
rf_pansspos_table <- rf_pansspos_sum
write.table(rf_pansspos_table, "rf_pansspos_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_pansspos1 = with(data=data_imputed, glm(strataeitherever~rf_pansspos,
                                             family = binomial))
glm.diag.plots(exprf_pansspos1, glmdiag = glm.diag(exprf_pansspos1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_pansspos2 = with(data=data_imputed2, glm(strataeitherever~rf_pansspos,
                                              family = binomial))
glm.diag.plots(exprf_pansspos2, glmdiag = glm.diag(exprf_pansspos2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_pansspos3 = with(data=data_imputed3, glm(strataeitherever~rf_pansspos,
                                              family = binomial))
glm.diag.plots(exprf_pansspos3, glmdiag = glm.diag(exprf_pansspos3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_pansspos4 = with(data=data_imputed4, glm(strataeitherever~rf_pansspos,
                                              family = binomial))
glm.diag.plots(exprf_pansspos4, glmdiag = glm.diag(exprf_pansspos4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_pansspos5 = with(data=data_imputed5, glm(strataeitherever~rf_pansspos,
                                              family = binomial))
glm.diag.plots(exprf_pansspos5, glmdiag = glm.diag(exprf_pansspos5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_saps
rf_saps = with(data=imp100, glm(strataeitherever~rf_saps,
                                    family = binomial))
rf_saps_combFit = pool(rf_saps)
rf_saps_sum <- summary(rf_saps_combFit)
rf_saps_table <- rf_saps_sum
write.table(rf_saps_table, "rf_saps_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_saps1 = with(data=data_imputed, glm(strataeitherever~rf_saps,
                                              family = binomial))
glm.diag.plots(exprf_saps1, glmdiag = glm.diag(exprf_saps1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_saps2 = with(data=data_imputed2, glm(strataeitherever~rf_saps,
                                               family = binomial))
glm.diag.plots(exprf_saps2, glmdiag = glm.diag(exprf_saps2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_saps3 = with(data=data_imputed3, glm(strataeitherever~rf_saps,
                                               family = binomial))
glm.diag.plots(exprf_saps3, glmdiag = glm.diag(exprf_saps3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_saps4 = with(data=data_imputed4, glm(strataeitherever~rf_saps,
                                               family = binomial))
glm.diag.plots(exprf_saps4, glmdiag = glm.diag(exprf_saps4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_saps5 = with(data=data_imputed5, glm(strataeitherever~rf_saps,
                                               family = binomial))
glm.diag.plots(exprf_saps5, glmdiag = glm.diag(exprf_saps5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_sans
rf_sans = with(data=imp100, glm(strataeitherever~rf_sans,
                                family = binomial))
rf_sans_combFit = pool(rf_sans)
rf_sans_sum <- summary(rf_sans_combFit)
rf_sans_table <- rf_sans_sum
write.table(rf_sans_table, "rf_sans_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_sans1 = with(data=data_imputed, glm(strataeitherever~rf_sans,
                                          family = binomial))
glm.diag.plots(exprf_sans1, glmdiag = glm.diag(exprf_sans1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_sans2 = with(data=data_imputed2, glm(strataeitherever~rf_sans,
                                           family = binomial))
glm.diag.plots(exprf_sans2, glmdiag = glm.diag(exprf_sans2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_sans3 = with(data=data_imputed3, glm(strataeitherever~rf_sans,
                                           family = binomial))
glm.diag.plots(exprf_sans3, glmdiag = glm.diag(exprf_sans3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_sans4 = with(data=data_imputed4, glm(strataeitherever~rf_sans,
                                           family = binomial))
glm.diag.plots(exprf_sans4, glmdiag = glm.diag(exprf_sans4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_sans5 = with(data=data_imputed5, glm(strataeitherever~rf_sans,
                                           family = binomial))
glm.diag.plots(exprf_sans5, glmdiag = glm.diag(exprf_sans5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_bprstotal
rf_bprstotal = with(data=imp100, glm(strataeitherever~rf_bprstotal,
                                family = binomial))
rf_bprstotal_combFit = pool(rf_bprstotal)
rf_bprstotal_sum <- summary(rf_bprstotal_combFit)
rf_bprstotal_table <- rf_bprstotal_sum
write.table(rf_bprstotal_table, "rf_bprstotal_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_bprstotal1 = with(data=data_imputed, glm(strataeitherever~rf_bprstotal,
                                          family = binomial))
glm.diag.plots(exprf_bprstotal1, glmdiag = glm.diag(exprf_bprstotal1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_bprstotal2 = with(data=data_imputed2, glm(strataeitherever~rf_bprstotal,
                                           family = binomial))
glm.diag.plots(exprf_bprstotal2, glmdiag = glm.diag(exprf_bprstotal2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_bprstotal3 = with(data=data_imputed3, glm(strataeitherever~rf_bprstotal,
                                           family = binomial))
glm.diag.plots(exprf_bprstotal3, glmdiag = glm.diag(exprf_bprstotal3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_bprstotal4 = with(data=data_imputed4, glm(strataeitherever~rf_bprstotal,
                                           family = binomial))
glm.diag.plots(exprf_bprstotal4, glmdiag = glm.diag(exprf_bprstotal4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_bprstotal5 = with(data=data_imputed5, glm(strataeitherever~rf_bprstotal,
                                           family = binomial))
glm.diag.plots(exprf_bprstotal5, glmdiag = glm.diag(exprf_bprstotal5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_gaf
rf_gaf = with(data=imp100, glm(strataeitherever~rf_gaf,
                                     family = binomial))
rf_gaf_combFit = pool(rf_gaf)
rf_gaf_sum <- summary(rf_gaf_combFit)
rf_gaf_table <- rf_gaf_sum
write.table(rf_gaf_table, "rf_gaf_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_gaf1 = with(data=data_imputed, glm(strataeitherever~rf_gaf,
                                               family = binomial))
glm.diag.plots(exprf_gaf1, glmdiag = glm.diag(exprf_gaf1),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_gaf2 = with(data=data_imputed2, glm(strataeitherever~rf_gaf,
                                                family = binomial))
glm.diag.plots(exprf_gaf2, glmdiag = glm.diag(exprf_gaf2),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_gaf3 = with(data=data_imputed3, glm(strataeitherever~rf_gaf,
                                                family = binomial))
glm.diag.plots(exprf_gaf3, glmdiag = glm.diag(exprf_gaf3),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_gaf4 = with(data=data_imputed4, glm(strataeitherever~rf_gaf,
                                                family = binomial))
glm.diag.plots(exprf_gaf4, glmdiag = glm.diag(exprf_gaf4),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_gaf5 = with(data=data_imputed5, glm(strataeitherever~rf_gaf,
                                                family = binomial))
glm.diag.plots(exprf_gaf5, glmdiag = glm.diag(exprf_gaf5),
               iden = FALSE, labels = NULL, ret = FALSE)

#rf_ethnicity02
rf_ethnicity02 = with(data=imp100, glm(strataeitherever~rf_ethnicity02,
                               family = binomial))
rf_ethnicity02_combFit = pool(rf_ethnicity02)
rf_ethnicity02_sum <- summary(rf_ethnicity02_combFit)
rf_ethnicity02_table <- rf_ethnicity02_sum
write.table(rf_ethnicity02_table, "rf_ethnicity02_table.txt", sep="\t")

#Checking diagnostic:
#first imputation
exprf_ethnicity021 = with(data=data_imputed, glm(strataeitherever~rf_ethnicity02,
                                         family = binomial))
glm.diag.plots(exprf_ethnicity021, glmdiag = glm.diag(exprf_ethnicity021),
               iden = FALSE, labels = NULL, ret = FALSE)
#second imputation
exprf_ethnicity022 = with(data=data_imputed2, glm(strataeitherever~rf_ethnicity02,
                                          family = binomial))
glm.diag.plots(exprf_ethnicity022, glmdiag = glm.diag(exprf_ethnicity022),
               iden = FALSE, labels = NULL, ret = FALSE)
#third imputation
exprf_ethnicity023 = with(data=data_imputed3, glm(strataeitherever~rf_ethnicity02,
                                          family = binomial))
glm.diag.plots(exprf_ethnicity023, glmdiag = glm.diag(exprf_ethnicity023),
               iden = FALSE, labels = NULL, ret = FALSE)
#fourth imputation
exprf_ethnicity024 = with(data=data_imputed4, glm(strataeitherever~rf_ethnicity02,
                                          family = binomial))
glm.diag.plots(exprf_ethnicity024, glmdiag = glm.diag(exprf_ethnicity024),
               iden = FALSE, labels = NULL, ret = FALSE)
#fifth imputation
exprf_ethnicity025 = with(data=data_imputed5, glm(strataeitherever~rf_ethnicity02,
                                          family = binomial))
glm.diag.plots(exprf_ethnicity025, glmdiag = glm.diag(exprf_ethnicity025),
               iden = FALSE, labels = NULL, ret = FALSE)


##############################################################################
# Pooled Multivariable Model
##############################################################################

explanatoryModel = with(data=imp100, glm(strataeitherever~lengthfollowupyears+cohortstrata+
                                           rf_onset+rf_dup+rf_gender+rf_bmi+
                                           rf_relationshipcurr+rf_livingcurr+
                                           rf_employment+rf_educationyears+
                                           rf_cannabis+rf_tobacco+rf_alcohol+
                                           rf_pansspos+rf_saps+rf_sans+rf_bprstotal+rf_gaf+
                                           rf_ethnicity02,
                                           family = binomial(logit)))
summary(explanatoryModel[[1]])
summary(explanatoryModel)
combFit = pool(explanatoryModel)

#confidence intervals ####
# log odds scale
summary(combFit, conf.int = TRUE)
# odds ratio scale
summary(combFit, conf.int = TRUE, exponentiate = TRUE)

explanatoryModel_table <- summary(combFit, conf.int = TRUE)
write.table(explanatoryModel_table, "sTable15.ExplanModMultivariable.txt", sep="\t")

summary(combFit$pooled$riv) #relative increase in variance due to response
summary(combFit$pooled$df) #residual degrees of freedom for hypothesis testing, Barnard-Rubin adjustment for small samples (Barnard and Rubin, 1999).
summary(combFit$pooled$lambda) #proportion of total variance due to missingness
summary(combFit$pooled$fmi) #fraction of missing information
summary(combFit$pooled$ubar) #mean of the variances
summary(combFit$pooled$b) #within imputation variance
summary(combFit$pooled$t) #total variance of the pooled estimates
summary(combFit$pooled$dfcom) #degrees of freedom for estimates in the complete data analysis

##############################################################################
# Pooled AUC and R-squared
##############################################################################

# Stack imputed data into one LONG dataset (generates two new variables indicating id and imputation number); raw (unimputed) data are appended (inc = TRUE)
imp100stacked <- complete(imp100, "long", inc = TRUE)
summary(imp100stacked)
imp100stacked[imp100stacked$.imp==0,] # original data
# We need take the original data and the ID variables off the stacked data:
imp100stackedNoOrig<-imp100stacked[imp100stacked$.imp!=0,-2]
# creating dummie:
MM<-model.matrix(~.,imp100stackedNoOrig)
MMdata<-data.frame(MM)
summary(MMdata)
#note that "strataeitherever" is now called "strataeithereverTRS"

# lp.orig= numeric vector of the original coefficient values 
# we get it from the pooled model:
combFit

predictors <- parse(text=gsub(' ','.',combFit$pooled$term[-1]))

output <- mivalext_lr(
  data.val = MMdata ,
  #data.orig=imp100$data,
  nimp = 100,
  impvar = ".imp",
  Outcome = "strataeithereverTRS",
  predictors = parse(text=gsub('/','.',predictors)), #29 predictors
  lp.orig = combFit$pooled$estimate, #intercept is included (30 estimates)
  cal.plot = TRUE,
  plot.indiv = FALSE,
  val.check = FALSE)

output
output$ROC
output$ROC$`ROC (logit)`

output$R2_fixed

output$R2_calibr

##############################################################################
# Save workspace
##############################################################################
save.image("STRATAGData_21092020_ExpModel.RData")



##############################################################################
##############################################################################
##############################################################################
# END OF SCRIPT ##############################################################
##############################################################################
##############################################################################
##############################################################################
