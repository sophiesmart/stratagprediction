##########################################################################################
##########################################################################################
# STRATA-G PROTOCOL ######################################################################
##########################################################################################
##########################################################################################
# PREDICTION MODEL #######################################################################
##########################################################################################
##########################################################################################
# 21st September 2020 ####################################################################
##########################################################################################
##########################################################################################
# Deborah Agbedjro: deborah.1.agbedjro@kcl.ac.uk
# Sophie Smart: SmartS1@cardiff.ac.uk
##########################################################################################
##########################################################################################

library("rms")

setwd("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/PredModel")
load("STRATAGData_21092020_PredModel.RData")

##########################################################################################
# NOMOGRAM PLOT 
##########################################################################################

# Lasso LOGISTIC

#Recalibrated coefficients
nomlassocoefs<-c(-0.761, rf_onset=-0.0265150902020291, rf_genderFemale=-0.0887210795833479,
                 rf_bmi=0.0348072561515994, rf_relationshipcurrRelationship=-0.369302490534537,
                 rf_educationyears=-0.0558554449760919, rf_alcoholNo=-0.476637067305828, rf_pansspos=-0.00171999849397592)

label(output$data.imputed$rf_onset) <- 'Age at onset (years)'
label(output$data.imputed$rf_gender) <- 'Female (Yes=1)'
label(output$data.imputed$rf_bmi) <- 'BMI'
label(output$data.imputed$rf_relationshipcurr) <- 'Current Relationship (Yes=0)'
label(output$data.imputed$rf_educationyears) <- 'Education (years)'
label(output$data.imputed$rf_alcohol) <- 'Alcohol (No=1)'
label(output$data.imputed$rf_pansspos) <- 'PANSS Positive'

d<-as.data.frame(output$data.imputed)
ddist <- datadist(d); options(datadist='ddist')

f <- lrm(y ~ rf_onset+rf_gender+rf_bmi+rf_relationshipcurr+
           rf_educationyears+rf_alcohol+rf_pansspos,
         data=d)
f$coefficients <- nomlassocoefs
X.data<-model.matrix(strataeitherever~.,output$data.imputed)[,-1]
pred.probabilities<-predict(output$Fit.Caret1SE,newdata=X.data,type="prob")[,2]
linear.predictor<-log(pred.probabilities/(1-pred.probabilities))
f$linear.predictors<-linear.predictor-mean(linear.predictor)  # normalized linear predictors
f$center <-mean(linear.predictor) #mean linear predictors

nomogram <- nomogram(f, fun=function(x) 1/(1+exp(-x)),#fun.lp.at=c(.001,.01,.05,seq(.1,.9,by=.1),.95,.99,.999),
                     funlabel="Risk of TRS")

#Instead of fun.at, could have specified fun.lp.at=logit of
#sequence above - faster and slightly more accurate

plot(nomogram, xfrac=.45)
print(nomogram)


##########################################################################################

library(DynNom)
f2 <- lrm(y ~ rf_onset+rf_gender+rf_bmi+rf_relationshipcurr+
           rf_educationyears+rf_alcohol+rf_pansspos,
         data=d)
f2 <- glm(strataeitherever ~ rf_onset+rf_gender+rf_bmi+rf_relationshipcurr+rf_educationyears+rf_alcohol+rf_pansspos, family = binomial(), data = d)
f2$coefficients <- nomlassocoefs


DynNom(f2)
