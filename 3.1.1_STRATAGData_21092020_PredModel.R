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

library(caret)
library(MatrixModels)
library(missForest) #does not impute out-of-range values
library(doParallel)
library(glmnet)
library(e1071)
library(pROC) # to compute the AUC
library(gmodels) # for CrossTable
library(haven) # to read in .dta data
library(psych) # to check correlations between categorical variables
library(VIM)
library(mice) #to use MICE
library(boot) #for diagnostics
library(dplyr) #for %>% command
library(tidyr) #for gather()
library(ggplot2)
library(broom) #for augment()
library(reshape2)
library(tableone) #for including both cat and cont variables in a stratified table

options("scipen"=100, "digits"=4)
setwd("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/PredModel")

##############################################################################
# Read in data
##############################################################################

load("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/DataPrep/STRATAGData_21092020_DataPrepped.RData")

##############################################################################
# Establish Functions
##############################################################################

# Pre-processing steps #########
# near zero variance variables
nzv<-nearZeroVar(data_2_impute_pred, saveMetrics= TRUE)
nzv #no near zero variance variables


#GRID FOR TUNING PARAMETERS
Grid <- expand.grid(alpha = 1 , lambda= 10^seq(-1,-3,length=100))


#FUNCTIONS NEEDED:__________________________________________________________________________________________________________________________________

#The outcome needs to be the first column of the dataframe

###1st FUNCTION:___________________________________________________________________________________________________________________________________________
### A function to compute apparent performances ################################
AUC.alpha.beta <- function (coef,data.imputed,Fit.Caret) {
  
  Outcome.data <- unlist(data.imputed[,dim(data.imputed)[2]])
  modelMatrix.data <- model.Matrix(as.formula(paste(colnames(data.imputed)[dim(data.imputed)[2]],"~.",sep="")),data.imputed)  
  X.data<-modelMatrix.data[,-1]  
  # y_prob<-predict(Fit.Caret,type="prob",newdata=X.data)
  # y_pred<-predict(Fit.Caret,type="raw",newdata=X.data)
  Predicted.data  <-  as.matrix(exp(coef[1] + X.data%*%coef[-1])/(exp(coef[1] + X.data%*%coef[-1])+1))
  roc_obj <- roc(Outcome.data, as.vector(Predicted.data))
  AUC.apparent   <-as.numeric(roc_obj$auc)
  logOdds<-log(Predicted.data/(1-Predicted.data))
  glm.coef.beta      <-  glm(Outcome.data ~ logOdds,family=binomial)$coef
  Beta.apparent	 <-  glm.coef.beta[2]
  glm.coef.alpha    <- glm(Outcome.data ~ offset(logOdds),family=binomial)$coef
  Alpha.apparent  <- glm.coef.alpha[1]
  if (!is.numeric(Fit.Caret)){
    y_predClass<-predict(Fit.Caret,type="raw",newdata=X.data)
    y_predProb<-predict(Fit.Caret,type="prob",newdata=X.data)
    out.performance  <- list(AUC.apparent,Alpha.apparent,Beta.apparent,y_predClass=y_predClass,y_predProb=y_predProb)
  } else {
    out.performance  <- list(AUC.apparent,Alpha.apparent,Beta.apparent)
  }
  
  
  out.performance
}


#________________________________________________________________________________________________________________________________________________________

# MODEL ###########################################################################################

MissForestLasso <- function (Num.boot=NULL,method,Num.cv=NULL,repeats=NULL,Data.NA,Grid,percent.tol,cores_2_use,seed){
  
  ### Peformimg missForest (outcome excluded)
  
  #set.seed(j*nrow(Data.NA))
  cl <- makeCluster(cores_2_use)
  clusterExport(cl, c("Data.NA","cores_2_use","seed"),envir=environment())
  clusterSetRNGStream(cl, seed)
  clusterEvalQ(cl, library(missForest))
  Imput <- 
    parLapply(cl = cl, X = 1:cores_2_use, fun = function(no){
      missForest(Data.NA[,-1], maxiter = 10, ntree = 100, variablewise = FALSE,
                 verbose = TRUE,
                 replace = TRUE)#, mtry = floor(sqrt(ncol(data))), 
      #parallelize = "no") 
    })
  stopCluster(cl)
  data.imputed=Imput[[1]]$ximp
  
  # Checking the imputation error
  ErrImp<-Imput[[1]]$OOBerror #-----> to save in output
  # NRMSE       PFC 
  
  ### Adding the outcome to the imputed dataset "data.imputed"
  data.imputed[,dim(Data.NA)[2]]<-Data.NA[,1]
  names(data.imputed)[dim(Data.NA)[2]]<-names(Data.NA)[1]
  
  ### Fit glmnet using caret. 
  #modelMatrixData<-model.Matrix(as.formula(paste(colnames(Data.NA)[dim(Data.NA)[2]],"~.",sep="")),Data.NA)
  #coef.best<-coef.tol.1SE<-coef.tol<-coef.tol.15<- double(dim(modelMatrixData)[2])  
  lambda<- double(4)
  names(lambda)<-c("best","tol.1SE","tol","tol.15%")
  
  modelMatrix<-model.Matrix(as.formula(paste(colnames(data.imputed)[dim(data.imputed)[2]],"~.",sep="")),data.imputed) #predictors 
  Outcome   <- unlist(data.imputed[,dim(data.imputed)[2]])    
  names(Outcome)<-NULL
  levels(Outcome) <- c("nonTRS", "TRS")
  
  options(warn=-1)
  
  cl=makeCluster(cores_2_use);registerDoParallel(cl) #parallelizing
  
  ## Make a custom trainControl - use ROC as a model selection criteria instead of accuracy (PPV)
  if (method=="repeatedcv")
  {set.seed(seed)
    cvIndex <- createMultiFolds(Outcome, k=Num.cv, times=repeats)# for repeated startified CV
    Fit.Caret <- train(x=modelMatrix[,-1] ,y=Outcome, method="glmnet",tuneGrid=Grid, family="binomial",            
                       trControl=trainControl(method=method, number=Num.cv,  repeats=repeats, 
                                              index = cvIndex,
                                              selectionFunction="best",
                                              summaryFunction = twoClassSummary, #optimizing with AUC instead of accuracy
                                              classProbs = TRUE ))  #http://rstudio-pubs-static.s3.amazonaws.com/251240_12a8ecea8e144fada41120ddcf52b116.html 
    Fit.Caret1SE <- train(x=modelMatrix[,-1] ,y=Outcome, method="glmnet",tuneGrid=Grid, family="binomial",            
                          trControl=trainControl(method=method, number=Num.cv,  repeats=repeats, 
                                                 index = cvIndex,
                                                 selectionFunction="oneSE",
                                                 summaryFunction = twoClassSummary, #optimizing with AUC instead of accuracy
                                                 classProbs = TRUE ))  
    Fit.CaretTol <- tolerance(Fit.Caret$results, metric = "ROC",
                              tol = 3, maximize = TRUE)
    Fit.Caret15tol <- tolerance(Fit.Caret$results, metric = "ROC",
                                tol = 15, maximize = TRUE)
  } else if (method=="boot") #stratified bootstrap to implement 
  {set.seed(seed)
    Fit.Caret <- train(x=modelMatrix[,-1] ,y=Outcome, method="glmnet",tuneGrid=Grid, family="binomial",             
                       trControl=trainControl(number=Num.boot, method=method, selectionFunction="best",
                                              summaryFunction = twoClassSummary,
                                              classProbs = TRUE))   
    Fit.Caret1SE <- train(x=modelMatrix[,-1] ,y=Outcome, method="glmnet",tuneGrid=Grid, family="binomial",             
                          trControl=trainControl(number=Num.boot, method=method, selectionFunction="oneSE",
                                                 summaryFunction = twoClassSummary,
                                                 classProbs = TRUE))   
    Fit.CaretTol <- tolerance(Fit.Caret$results, metric = "ROC", 
                              tol = 3, maximize = TRUE)  
    Fit.Caret15tol <- tolerance(Fit.Caret$results, metric = "ROC", 
                                tol = 15, maximize = TRUE) 
  }
  stopCluster(cl)
  options(warn=0)
  
  
  #plot(Fit.Caret, metric="Accuracy")
  #plot(Fit.Caret, metric="Kappa")
  
  #- Tuning parameters
  
  lambda[1] <- Fit.Caret$finalModel$lambdaOpt #
  if (method=="boot"){
    lambda[2] <- max(Fit.Caret$results$lambda[max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC <=
                                                (Fit.Caret$results[row.names(Fit.Caret$bestTune),]$ROCSD)/sqrt(Num.boot)])
  } else if (method=="repeatedcv"){
    lambda[2] <- max(Fit.Caret$results$lambda[max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC <=
                                                (Fit.Caret$results[row.names(Fit.Caret$bestTune),]$ROCSD)/sqrt(Num.cv*repeats)])
  }
  lambda[3] <- max(Fit.Caret$results$lambda[(max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC)*100/max(Fit.Caret$results$ROC)<=percent.tol])
  
  lambda[4] <- max(Fit.Caret$results$lambda[(max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC)*100/max(Fit.Caret$results$ROC)<=15])
  
  #- Model coefficients -#
  coef.best <- coef(Fit.Caret$finalModel,s=Fit.Caret$bestTune$lambda)
  coef.tol.1SE <- coef(Fit.Caret$finalModel,s=lambda[2])
  coef.tol<-coef(Fit.Caret$finalModel,s=lambda[3])
  coef.tol.15<-coef(Fit.Caret$finalModel,s=lambda[4])
  
  coef <-cbind(coef.best=coef.best,coef.tol.1SE=coef.tol.1SE,coef.tol=coef.tol,coef.tol.15=coef.tol.15)
  #row.names(coef) <-attributes(coef(Fit.Caret$finalModel))$Dimnames[[1]]
  colnames(coef)<-c("Best","1SE","tol","15tol")
  
  #-- Calculate  AUC and calibration slope (beta)-- #
  
  Model.best = AUC.alpha.beta(coef=coef.best,data.imputed=data.imputed, Fit.Caret =Fit.Caret)
  Model.tol.1SE= AUC.alpha.beta(coef=coef.tol.1SE,data.imputed=data.imputed, Fit.Caret= Fit.Caret1SE)
  Model.tol = AUC.alpha.beta(coef=coef.tol,data.imputed=data.imputed, Fit.Caret= Fit.CaretTol)
  Model.tol.15= AUC.alpha.beta(coef=coef.tol.15,data.imputed=data.imputed, Fit.Caret= Fit.Caret15tol)
  
  ## Calculate AUC and model averaged regression coefficents 
  
  AUC.apparent.best          <- Model.best[[1]]
  AUC.apparent.tol.1SE           <- Model.tol.1SE[[1]]
  AUC.apparent.tol      <- Model.tol[[1]]
  AUC.apparent.tol.15      <- Model.tol.15[[1]]
  Alpha.apparent.best          <- Model.best[[2]]
  Alpha.apparent.tol.1SE          <- Model.tol.1SE[[2]]
  Alpha.apparent.tol         <- Model.tol[[2]]
  Alpha.apparent.tol.15          <- Model.tol.15[[2]]
  Beta.apparent.best  	      <- Model.best[[3]]
  Beta.apparent.tol.1SE	<- Model.tol.1SE[[3]]
  Beta.apparent.tol     <- Model.tol[[3]]
  Beta.apparent.tol.15    <- Model.tol.15[[3]]
  
  y_pred<-data.frame(Model.best[[4]],Model.tol.1SE[[4]])
  y_predProb<-data.frame(Model.best[[5]],Model.tol.1SE[[5]])
  
  Performance<-matrix(c(AUC.apparent.best,AUC.apparent.tol.1SE,AUC.apparent.tol,AUC.apparent.tol.15,
                        Beta.apparent.best,Beta.apparent.tol.1SE,Beta.apparent.tol,Beta.apparent.tol.15,
                        Alpha.apparent.best,Alpha.apparent.tol.1SE,Alpha.apparent.tol,Alpha.apparent.tol.15),3,4,byrow=TRUE)
  row.names(Performance)<-c("Apparent.AUC","Apparent.Beta","Apparent.Alpha")
  colnames(Performance)<-c("best","tol.1SE","tol","tol.15%")
  
  out.temp <- list(Performance=Performance,lambda=lambda,coef=coef,y_pred=y_pred,y_predProb=y_predProb,Fit.Caret=Fit.Caret, 
                   Fit.Caret1SE=Fit.Caret1SE,ErrImp=ErrImp, data.imputed=data.imputed)
  
  out.temp
  
}

################################################################################################
# Prediction model
################################################################################################

(output<-MissForestLasso(Num.boot=NULL,method="repeatedcv",Num.cv=5,repeats=50,Data.NA=data_2_impute_pred,Grid=Grid,percent.tol=3,cores_2_use=3,seed=10))

################################################################################################
# Prediction model coefficients
################################################################################################

output$ErrImp

output$lambda

pdf("plotROC21092020.pdf",width=6,height=4,paper='special') 
plot(output$Fit.Caret, metric="ROC")
dev.off()

summary(output$coef)
output$coef

LASSOregressioncoef21092020 <- as.matrix(output$coef)
write.table(LASSOregressioncoef21092020, "sTable16.PredModApparent.txt", sep="\t")

################################################################################################
# Prediction model apparent performance measures
################################################################################################

output$Performance

# Plot a cross table of observed and predicted
y<- unlist(data_2_impute_pred[,1])  
names(y)<-NULL
levels(y) <- c("nonTRS", "TRS")
table(y,output$y_pred$Model.tol.1SE..4..)
#This shows that the 50% threshold model is classing all participants as NTR

print(postResample(pred=output$y_pred$Model.tol.1SE..4.., obs=y))

# Confusion Matrix and Statistics

ConfMatrix.1<-confusionMatrix(output$y_pred$Model.tol.1SE..4.., y, positive="TRS")
ConfMatrix.1$byClass["Sensitivity"]
ConfMatrix.1$byClass["Specificity"]
ConfMatrix.1$byClass["Pos Pred Value"]
ConfMatrix.1$byClass["Neg Pred Value"]

ConfMatrix.1

################################################################################################
# Prediction model apparent performance - find the best threshold and recalculate performance measures
################################################################################################

# Calculate parameters for ROC curve and AUC

roc1SE<-roc(y,output$y_predProb$TRS.1)
bestThreshold1SE<-pROC::coords(roc1SE, "best", ret = "threshold")
bestThreshold1SE
PredictedClassBest<-factor(output$y_predProb$TRS.1>0.2021) #change threshold
levels(PredictedClassBest)<-c("nonTRS","TRS")

pdf("ROCPlot_1SE_Best_21092020.pdf",width=6,height=4,paper='special') 
plot.roc(roc1SE,print.auc=T,print.thres=T,print.thres.pch=0.1,print.auc.x=0.3,print.auc.y=0.2)
dev.off()

ConfMatrixBest.1<-confusionMatrix(PredictedClassBest, y, positive="TRS")
ConfMatrixBest.1$byClass["Sensitivity"]
ConfMatrixBest.1$byClass["Specificity"]
ConfMatrixBest.1$byClass["Pos Pred Value"]
ConfMatrixBest.1$byClass["Neg Pred Value"]

ConfMatrixBest.1


# 50% Threshold
# 1SE Model: Confusion matrix by classifying to class 1 if Pr(TRS = 1|covariates)>= 0.5
CrossTable(output$y_predProb$TRS.1>0.5,y,format="SPSS",prop.chisq=F,prop.t=F) # really high specificity and very low sensitivity

# Best Threshold: 0.2021 Threshold, best threshold maximising the sum between sensitivity and specificity
CrossTable(output$y_predProb$TRS.1>0.2021,y,format="SPSS",prop.chisq=F,prop.t=F)  #change threshold

# Plot ROC
roc <-roc(y,output$y_predProb$TRS)
plot.roc(roc,print.auc=T,print.thres=T,print.thres.pch=0.1)

plot.roc(roc1SE,print.auc=T,print.thres=T,print.thres.pch=0.1,print.auc.x=0.3,print.auc.y=0.2)

# Comparison of Discriminative Performance ####
roc.test(roc,roc1SE)
plot.roc(roc,print.auc=F,print.thres.pch=0.1)
plot.roc(roc1SE,print.auc=F,print.thres.pch=0.1,col="red",add=T)
legend(.4,.35,c("Best model","1SE model"),fill=c("black","red"))


################################################################################################
# Sensitivity Analyses
################################################################################################

################################################################################################
# Using clozapine only as the definition of TR
################################################################################################

(outputCLOZ<-MissForestLasso(Num.boot=NULL,method="repeatedcv",Num.cv=5,repeats=50,Data.NA=data_2_impute_predCLOZ,Grid=Grid,percent.tol=3,cores_2_use=3,seed=10))

outputCLOZ$ErrImp

outputCLOZ$lambda

outputCLOZ$coef
LASSOregressionCLOZcoef21092020 <- as.matrix(outputCLOZ$coef)
write.table(LASSOregressionCLOZcoef21092020, "sTable17.PredModCLOZApparent.txt", sep="\t")

outputCLOZ$Performance

yCLOZ<- unlist(data_2_impute_predCLOZ[,1])  
names(yCLOZ)<-NULL
levels(yCLOZ) <- c("nonTRS", "TRS")
table(yCLOZ,outputCLOZ$y_pred$Model.tol.1SE..4..)

ConfMatrixCLOZ.1 <- confusionMatrix(outputCLOZ$y_pred$Model.tol.1SE..4.., yCLOZ, positive="TRS")
ConfMatrixCLOZ.1
ConfMatrixCLOZ.1$byClass["Sensitivity"]
ConfMatrixCLOZ.1$byClass["Specificity"]
ConfMatrixCLOZ.1$byClass["Pos Pred Value"]
ConfMatrixCLOZ.1$byClass["Neg Pred Value"]

rocCLOZ1SE<-roc(yCLOZ,outputCLOZ$y_predProb$TRS.1)
bestThresholdCLOZ1SE<-pROC::coords(rocCLOZ1SE, "best", ret = "threshold")
bestThresholdCLOZ1SE
plot.roc(rocCLOZ1SE,print.auc=T,print.thres=T,print.thres.pch=0.1)

PredictedClassBestCLOZ<-factor(outputCLOZ$y_predProb$TRS.1>0.1245) #change threshold
levels(PredictedClassBestCLOZ)<-c("nonTRS","TRS")
ConfMatrixCLOZBEST.1 <- confusionMatrix(PredictedClassBestCLOZ, yCLOZ, positive="TRS")
ConfMatrixCLOZBEST.1
ConfMatrixCLOZBEST.1$byClass["Sensitivity"]
ConfMatrixCLOZBEST.1$byClass["Specificity"]
ConfMatrixCLOZBEST.1$byClass["Pos Pred Value"]
ConfMatrixCLOZBEST.1$byClass["Neg Pred Value"]

################################################################################################
# Using participants with a (follow up) diagnosis of schziophrenia
################################################################################################

(outputSCZ<-MissForestLasso(Num.boot=NULL,method="repeatedcv",Num.cv=5,repeats=50,Data.NA=data_2_impute_predSZ,Grid=Grid,percent.tol=3,cores_2_use=3,seed=10))

outputSCZ$ErrImp

outputSCZ$lambda

outputSCZ$coef
LASSOregressionSCZcoef21092020 <- as.matrix(outputSCZ$coef)
write.table(LASSOregressionSCZcoef21092020, "sTable18.PredModSZApparent.txt", sep="\t")

outputSCZ$Performance

ySCZ<- unlist(data_2_impute_predSZ[,1])  
names(ySCZ)<-NULL
levels(ySCZ) <- c("nonTRS", "TRS")
table(ySCZ,outputSCZ$y_pred$Model.tol.1SE..4..)

ConfMatrixSCZ.1 <- confusionMatrix(outputSCZ$y_pred$Model.tol.1SE..4.., ySCZ, positive="TRS")
ConfMatrixSCZ.1
ConfMatrixSCZ.1$byClass["Sensitivity"]
ConfMatrixSCZ.1$byClass["Specificity"]
ConfMatrixSCZ.1$byClass["Pos Pred Value"]
ConfMatrixSCZ.1$byClass["Neg Pred Value"]

rocSCZ1SE<-roc(ySCZ,outputSCZ$y_predProb$TRS.1)
bestThresholdSCZ1SE<-pROC::coords(rocSCZ1SE, "best", ret = "threshold")
bestThresholdSCZ1SE
plot.roc(rocSCZ1SE,print.auc=T,print.thres=T,print.thres.pch=0.1)

PredictedClassBestSCZ<-factor(outputSCZ$y_predProb$TRS.1>0.1908) #change threshold
levels(PredictedClassBestSCZ)<-c("nonTRS","TRS")
ConfMatrixSCZBEST.1 <- confusionMatrix(PredictedClassBestSCZ, ySCZ, positive="TRS")
ConfMatrixSCZBEST.1
ConfMatrixSCZBEST.1$byClass["Sensitivity"]
ConfMatrixSCZBEST.1$byClass["Specificity"]
ConfMatrixSCZBEST.1$byClass["Pos Pred Value"]
ConfMatrixSCZBEST.1$byClass["Neg Pred Value"]


################################################################################################
# Upsampling TR cases
################################################################################################

table(data_2_impute$strataeitherever,data_2_impute$cohortstrata)
prevalences<-table(data_2_impute$strataeitherever,data_2_impute$cohortstrata)[2,]/table(data_2_impute$strataeitherever,data_2_impute$cohortstrata)[1,]

# Trying up-sampling in each study ##############################################
cohortstrata<-factor(data_2_impute$cohortstrata)
set.seed(9560)

up_trainAESOP <- upSample(x = data_2_impute[cohortstrata=="AESOP London", -1],
                          y = data_2_impute$strataeitherever[cohortstrata=="AESOP London"])
table(up_trainAESOP$Class) 

up_trainBelfast <- upSample(x = data_2_impute[cohortstrata=="Belfast", -1],
                            y = data_2_impute$strataeitherever[cohortstrata=="Belfast"])
table(up_trainBelfast$Class) 

up_trainBologna <- upSample(x = data_2_impute[cohortstrata=="Bologna", -1],
                            y = data_2_impute$strataeitherever[cohortstrata=="Bologna"])
table(up_trainBologna$Class) 

up_trainGAP_London <- upSample(x = data_2_impute[cohortstrata=="GAP London", -1],
                               y = data_2_impute$strataeitherever[cohortstrata=="GAP London"])
table(up_trainGAP_London$Class) 

up_trainIstanbul <- upSample(x = data_2_impute[cohortstrata=="Istanbul", -1],
                             y = data_2_impute$strataeitherever[cohortstrata=="Istanbul"])
table(up_trainIstanbul$Class) 

up_trainLausanne <- upSample(x = data_2_impute[cohortstrata=="Lausanne", -1],
                             y = data_2_impute$strataeitherever[cohortstrata=="Lausanne"])
table(up_trainLausanne$Class) 

up_trainParis <- upSample(x = data_2_impute[cohortstrata=="Paris", -1],
                           y = data_2_impute$strataeitherever[cohortstrata=="Paris"])                         
table(up_trainParis$Class) 

up_trainPrague <- upSample(x = data_2_impute[cohortstrata=="Prague", -1],
                           y = data_2_impute$strataeitherever[cohortstrata=="Prague"])                         
table(up_trainPrague$Class) 

up_trainSantander <- upSample(x = data_2_impute[cohortstrata=="Santander", -1],
                              y = data_2_impute$strataeitherever[cohortstrata=="Santander"])                  
table(up_trainSantander$Class)

up_trainUCL <- upSample(x = data_2_impute[cohortstrata=="UCL London", -1],
                        y = data_2_impute$strataeitherever[cohortstrata=="UCL London"])
table(up_trainUCL$Class) 

data_2_impute_UpSam<-rbind(up_trainAESOP,up_trainBelfast,up_trainBologna,up_trainGAP_London,up_trainIstanbul,up_trainLausanne,up_trainParis,up_trainPrague,up_trainUCL,up_trainSantander)

names(data_2_impute_UpSam)
data_2_impute_UpSam<-data_2_impute_UpSam[,c("Class",setdiff(names(data_2_impute_UpSam),"Class"))]
yUpSam<-data_2_impute_UpSam$Class
levels(yUpSam)<-c("nonTRS","TRS")

summary(data_2_impute_UpSam)
data_2_impute_UpSam <- subset(data_2_impute_UpSam, select = -c(Class,cohortstrata,strataclozeverbinary,diagnosisFUstrata02,lengthfollowupyears) )

(outputUpSam<-MissForestLasso(Num.boot=NULL,method="repeatedcv",Num.cv=5,repeats=50,Data.NA=data_2_impute_UpSam,Grid=Grid,percent.tol=3,cores_2_use=3,seed=10))

outputUpSam$ErrImp

outputUpSam$lambda

outputUpSam$coef

LASSOregressionUpSamcoef21092020 <- as.matrix(outputUpSam$coef)
write.table(LASSOregressionUpSamcoef21092020, "LASSOregressionUpSamcoef21092020.txt", sep="\t")

outputUpSam$Performance

yUpSam2<- unlist(data_2_impute_UpSam[,1])  
names(yUpSam2)<-NULL
levels(yUpSam2) <- c("nonTRS", "TRS")
table(yUpSam2,outputUpSam$y_pred$Model.tol.1SE..4..)

ConfMatrixUpSam.1 <- confusionMatrix(outputUpSam$y_pred$Model.tol.1SE..4.., yUpSam2, positive="TRS")
ConfMatrixUpSam.1
ConfMatrixUpSam.1$byClass["Sensitivity"]
ConfMatrixUpSam.1$byClass["Specificity"]
ConfMatrixUpSam.1$byClass["Pos Pred Value"]
ConfMatrixUpSam.1$byClass["Neg Pred Value"]

rocUpSam1SE<-roc(yUpSam2,outputUpSam$y_predProb$TRS.1)
bestThresholdUpSam1SE<-pROC::coords(rocUpSam1SE, "best", ret = "threshold")
bestThresholdUpSam1SE
plot.roc(rocUpSam1SE,print.auc=T,print.thres=T,print.thres.pch=0.1)

PredictedClassBestUpSam<-factor(outputUpSam$y_predProb$TRS.1>0.5046) #change threshold
levels(PredictedClassBestUpSam)<-c("nonTRS","TRS")
ConfMatrixUpSamBEST.1 <- confusionMatrix(PredictedClassBestUpSam, yUpSam2, positive="TRS")
ConfMatrixUpSamBEST.1
ConfMatrixUpSamBEST.1$byClass["Sensitivity"]
ConfMatrixUpSamBEST.1$byClass["Specificity"]
ConfMatrixUpSamBEST.1$byClass["Pos Pred Value"]
ConfMatrixUpSamBEST.1$byClass["Neg Pred Value"]


##############################################################################
# Save workspace
##############################################################################

save.image("STRATAGData_21092020_PredModel.RData")
