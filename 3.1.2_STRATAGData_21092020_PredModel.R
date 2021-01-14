
#Need to run 3.2.1_STRATAGData_21092020_PredModel.R before this script

# Add our local library to the libpath
.libPaths( c( Sys.getenv("R_LIBS_mpmss1") ) )

# Load libraries 

library(boot)
library(broom)
library(caret)
library(doParallel)
library(dplyr)
library(e1071)
library(ggplot2)
library(glmnet)
library(gmodels)
library(haven)
library(Hmisc)
library(itertools)
library(MatrixModels)
library(mice)
library(missForest)
library(pROC)
library(psfmi)
library(psych)
library(reshape2)
library(sandwich)
library(tableone)
library(tidyr)



##############################################################################
# Read in data
##############################################################################

setwd("/nfshome/store02/users/c.mpmss1/STRATAG")
load("/nfshome/store02/users/c.mpmss1/STRATAG/STRATAGData_21092020_DataPrepped.RData")

##############################################################################
# Cross Validation Across Cohorts
##############################################################################

table(data_2_impute$strataeitherever,data_2_impute$cohortstrata)
prevalences<-table(data_2_impute$strataeitherever,data_2_impute$cohortstrata)[2,]/table(data_2_impute$strataeitherever,data_2_impute$cohortstrata)[1,]

cohortstrata<-factor(data_2_impute$cohortstrata)

#GRID FOR TUNING PARAMETERS
Grid <- expand.grid(alpha = 1 , lambda= 10^seq(-1,-3,length=100))


#FUNCTIONS NEEDED:__________________________________________________________________________________________________________________________________

### A function to compute optimism ################################
AUC.alpha.beta <- function (coef,cohortstrata,Cv.row,data.imputed,f) {
  
  AUC<-rep(NA,length(levels(cohortstrata)))
  Alpha<-rep(NA,length(levels(cohortstrata)))
  Beta<-rep(NA,length(levels(cohortstrata)))
  
  Outcome.data  <- unlist(data.imputed[,dim(data.imputed)[2]])
  levels(Outcome.data) <- c("nonTRS","TRS")
  
  modelMatrix.data <- model.matrix(as.formula(paste(colnames(data.imputed)[dim(data.imputed)[2]],"~.",sep="")),data.imputed)  
  X.data<-modelMatrix.data[,-1]   
  Predicted.data  <-  as.matrix(exp(coef[1] + X.data%*%coef[-1])/(exp(coef[1] + X.data%*%coef[-1])+1))
  Predicted.data[Predicted.data==1]  <- 0.999999999
  Predicted.data[Predicted.data==0]  <- 0.000000001
  
  logOdds.data<-log(Predicted.data/(1-Predicted.data))
  
  i=1
  while(i<=length(levels(cohortstrata))){
    if(length(unique(Outcome.data[cohortstrata[-Cv.row[[f]]]==levels(cohortstrata)[i]]))>1){
      AUC[i]<-as.numeric(roc(Outcome.data[cohortstrata[-Cv.row[[f]]]==levels(cohortstrata)[i]], 
                             as.vector(Predicted.data[cohortstrata[-Cv.row[[f]]]==levels(cohortstrata)[i]]))$auc)
      glm.coef.data       <-  glm(Outcome.data[cohortstrata[-Cv.row[[f]]]==levels(cohortstrata)[i]]~
                                    logOdds.data[cohortstrata[-Cv.row[[f]]]==levels(cohortstrata)[i]],
                                  family=binomial)$coef
      Alpha[i] <-  glm.coef.data[1]
      Beta[i] <-  glm.coef.data[2]
    }
    i<-i+1
    
  }
  
  
  
  out.AUC.Alpha.Beta  <- list(AUC,Alpha,Beta)
  out.AUC.Alpha.Beta 
}


#________________________________________________________________________________________________________________________________________________________

# MAIN FUNCTION ###########################################################################################

RepeatedCVvalidationStudy<- function (valCVfolds,valCVRepeats,method,Num.boot=NULL,Num.cv=NULL,repeats=NULL,Data.NA,Grid,percent.tol,cores_2_use,seed,cohortstrata){
  
  set.seed(seed)
  seeds<-sample(1:10000000,valCVfolds*valCVRepeats)
  set.seed(seed)
  Cv.row <- createMultiFolds(Data.NA[,1],valCVfolds,valCVRepeats) #should create the 1000 folds for the 100 times repeated 10-cv
  # inspect Cv.row as each repetition doesn't necessarily have 10 folds because of stratified sampling ####
  if (valCVRepeats<10){
    numberFolds<-unlist(lapply(sprintf(".Rep%01d", 1:valCVRepeats),function(x) length(grep(x,names(Cv.row)))))
  } else if (valCVRepeats<100){
    numberFolds<-unlist(lapply(sprintf(".Rep%02d", 1:valCVRepeats),function(x) length(grep(x,names(Cv.row)))))
  } else {
    numberFolds<-unlist(lapply(sprintf(".Rep%03d", 1:valCVRepeats),function(x) length(grep(x,names(Cv.row)))))
  }
  N<-sum(numberFolds)
  
  All.AUC.best<-All.AUC.tol1SE<-All.AUC.tolerance<-All.AUC.tol15<-matrix(NA,N,length(levels(cohortstrata)))
  All.Alpha.best<-All.Alpha.tol1SE<-All.Alpha.tolerance<-All.Alpha.tol15<-matrix(NA,N,length(levels(cohortstrata)))
  All.Beta.best<-All.Beta.tol1SE<-All.Beta.tolerance<-All.Beta.tol15<-matrix(NA,N,length(levels(cohortstrata)))
  
  
  f=1 #fold
  while(f<=N){
    print(f)
    
    
    # Imputation of 9 training folds minus fold j in repetition k without the outcome
    cl=makeCluster(cores_2_use);registerDoParallel(cl)
    clusterEvalQ(cl, .libPaths( c( Sys.getenv("R_LIBS_mpmss1") ) ))
    data.imputed.train<-missForest(Data.NA[Cv.row[[f]],-1], maxiter = 10, ntree = 1000,
                                   variablewise = FALSE,
                                   #verbose = TRUE,
                                   replace = TRUE,
                                   parallelize = "forests")$ximp
    # Imputation of the test fold j in repetition k without the outcome
    data.imputed.test<-missForest(Data.NA[-Cv.row[[f]],-1], maxiter = 10, ntree = 1000,
                                  variablewise = FALSE,
                                  #verbose = TRUE,
                                  replace = TRUE,
                                  parallelize = "forests")$ximp
    stopCluster(cl)
    # Adding the outcome to the imputed dataset "data.imputed.train"
    data.imputed.train[,dim(Data.NA)[2]]<-Data.NA[Cv.row[[f]],1]
    names(data.imputed.train)[dim(Data.NA)[2]]<-names(Data.NA)[1]
    # Adding the outcome to the imputed dataset "data.imputed.test"
    data.imputed.test[,dim(Data.NA)[2]]<-Data.NA[-Cv.row[[f]],1]
    names(data.imputed.test)[dim(Data.NA)[2]]<-names(Data.NA)[1]
    
    
    # Defining outcome and predictors 
    Outcome.train<-data.imputed.train[,length(names(data.imputed.train))] #outcome is the last variable now
    modelMatrix.data.train <- model.Matrix(as.formula(paste(colnames(data.imputed.train)[length(names(data.imputed.train))],"~.",sep="")),data.imputed.train)  
    # X.data.train<-modelMatrix.data.train[,-1]  #predictors only (no intercept)
    Outcome.test<-data.imputed.test[,length(names(data.imputed.test))] #outcome is the last variable now
    modelMatrix.data.test <- model.Matrix(as.formula(paste(colnames(data.imputed.test)[length(names(data.imputed.test))],"~.",sep="")),data.imputed.test)  
    # X.data.test<-modelMatrix.data.test[,-1]  #predictors only (no intercept)
    
    levels(Outcome.train) <- c("nonTRS","TRS")
    levels(Outcome.test) <- c("nonTRS","TRS")
    
    options(warn=-1)
    #registerDoMC(cores=cores_2_use)
    cl=makeCluster(cores_2_use);registerDoParallel(cl)
    clusterEvalQ(cl, .libPaths( c( Sys.getenv("R_LIBS_mpmss1") ) ))
    if (method=="repeatedcv")
    {set.seed(seeds[f])
      cvIndex <- createMultiFolds(Outcome.train, k=Num.cv, times=repeats)# for repeated startified CV
      Fit.Caret <- train(x=modelMatrix.data.train[,-1] ,y=Outcome.train, method="glmnet",tuneGrid=Grid, family="binomial",            
                         trControl=trainControl(method=method, number=Num.cv,  repeats=repeats, 
                                                index = cvIndex,
                                                selectionFunction="best",
                                                summaryFunction = twoClassSummary, #optimizing with AUC instead of accuracy
                                                classProbs = TRUE )) 
    } else if(method=="boot")
    {set.seed(seeds[f])
      Fit.Caret <- train(modelMatrix.data.train[,-1] ,Outcome.train, method="glmnet",tuneGrid=Grid, family="binomial",            
                         trControl=trainControl(number=Num.boot, method=method, selectionFunction="best",
                                                summaryFunction = twoClassSummary,
                                                classProbs = TRUE))   
    }
    stopCluster(cl)
    options(warn=0)
    
    #- Model coefficients -#
    
    coef.best <- as.matrix(coef(Fit.Caret$finalModel,s=Fit.Caret$bestTune$lambda))
    if (method=="boot"){
      coef.tol1SE <- as.matrix(coef(Fit.Caret$finalModel,s=max(Fit.Caret$results$lambda[max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC <=
                                                                                          (Fit.Caret$results[row.names(Fit.Caret$bestTune),]$ROCSD)/sqrt(Num.boot)])))
    } else if (method=="repeatedcv") {
      coef.tol1SE <- as.matrix(coef(Fit.Caret$finalModel,s=max(Fit.Caret$results$lambda[max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC <=
                                                                                          (Fit.Caret$results[row.names(Fit.Caret$bestTune),]$ROCSD)/sqrt(repeats)])))
    }
    coef.tolerance <- as.matrix(coef(Fit.Caret$finalModel,s=max(Fit.Caret$results$lambda[(max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC)*100/max(Fit.Caret$results$ROC)<=percent.tol])))
    coef.tol15<- as.matrix(coef(Fit.Caret$finalModel,s=max(Fit.Caret$results$lambda[(max(Fit.Caret$results$ROC)-Fit.Caret$results$ROC)*100/max(Fit.Caret$results$ROC)<=15])))
    
    
    #-- Calculate AUC and calibration slope (beta)-- #
    
    Model.best = AUC.alpha.beta(coef=coef.best,cohortstrata=cohortstrata,Cv.row=Cv.row,f=f,data.imputed=data.imputed.test)
    Model.tol.1SE = AUC.alpha.beta(coef=coef.tol1SE,cohortstrata=cohortstrata,Cv.row=Cv.row,f=f,data.imputed=data.imputed.test)
    Model.tol = AUC.alpha.beta(coef=coef.tolerance,cohortstrata=cohortstrata,Cv.row=Cv.row,f=f,data.imputed=data.imputed.test)
    Model.tol.15 = AUC.alpha.beta(coef=coef.tol15,cohortstrata=cohortstrata,Cv.row=Cv.row,f=f,data.imputed=data.imputed.test)
    
    
    All.AUC.best[f,]    <- Model.best[[1]] 
    All.Alpha.best[f,]       <- Model.best[[2]]
    All.Beta.best[f,]        <- Model.best[[3]]
    All.AUC.tol1SE[f,]       <- Model.tol.1SE[[1]] 
    All.Alpha.tol1SE[f,]          <- Model.tol.1SE[[2]]
    All.Beta.tol1SE[f,]   	       <- Model.tol.1SE[[3]]
    All.AUC.tolerance[f,]  <- Model.tol[[1]]
    All.Alpha.tolerance[f,]	<- Model.tol[[2]]
    All.Beta.tolerance[f,]      <- Model.tol[[3]]
    All.AUC.tol15[f,]  <- Model.tol.15[[1]] 
    All.Alpha.tol15[f,]	<- Model.tol.15[[2]]
    All.Beta.tol15[f,]      <- Model.tol.15[[3]]
    
    
    # #Saving performance
    # 
    # if(f==1){
    #   write(t(cbind(unlist(Model.best),unlist(Model.tol.1SE),unlist(Model.tol),unlist(Model.tol.15))),file = "CVLate.txt",append = FALSE, ncolumns=4)
    # } else {
    #   write(t(cbind(unlist(Model.best),unlist(Model.tol.1SE),unlist(Model.tol),unlist(Model.tol.15))),file = "CVLate.txt",append = TRUE, ncolumns=4)
    # }
    
    f<-f+1
    
    
  } 
  ## Calculate AUC and calibration performances
  
  Av.AUC.best    <- apply(All.AUC.best,2,function(x) mean(x,na.rm = T))
  Av.AUC.tol1SE   <- apply(All.AUC.tol1SE,2,function(x) mean(x,na.rm = T))
  Av.AUC.tolerance  <- apply(All.AUC.tolerance,2,function(x) mean(x,na.rm = T))
  Av.AUC.tol15      <- apply(All.AUC.tol15,2,function(x) mean(x,na.rm = T))
  Av.Beta.best       <- apply(All.Beta.best,2,function(x) mean(x,na.rm = T))
  Av.Alpha.best       <- apply(All.Alpha.best,2,function(x) mean(x,na.rm = T))
  Av.Beta.tol1SE       <- apply(All.Beta.tol1SE,2,function(x) mean(x,na.rm = T))
  Av.Alpha.tol1SE       <- apply(All.Alpha.tol1SE,2,function(x) mean(x,na.rm = T))
  Av.Beta.tolerance      <- apply(All.Beta.tolerance,2,function(x) mean(x,na.rm = T))
  Av.Alpha.tolerance     <- apply(All.Alpha.tolerance,2,function(x) mean(x,na.rm = T))
  Av.Beta.tol15         <- apply(All.Beta.tol15,2,function(x) mean(x,na.rm = T))
  Av.Alpha.tol15      <- apply(All.Alpha.tol15,2,function(x) mean(x,na.rm = T))
  
  
  
  Averages<-array(c(Av.AUC.best,Av.AUC.tol1SE,Av.AUC.tolerance,Av.AUC.tol15,
                    Av.Beta.best,Av.Beta.tol1SE,Av.Beta.tolerance,Av.Beta.tol15,
                    Av.Alpha.best, Av.Alpha.tol1SE,Av.Alpha.tolerance,Av.Alpha.tol15),
                  dim      = c(length(levels(cohortstrata)), 4, 3),
                  dimnames = list(c("AESOP London", "Belfast", "Bologna","GAP London","Istanbul","Lausanne","Paris","Prague","Santander","UCL London" ),
                                  c("Av.best","Av.tol.1SE","Av.tol","Av.tol.15%"),c("Av.AUC","Av.Beta","Av.Alpha")))
  
  
  
  out.temp <- list(Averages=Averages,numberFolds=numberFolds)
  
  out.temp
  
}

#___________________________________________________________________________________________________________________________________________
#try##########################################################################################################################################
#_____________________________________________________________________________________________________________________________________________

(ResultsRepeatedCVvalCOHORTS<-RepeatedCVvalidationStudy(valCVfolds=5,valCVRepeats=50,method="repeatedcv",cohortstrata=cohortstrata,Num.boot=NULL,Num.cv=5,repeats=50,Data.NA=data_2_impute_pred,Grid=Grid,percent.tol=3,cores_2_use=5,seed=777))

ResultsRepeatedCVvalCOHORTS21092020 <- as.data.frame(ResultsRepeatedCVvalCOHORTS$Averages)
#write.table(ResultsRepeatedCVvalCOHORTS21092020, "ResultsRepeatedCVvalCOHORTS21092020.txt", sep="\t")

ResultsRepeatedCVvalCOHORTS$Averages

save.image("/nfshome/store02/users/c.mpmss1/STRATAG/STRATAGData_21092020_PredictionModel_RepeatedCVvalStudy.RData")

#setwd("/Volumes/Imation USB/Sep2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/PredModel")
#load("STRATAGData_21092020_PredictionModel_RepeatedCVvalStudy.RData")
