

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
# REPEATED CROSS VALIDATION OPTIMISM CORRECTION
##############################################################################
#first column is the outcome

#GRID FOR TUNING PARAMETERS
Grid <- expand.grid(alpha = 1 , lambda= 10^seq(-2,-4,length=100))


#FUNCTIONS NEEDED:__________________________________________________________________________________________________________________________________

### A function to compute optimism ################################
optimism.alpha.beta <- function (coef,data.imputed.Boot,data.imputed) {
  
  Outcome.boot  <- unlist(data.imputed.Boot[,dim(data.imputed.Boot)[2]])
  levels(Outcome.boot) <- c("nonTRS","TRS")
  Outcome.data  <- unlist(data.imputed[,dim(data.imputed)[2]])
  levels(Outcome.data) <- c("nonTRS","TRS")
  modelMatrix.boot <- model.Matrix(as.formula(paste(colnames(data.imputed.Boot)[dim(data.imputed.Boot)[2]],"~.",sep="")),data.imputed.Boot)  
  X.boot<- modelMatrix.boot[,-1]  
  modelMatrix.data <- model.Matrix(as.formula(paste(colnames(data.imputed)[dim(data.imputed)[2]],"~.",sep="")),data.imputed)  
  X.data<-modelMatrix.data[,-1]   
  Predicted.data  <-  as.matrix(exp(coef[1] + X.data%*%coef[-1])/(exp(coef[1] + X.data%*%coef[-1])+1))
  Predicted.data[Predicted.data==1]  <- 0.999999999
  Predicted.data[Predicted.data==0]  <- 0.000000001
  Predicted.boot  <-  as.matrix(exp(coef[1] + X.boot%*%coef[-1])/(exp(coef[1] + X.boot%*%coef[-1])+1))
  Predicted.boot[Predicted.boot==1]  <- 0.999999999
  Predicted.boot[Predicted.boot==0]  <- 0.000000001
  roc_obj.data <- roc(Outcome.data, as.vector(Predicted.data))
  bestThreshold.data<-pROC::coords(roc_obj.data, "best", ret = "threshold", transpose = TRUE)
  roc_obj.boot <- roc(Outcome.boot, as.vector(Predicted.boot))
  bestThreshold.boot<-pROC::coords(roc_obj.boot, "best", ret = "threshold", transpose = TRUE)
  PredictedClass.data<-factor(Predicted.data>0.5); levels(PredictedClass.data)<-c("nonTRS","TRS")
  PredictedClass.boot<-factor(Predicted.boot>0.5); levels(PredictedClass.boot)<-c("nonTRS","TRS")
  confMatrix50threshold.data<-confusionMatrix(PredictedClass.data, Outcome.data, positive="TRS")
  confMatrix50threshold.boot<-confusionMatrix(PredictedClass.boot, Outcome.boot, positive="TRS")
  sensitivity.data<-confMatrix50threshold.data$byClass["Sensitivity"]
  specificity.data<-confMatrix50threshold.data$byClass["Specificity"]
  PPV.data<-confMatrix50threshold.data$byClass["Pos Pred Value"]
  NPV.data<-confMatrix50threshold.data$byClass["Neg Pred Value"]
  Accuracy.data<-confMatrix50threshold.data$overall["Accuracy"]
  Kappa.data<-confMatrix50threshold.data$overall["Kappa"]
  sensitivity.boot<-confMatrix50threshold.boot$byClass["Sensitivity"]
  specificity.boot<-confMatrix50threshold.boot$byClass["Specificity"]
  PPV.boot<-confMatrix50threshold.boot$byClass["Pos Pred Value"]
  NPV.boot<-confMatrix50threshold.boot$byClass["Neg Pred Value"]
  Accuracy.boot<-confMatrix50threshold.boot$overall["Accuracy"]
  Kappa.boot<-confMatrix50threshold.boot$overall["Kappa"]
  sensitivity.opt<-sensitivity.boot-sensitivity.data
  specificity.opt<-specificity.boot-specificity.data
  PPV.opt<-PPV.boot-PPV.data
  NPV.opt<-NPV.boot-NPV.data
  Accuracy.opt<-Accuracy.boot-Accuracy.data
  Kappa.opt<-Kappa.boot-Kappa.data
  if(bestThreshold.data[1]==-Inf | bestThreshold.boot[1]==-Inf ){
    sensitivityBest.opt<-NA
    specificityBest.opt<-NA
    PPVBest.opt<-NA
    NPVBest.opt<-NA
    AccuracyBest.opt<-NA
    KappaBest.opt<-NA
    sensitivityBest.data<-NA
    specificityBest.data<-NA
    PPVBest.data<-NA
    NPVBest.data<-NA
    AccuracyBest.data<-NA
    KappaBest.data<-NA
    sensitivityBest.boot<-NA
    specificityBest.boot<-NA
    PPVBest.boot<-NA
    NPVBest.boot<-NA
    AccuracyBest.boot<-NA
    KappaBest.boot<-NA
  } else {
    PredictedClassBest.data<-factor(Predicted.data>bestThreshold.data[1]); levels(PredictedClassBest.data)<-c("nonTRS","TRS")
    PredictedClassBest.boot<-factor(Predicted.boot>bestThreshold.boot[1]); levels(PredictedClassBest.boot)<-c("nonTRS","TRS")
    confMatrixBestthreshold.data<-confusionMatrix(PredictedClassBest.data, Outcome.data, positive="TRS")
    confMatrixBestthreshold.boot<-confusionMatrix(PredictedClassBest.boot, Outcome.boot, positive="TRS")
    sensitivityBest.data<-confMatrixBestthreshold.data$byClass["Sensitivity"]
    specificityBest.data<-confMatrixBestthreshold.data$byClass["Specificity"]
    PPVBest.data<-confMatrixBestthreshold.data$byClass["Pos Pred Value"]
    NPVBest.data<-confMatrixBestthreshold.data$byClass["Neg Pred Value"]
    AccuracyBest.data<-confMatrixBestthreshold.data$overall["Accuracy"]
    KappaBest.data<-confMatrixBestthreshold.data$overall["Kappa"]
    sensitivityBest.boot<-confMatrixBestthreshold.boot$byClass["Sensitivity"]
    specificityBest.boot<-confMatrixBestthreshold.boot$byClass["Specificity"]
    PPVBest.boot<-confMatrixBestthreshold.boot$byClass["Pos Pred Value"]
    NPVBest.boot<-confMatrixBestthreshold.boot$byClass["Neg Pred Value"]
    AccuracyBest.boot<-confMatrixBestthreshold.boot$overall["Accuracy"]
    KappaBest.boot<-confMatrixBestthreshold.boot$overall["Kappa"]
    sensitivityBest.opt<-sensitivityBest.boot-sensitivityBest.data
    specificityBest.opt<-specificityBest.boot-specificityBest.data
    PPVBest.opt<-PPVBest.boot-PPVBest.data
    NPVBest.opt<-NPVBest.boot-NPVBest.data
    AccuracyBest.opt<-AccuracyBest.boot-AccuracyBest.data
    KappaBest.opt<-KappaBest.boot-KappaBest.data
  }
  AUC.data   <-as.numeric(roc_obj.data$auc)
  AUC.boot   <-as.numeric(roc_obj.boot$auc)
  AUC.Optimism    <-  AUC.boot-AUC.data
  logOdds.data<-log(Predicted.data/(1-Predicted.data))
  logOdds.boot<-log(Predicted.boot/(1-Predicted.boot))
  glm.coef.data.alpha       <-  glm(Outcome.data ~ offset(logOdds.data),family=binomial)$coef
  glm.coef.boot.alpha<-    glm(Outcome.boot ~ offset(logOdds.boot),family=binomial)$coef
  glm.coef.data.beta       <-  glm(Outcome.data ~ logOdds.data,family=binomial)$coef
  glm.coef.boot.beta<-    glm(Outcome.boot ~ logOdds.boot,family=binomial)$coef
  Alpha.data  <-  glm.coef.data.alpha[1]
  Beta.data	 <-  glm.coef.data.beta[2]
  Alpha.boot  <-  glm.coef.boot.alpha[1]
  Beta.boot	 <-  glm.coef.boot.beta[2]
  Alpha.optimism  <-  Alpha.boot-Alpha.data
  Beta.optimism	 <-  Beta.boot-Beta.data
  
  
  out.opti.best.tol  <- list(AUC.Optimism,Alpha.optimism,Beta.optimism,sensitivityBest.opt,
                             specificityBest.opt,PPVBest.opt,NPVBest.opt,AccuracyBest.opt,
                             KappaBest.opt,sensitivity.opt,specificity.opt,PPV.opt,NPV.opt,
                             Accuracy.opt,Kappa.opt,
                             AUC.boot,AUC.data,Alpha.boot,Alpha.data,Beta.boot,Beta.data,sensitivityBest.boot,
                             sensitivityBest.data,specificityBest.boot,specificityBest.data,PPVBest.boot,
                             PPVBest.data,NPVBest.boot,NPVBest.data,AccuracyBest.boot,AccuracyBest.data,KappaBest.boot,
                             KappaBest.data,sensitivity.boot,sensitivity.data,
                             specificity.boot,specificity.data,PPV.boot,PPV.data,NPV.boot,NPV.data,
                             Accuracy.boot,Accuracy.data,Kappa.boot,Kappa.data)
  out.opti.best.tol
}


#________________________________________________________________________________________________________________________________________________________

# MAIN FUNCTION ###########################################################################################

RepeatedCVvalidation<- function (valCVfolds,valCVRepeats,method,Num.boot=NULL,Num.cv=NULL,repeats=NULL,Data.NA,Grid,percent.tol,cores_2_use,seed){
  
  set.seed(seed)
  seeds<-sample(1:10000000,valCVfolds*valCVRepeats)
  set.seed(seed)
  Cv.row <- createMultiFolds(Data.NA[,1],valCVfolds,valCVRepeats) #should create the 1000 folds for the 100 times repeated 10-cv
  # inspect Cv.row as each repetition doesn't necessarily have 10 folds because of stratified sampling ####
  if (valCVRepeats<100){
    numberFolds<-unlist(lapply(sprintf(".Rep%02d", 1:valCVRepeats),function(x) length(grep(x,names(Cv.row)))))
  } else {
    numberFolds<-unlist(lapply(sprintf(".Rep%03d", 1:valCVRepeats),function(x) length(grep(x,names(Cv.row)))))
  }
  N<-sum(numberFolds)
  
  All.sensitivityBest.best   <-All.sensitivityBest.tol1SE <-All.sensitivityBest.tolerance <-All.sensitivityBest.tol15<-rep(NA,N)
  All.specificityBest.best   <-All.specificityBest.tol1SE <-All.specificityBest.tolerance <-All.specificityBest.tol15<-rep(NA,N)
  All.PPVBest.best   <-All.PPVBest.tol1SE <-All.PPVBest.tolerance <-All.PPVBest.tol15<-rep(NA,N)
  All.NPVBest.best   <-All.NPVBest.tol1SE <-All.NPVBest.tolerance <-All.NPVBest.tol15<-rep(NA,N)
  All.AccuracyBest.best   <-All.AccuracyBest.tol1SE <-All.AccuracyBest.tolerance <-All.AccuracyBest.tol15<-rep(NA,N)
  All.KappaBest.best   <-All.KappaBest.tol1SE <-All.KappaBest.tolerance <-All.KappaBest.tol15<-rep(NA,N)
  All.sensitivity.best   <-All.sensitivity.tol1SE <-All.sensitivity.tolerance <-All.sensitivity.tol15<-rep(NA,N)
  All.specificity.best   <-All.specificity.tol1SE <-All.specificity.tolerance <-All.specificity.tol15<-rep(NA,N)
  All.PPV.best   <-All.PPV.tol1SE <-All.PPV.tolerance <-All.PPV.tol15<-rep(NA,N)
  All.NPV.best   <-All.NPV.tol1SE <-All.NPV.tolerance <-All.NPV.tol15<-rep(NA,N)
  All.Accuracy.best   <-All.Accuracy.tol1SE <-All.Accuracy.tolerance <-All.Accuracy.tol15<-rep(NA,N)
  All.Kappa.best   <-All.Kappa.tol1SE <-All.Kappa.tolerance <-All.Kappa.tol15<-rep(NA,N)
  All.AUC.Optimism.best   <- All.AUC.Optimism.tol1SE   <-    All.AUC.Optimism.tolerance  <-  All.AUC.Optimism.tol15 <- rep(NA,N)
  All.Beta.best.opt   <- All.Beta.tol1SE.opt   <-    All.Alpha.best.opt  <- All.Alpha.tol1SE.opt  <-  rep(NA,N)
  All.Beta.tolerance.opt  <-    All.Alpha.tolerance.opt       <- All.Beta.tol15.opt  <-    All.Alpha.tol15.opt   <-     rep(NA,N)
  
  f=1 #indexing the fold in numberFolds
  while(f<=N){
    #k=1 # indexing repetition
    #while(k<=100){
    print(f)
    #j=1 # indexing fold in each repetition
    #while(j<=numberFolds[k]){
    
    
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
    
    
    #-- Calculate optimism and calibration slope (beta)-- #
    
    Model.best = optimism.alpha.beta(coef=coef.best,data.imputed.Boot=data.imputed.train,data.imputed=data.imputed.test)
    Model.tol.1SE = optimism.alpha.beta(coef=coef.tol1SE,data.imputed.Boot=data.imputed.train,data.imputed=data.imputed.test)
    Model.tol = optimism.alpha.beta(coef=coef.tolerance,data.imputed.Boot=data.imputed.train,data.imputed=data.imputed.test)
    Model.tol.15 = optimism.alpha.beta(coef=coef.tol15,data.imputed.Boot=data.imputed.train,data.imputed=data.imputed.test)
    
    
    All.AUC.Optimism.best[f]    <- Model.best[[1]] 
    All.Alpha.best.opt[f]       <- Model.best[[2]]
    All.Beta.best.opt[f]        <- Model.best[[3]]
    All.AUC.Optimism.tol1SE[f]       <- Model.tol.1SE[[1]] 
    All.Alpha.tol1SE.opt[f]          <- Model.tol.1SE[[2]]
    All.Beta.tol1SE.opt[f]   	       <- Model.tol.1SE[[3]]
    All.AUC.Optimism.tolerance[f]  <- Model.tol[[1]]
    All.Alpha.tolerance.opt[f]	<- Model.tol[[2]]
    All.Beta.tolerance.opt[f]      <- Model.tol[[3]]
    All.AUC.Optimism.tol15[f]  <- Model.tol.15[[1]] 
    All.Alpha.tol15.opt[f]	<- Model.tol.15[[2]]
    All.Beta.tol15.opt[f]      <- Model.tol.15[[3]]
    
    All.sensitivityBest.best[f]<- Model.best[[4]] 
    All.sensitivityBest.tol1SE[f]<- Model.tol.1SE[[4]]
    All.sensitivityBest.tolerance[f]<-Model.tol[[4]]
    All.sensitivityBest.tol15[f]<- Model.tol.15[[4]]
    
    All.specificityBest.best[f]  <- Model.best[[5]] 
    All.specificityBest.tol1SE[f] <-Model.tol.1SE[[5]]
    All.specificityBest.tolerance[f] <-Model.tol[[5]]
    All.specificityBest.tol15[f] <-Model.tol.15[[5]]
    
    All.PPVBest.best[f]   <- Model.best[[6]] 
    All.PPVBest.tol1SE[f]<-Model.tol.1SE[[6]]
    All.PPVBest.tolerance[f] <-Model.tol[[6]]
    All.PPVBest.tol15[f] <-Model.tol.15[[6]]
    
    All.NPVBest.best[f]  <- Model.best[[7]] 
    All.NPVBest.tol1SE[f] <-Model.tol.1SE[[7]]
    All.NPVBest.tolerance[f] <-Model.tol[[7]]
    All.NPVBest.tol15[f]<-Model.tol.15[[7]]
    
    All.AccuracyBest.best[f]   <- Model.best[[8]] 
    All.AccuracyBest.tol1SE[f]<-Model.tol.1SE[[8]]
    All.AccuracyBest.tolerance[f] <-Model.tol[[8]]
    All.AccuracyBest.tol15[f] <-Model.tol.15[[8]]
    
    All.KappaBest.best[f]  <- Model.best[[9]] 
    All.KappaBest.tol1SE[f] <-Model.tol.1SE[[9]]
    All.KappaBest.tolerance[f] <-Model.tol[[9]]
    All.KappaBest.tol15[f]<-Model.tol.15[[9]]
    
    All.sensitivity.best[f]  <- Model.best[[10]] 
    All.sensitivity.tol1SE[f] <-Model.tol.1SE[[10]]
    All.sensitivity.tolerance[f] <-Model.tol[[10]]
    All.sensitivity.tol15[f]<-Model.tol.15[[10]]
    
    All.specificity.best[f] <- Model.best[[11]] 
    All.specificity.tol1SE[f] <-Model.tol.1SE[[11]]
    All.specificity.tolerance[f] <-Model.tol[[11]]
    All.specificity.tol15[f]<-Model.tol.15[[11]]
    
    All.PPV.best[f]   <- Model.best[[12]] 
    All.PPV.tol1SE[f] <-Model.tol.1SE[[12]]
    All.PPV.tolerance[f] <-Model.tol[[12]]
    All.PPV.tol15[f]<-Model.tol.15[[12]]
    
    All.NPV.best[f]  <- Model.best[[13]] 
    All.NPV.tol1SE[f] <-Model.tol.1SE[[13]]
    All.NPV.tolerance[f] <-Model.tol[[13]]
    All.NPV.tol15[f]<-Model.tol.15[[13]]
    
    All.Accuracy.best[f]  <- Model.best[[14]] 
    All.Accuracy.tol1SE[f] <-Model.tol.1SE[[14]]
    All.Accuracy.tolerance[f] <-Model.tol[[14]]
    All.Accuracy.tol15[f]<-Model.tol.15[[14]]
    
    All.Kappa.best[f]   <- Model.best[[15]] 
    All.Kappa.tol1SE[f] <-Model.tol.1SE[[15]]
    All.Kappa.tolerance[f] <-Model.tol[[15]]
    All.Kappa.tol15[f]<-Model.tol.15[[15]]
    
    #Saving performance
    
    if(f==1){
      write(t(cbind(unlist(Model.best),unlist(Model.tol.1SE),unlist(Model.tol),unlist(Model.tol.15))),file = "CVLate.txt",append = FALSE, ncolumns=4)
    } else {
      write(t(cbind(unlist(Model.best),unlist(Model.tol.1SE),unlist(Model.tol),unlist(Model.tol.15))),file = "CVLate.txt",append = TRUE, ncolumns=4)
    }
    
    f<-f+1
    
    #j<-j+1
    #}
    #k<-k+1
    #}
  } 
  ## Calculate AUC, optimism and model averaged regression coefficents
  
  Av.AUC.Optimism.best    <- mean(All.AUC.Optimism.best, na.rm = T)
  Av.AUC.Optimism.tol1SE           <- mean(All.AUC.Optimism.tol1SE, na.rm = T)
  Av.AUC.Optimism.tolerance      <- mean(All.AUC.Optimism.tolerance, na.rm = T)
  Av.AUC.Optimism.tol15      <- mean(All.AUC.Optimism.tol15, na.rm = T)
  Av.Beta.best.opt        <- mean(All.Beta.best.opt, na.rm = T) 
  Av.Alpha.best.opt        <- mean(All.Alpha.best.opt, na.rm = T)  
  Av.Beta.tol1SE.opt               <- mean(All.Beta.tol1SE.opt, na.rm = T)  
  Av.Alpha.tol1SE.opt             <- mean(All.Alpha.tol1SE.opt, na.rm = T)
  Av.Beta.tolerance.opt          <- mean(All.Beta.tolerance.opt, na.rm = T)  
  Av.Alpha.tolerance.opt        <- mean(All.Alpha.tolerance.opt, na.rm = T)
  Av.Beta.tol15.opt          <- mean(All.Beta.tol15.opt, na.rm = T)  
  Av.Alpha.tol15.opt       <- mean(All.Alpha.tol15.opt, na.rm = T)
  
  Av.sensitivityBest.best.opt  <- mean(All.sensitivity.best,na.rm=T)
  Av.sensitivityBest.tol1SE.opt <-mean(All.sensitivity.tol1SE,na.rm=T)
  Av.sensitivityBest.tolerance.opt <-mean(All.sensitivity.tolerance,na.rm=T)
  Av.sensitivityBest.tol15.opt<-mean(All.sensitivity.tol15,na.rm=T)
  
  Av.specificityBest.best.opt <- mean(All.specificity.best,na.rm=T)
  Av.specificityBest.tol1SE.opt <-mean(All.specificity.tol1SE,na.rm=T)
  Av.specificityBest.tolerance.opt <-mean(All.specificity.tolerance,na.rm=T)
  Av.specificityBest.tol15.opt<-mean(All.specificity.tol15,na.rm=T)
  
  Av.PPVBest.best.opt   <- mean(All.PPVBest.best,na.rm=T)
  Av.PPVBest.tol1SE.opt<-mean(All.PPVBest.tol1SE,na.rm=T)
  Av.PPVBest.tolerance.opt <-mean(All.PPVBest.tolerance,na.rm=T)
  Av.PPVBest.tol15.opt <-mean(All.PPVBest.tol15,na.rm=T)
  
  Av.NPVBest.best.opt  <- mean(All.NPVBest.best,na.rm=T)
  Av.NPVBest.tol1SE.opt <-mean(All.NPVBest.tol1SE,na.rm=T)
  Av.NPVBest.tolerance.opt <-mean(All.NPVBest.tolerance,na.rm=T)
  Av.NPVBest.tol15.opt<-mean(All.NPVBest.tol15,na.rm=T)
  
  Av.AccuracyBest.best.opt   <- mean(All.AccuracyBest.best,na.rm=T)
  Av.AccuracyBest.tol1SE.opt<-mean(All.AccuracyBest.tol1SE,na.rm=T)
  Av.AccuracyBest.tolerance.opt <-mean(All.AccuracyBest.tolerance,na.rm=T)
  Av.AccuracyBest.tol15.opt <-mean(All.AccuracyBest.tol15,na.rm=T)
  
  Av.KappaBest.best.opt  <- mean(All.KappaBest.best,na.rm=T)
  Av.KappaBest.tol1SE.opt <-mean(All.KappaBest.tol1SE,na.rm=T)
  Av.KappaBest.tolerance.opt <-mean(All.KappaBest.tolerance,na.rm=T)
  Av.KappaBest.tol15.opt<-mean(All.KappaBest.tol15,na.rm=T)
  
  Av.sensitivity.best.opt  <- mean(All.sensitivity.best,na.rm=T)
  Av.sensitivity.tol1SE.opt <-mean(All.sensitivity.tol1SE,na.rm=T)
  Av.sensitivity.tolerance.opt <-mean(All.sensitivity.tolerance,na.rm=T)
  Av.sensitivity.tol15.opt<-mean(All.sensitivity.tol15,na.rm=T)
  
  Av.specificity.best.opt <- mean(All.specificity.best,na.rm=T)
  Av.specificity.tol1SE.opt <-mean(All.specificity.tol1SE,na.rm=T)
  Av.specificity.tolerance.opt <-mean(All.specificity.tolerance,na.rm=T)
  Av.specificity.tol15.opt<-mean(All.specificity.tol15,na.rm=T)
  
  Av.PPV.best.opt   <- mean(All.PPV.best,na.rm=T)
  Av.PPV.tol1SE.opt  <-mean(All.PPV.tol1SE,na.rm=T)
  Av.PPV.tolerance.opt  <-mean(All.PPV.tolerance,na.rm=T)
  Av.PPV.tol15.opt <-mean(All.PPV.tol15,na.rm=T)
  
  Av.NPV.best.opt  <- mean(All.NPV.best,na.rm=T)
  Av.NPV.tol1SE.opt  <-mean(All.NPV.tol1SE,na.rm=T)
  Av.NPV.tolerance.opt  <-mean(All.NPV.tolerance,na.rm=T)
  Av.NPV.tol15.opt <-mean(All.NPV.tol15,na.rm=T)
  
  Av.Accuracy.best.opt   <- mean(All.AccuracyBest.best,na.rm=T)
  Av.Accuracy.tol1SE.opt<-mean(All.AccuracyBest.tol1SE,na.rm=T)
  Av.Accuracy.tolerance.opt <-mean(All.AccuracyBest.tolerance,na.rm=T)
  Av.AccuracyBest.tol15.opt <-mean(All.AccuracyBest.tol15,na.rm=T)
  
  Av.Kappa.best.opt  <- mean(All.Kappa.best,na.rm=T)
  Av.Kappa.tol1SE.opt <-mean(All.Kappa.tol1SE,na.rm=T)
  Av.Kappa.tolerance.opt <-mean(All.Kappa.tolerance,na.rm=T)
  Av.Kappa.tol15.opt<-mean(All.Kappa.tol15,na.rm=T)
  
  
  Averages<-matrix(c(Av.AUC.Optimism.best,Av.AUC.Optimism.tol1SE,Av.AUC.Optimism.tolerance,Av.AUC.Optimism.tol15,
                     Av.sensitivityBest.best.opt ,Av.sensitivityBest.tol1SE.opt,Av.sensitivityBest.tolerance.opt ,Av.sensitivityBest.tol15.opt,
                     Av.specificityBest.best.opt   ,Av.specificityBest.tol1SE.opt ,Av.specificityBest.tolerance.opt ,Av.specificityBest.tol15.opt,
                     Av.PPVBest.best.opt   ,Av.PPVBest.tol1SE.opt ,Av.PPVBest.tolerance.opt ,Av.PPVBest.tol15.opt,
                     Av.NPVBest.best.opt   ,Av.NPVBest.tol1SE.opt ,Av.NPVBest.tolerance.opt ,Av.NPVBest.tol15.opt,
                     Av.AccuracyBest.best.opt   ,Av.AccuracyBest.tol1SE.opt ,Av.AccuracyBest.tolerance.opt ,Av.AccuracyBest.tol15.opt,
                     Av.KappaBest.best.opt   ,Av.KappaBest.tol1SE.opt ,Av.KappaBest.tolerance.opt ,Av.KappaBest.tol15.opt,
                     Av.sensitivity.best.opt   ,Av.sensitivity.tol1SE.opt ,Av.sensitivity.tolerance.opt ,Av.sensitivity.tol15.opt,
                     Av.specificity.best.opt   ,Av.specificity.tol1SE.opt ,Av.specificity.tolerance.opt ,Av.specificity.tol15.opt,
                     Av.PPV.best.opt   ,Av.PPV.tol1SE.opt ,Av.PPV.tolerance.opt ,Av.PPV.tol15.opt,
                     Av.NPV.best.opt   ,Av.NPV.tol1SE.opt ,Av.NPV.tolerance.opt ,Av.NPV.tol15.opt,
                     Av.Accuracy.best.opt   ,Av.AccuracyBest.tol1SE.opt ,Av.AccuracyBest.tolerance.opt ,Av.AccuracyBest.tol15.opt,
                     Av.Kappa.best.opt   ,Av.Kappa.tol1SE.opt ,Av.Kappa.tolerance.opt ,Av.Kappa.tol15.opt,
                     Av.Beta.best.opt,Av.Beta.tol1SE.opt,Av.Beta.tolerance.opt,Av.Beta.tol15.opt,
                     Av.Alpha.best.opt, Av.Alpha.tol1SE.opt,Av.Alpha.tolerance.opt,Av.Alpha.tol15.opt),15,4,byrow=TRUE)
  
  colnames(Averages)<-c("Av.best","Av.tol.1SE","Av.tol","Av.tol.15%")
  
  row.names(Averages)<-c("Av.AUC.Optimism","Av.sensBestThresh.Optimism","Av.specBestThresh.Optimism",
                         "Av.PPVBestThresh.Optimism","Av.NPVBestThresh.Optimism",
                         "Av.AccuracyBestThresh.Optimism","Av.KappaBestThresh.Optimism",
                         "Av.sens50Thresh.Optimism","Av.spec50Thresh.Optimism",
                         "Av.PPV50Thresh.Optimism","Av.NPV50Thresh.Optimism",
                         "Av.Accuracy50Thresh.Optimism","Av.Kappa50Thresh.Optimism",
                         "Av.Beta.optimism","Av.Alpha.optimism")
  
  out.temp <- list(Averages=Averages,numberFolds=numberFolds)
  
  out.temp
  
}
#___________________________________________________________________________________________________________________________________________

#_____________________________________________________________________________________________________________________________________________

(ResultsRepeatedCVval<-RepeatedCVvalidation(valCVfolds=5,valCVRepeats=50,method="repeatedcv",Num.boot=NULL,Num.cv=5,repeats=50,Data.NA=data_2_impute_pred,Grid=Grid,percent.tol=3,cores_2_use=4,seed=777))
save(ResultsRepeatedCVval,file="OPTCORR_RepeatedCV.Rda")

(ResultsRepeatedCVvalCLOZ<-RepeatedCVvalidation(valCVfolds=5,valCVRepeats=50,method="repeatedcv",Num.boot=NULL,Num.cv=5,repeats=50,Data.NA=data_2_impute_predCLOZ,Grid=Grid,percent.tol=3,cores_2_use=4,seed=777))
save(ResultsRepeatedCVvalCLOZ,file="OPTCORR_RepeatedCV_CLOZ.Rda")

(ResultsRepeatedCVvalSCZ<-RepeatedCVvalidation(valCVfolds=5,valCVRepeats=50,method="repeatedcv",Num.boot=NULL,Num.cv=5,repeats=50,Data.NA=data_2_impute_predSZ,Grid=Grid,percent.tol=3,cores_2_use=4,seed=777))
save(ResultsRepeatedCVvalSCZ,file="OPTCORR_RepeatedCV_SCZ.Rda")

##############################################################################
# Save workspace
##############################################################################

save.image("/nfshome/store02/users/c.mpmss1/STRATAG/STRATAGData_21092020_PredictionModel_OptCorr.RData")

##############################################################################
# Results
##############################################################################

# setwd("/Volumes/Imation USB/Oct2020/STRATAG/PredictionModelPaper/Analysis/PredictionModels/PredModel")
# 
# load("STRATAGData_21092020_PredModel.RData")
# load("STRATAGData_21092020_PredictionModel_OptCorr.RData")

##############################################################################

# summary(ResultsRepeatedCVval$Averages)
# 
# #Corrected AUC
# output$Performance["Apparent.AUC",]-ResultsRepeatedCVval[[1]]["Av.AUC.Optimism",]
# #Corrected Beta
# (beta_corrected<-output$Performance["Apparent.Beta",]-ResultsRepeatedCVval[[1]]["Av.Beta.optimism",])
# #Corrected Alpha
# (alpha_corrected<-output$Performance["Apparent.Alpha",]-ResultsRepeatedCVval[[1]]["Av.Alpha.optimism",])
# 
# #Corrected Accuracy 50% threshold
# 0.8263-ResultsRepeatedCVval[[1]]["Av.Accuracy50Thresh.Optimism",2]
# #Corrected Sensitivity corresponding to the 50% threshold
# ConfMatrix.1$byClass["Sensitivity"]-ResultsRepeatedCVval[[1]]["Av.sens50Thresh.Optimism",2]
# #Corrected Specificity corresponding to the 50% threshold
# ConfMatrix.1$byClass["Specificity"]-ResultsRepeatedCVval[[1]]["Av.spec50Thresh.Optimism",2]
# #Corrected PPV corresponding to the 50% threshold
# ConfMatrix.1$byClass["Pos Pred Value"]-ResultsRepeatedCVval[[1]]["Av.PPV50Thresh.Optimism",2]
# #Corrected NPV corresponding to the 50% threshold
# ConfMatrix.1$byClass["Neg Pred Value"]-ResultsRepeatedCVval[[1]]["Av.NPV50Thresh.Optimism",2]
# 
# #Corrected Accuracy Best Threshold
# 0.7130-ResultsRepeatedCVval[[1]]["Av.AccuracyBestThresh.Optimism",2]
# #Corrected Sensitivity corresponding to the best threshold
# ConfMatrixBest.1$byClass["Sensitivity"]-ResultsRepeatedCVval[[1]]["Av.sensBestThresh.Optimism",2]
# #Corrected Specificity corresponding to the best threshold
# ConfMatrixBest.1$byClass["Specificity"]-ResultsRepeatedCVval[[1]]["Av.specBestThresh.Optimism",2]
# #Corrected PPV corresponding to the best threshold
# ConfMatrixBest.1$byClass["Pos Pred Value"]-ResultsRepeatedCVval[[1]]["Av.PPVBestThresh.Optimism",2]
# #Corrected NPV corresponding to the best threshold
# ConfMatrixBest.1$byClass["Neg Pred Value"]-ResultsRepeatedCVval[[1]]["Av.NPVBestThresh.Optimism",2]
# 
# #Recalibration of 1SE model: prediction model to be calibrated (mean prediction from model = observed event proportion)
# alpha_corrected[2]+output$coef[,2][output$coef[,2]!=0][1]*beta_corrected[2] #calibrated intercept
# beta_corrected[2] *output$coef[,2][output$coef[,2]!=0][-1] #calibrated coefficients
# LASSOregressioncoef21092020_OPTCORR <- as.matrix(beta_corrected[2] *output$coef[,2][output$coef[,2]!=0][-1])
# write.table(LASSOregressioncoef21092020_OPTCORR, "sTable16.PredModOptCorr.txt", sep="\t")

##############################################################################

# summary(ResultsRepeatedCVvalCLOZ$Averages)
# 
# #Corrected AUC
# outputCLOZ$Performance["Apparent.AUC",]-ResultsRepeatedCVvalCLOZ[[1]]["Av.AUC.Optimism",]
# #Corrected Beta
# (CLOZ_beta_corrected<-outputCLOZ$Performance["Apparent.Beta",]-ResultsRepeatedCVvalCLOZ[[1]]["Av.Beta.optimism",])
# #Corrected Alpha
# (CLOZ_alpha_corrected<-outputCLOZ$Performance["Apparent.Alpha",]-ResultsRepeatedCVvalCLOZ[[1]]["Av.Alpha.optimism",])
# 
# #Corrected Accuracy 50% threshold
# 0.88-ResultsRepeatedCVvalCLOZ[[1]]["Av.Accuracy50Thresh.Optimism",2]
# #Corrected Sensitivity corresponding to the 50% threshold
# ConfMatrixCLOZ.1$byClass["Sensitivity"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.sens50Thresh.Optimism",2]
# #Corrected Specificity corresponding to the 50% threshold
# ConfMatrixCLOZ.1$byClass["Specificity"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.spec50Thresh.Optimism",2]
# #Corrected PPV corresponding to the 50% threshold
# ConfMatrixCLOZ.1$byClass["Pos Pred Value"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.PPV50Thresh.Optimism",2]
# #Corrected NPV corresponding to the 50% threshold
# ConfMatrixCLOZ.1$byClass["Neg Pred Value"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.NPV50Thresh.Optimism",2]
# 
# #Corrected Accuracy Best Threshold
# 0.568-ResultsRepeatedCVvalCLOZ[[1]]["Av.AccuracyBestThresh.Optimism",2]
# #Corrected Sensitivity corresponding to the best threshold
# ConfMatrixCLOZBEST.1$byClass["Sensitivity"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.sensBestThresh.Optimism",2]
# #Corrected Specificity corresponding to the best threshold
# ConfMatrixCLOZBEST.1$byClass["Specificity"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.specBestThresh.Optimism",2]
# #Corrected PPV corresponding to the best threshold
# ConfMatrixCLOZBEST.1$byClass["Pos Pred Value"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.PPVBestThresh.Optimism",2]
# #Corrected NPV corresponding to the best threshold
# ConfMatrixCLOZBEST.1$byClass["Neg Pred Value"]-ResultsRepeatedCVvalCLOZ[[1]]["Av.NPVBestThresh.Optimism",2]
# 
# #Recalibration of 1SE model: prediction model to be calibrated (mean prediction from model = observed event proportion)
# CLOZ_alpha_corrected[2]+outputCLOZ$coef[,2][outputCLOZ$coef[,2]!=0][1]*CLOZ_beta_corrected[2] #calibrated intercept
# CLOZ_beta_corrected[2] *outputCLOZ$coef[,2][outputCLOZ$coef[,2]!=0][-1] #calibrated coefficients
# LASSOregressioncoef21092020_CLOZ_OPTCORR <- as.matrix(CLOZ_beta_corrected[2] *outputCLOZ$coef[,2][outputCLOZ$coef[,2]!=0][-1])
# write.table(LASSOregressioncoef21092020_CLOZ_OPTCORR, "sTable17.PredModClozOptCorr.txt", sep="\t")

##############################################################################

# summary(ResultsRepeatedCVvalSCZ$Averages)
# 
# #Corrected AUC
# outputSCZ$Performance["Apparent.AUC",]-ResultsRepeatedCVvalSCZ[[1]]["Av.AUC.Optimism",]
# #Corrected Beta
# (SCZ_beta_corrected<-outputSCZ$Performance["Apparent.Beta",]-ResultsRepeatedCVvalSCZ[[1]]["Av.Beta.optimism",])
# #Corrected Alpha
# (SCZ_alpha_corrected<-outputSCZ$Performance["Apparent.Alpha",]-ResultsRepeatedCVvalSCZ[[1]]["Av.Alpha.optimism",])
# 
# #Corrected Accuracy 50% threshold
# 0.81-ResultsRepeatedCVvalSCZ[[1]]["Av.Accuracy50Thresh.Optimism",2]
# #Corrected Sensitivity corresponding to the 50% threshold
# ConfMatrixSCZ.1$byClass["Sensitivity"]-ResultsRepeatedCVvalSCZ[[1]]["Av.sens50Thresh.Optimism",2]
# #Corrected Specificity corresponding to the 50% threshold
# ConfMatrixSCZ.1$byClass["Specificity"]-ResultsRepeatedCVvalSCZ[[1]]["Av.spec50Thresh.Optimism",2]
# #Corrected PPV corresponding to the 50% threshold
# ConfMatrixSCZ.1$byClass["Pos Pred Value"]-ResultsRepeatedCVvalSCZ[[1]]["Av.PPV50Thresh.Optimism",2]
# #Corrected NPV corresponding to the 50% threshold
# ConfMatrixSCZ.1$byClass["Neg Pred Value"]-ResultsRepeatedCVvalSCZ[[1]]["Av.NPV50Thresh.Optimism",2]
# 
# #Corrected Accuracy Best Threshold
# 0.552-ResultsRepeatedCVvalSCZ[[1]]["Av.AccuracyBestThresh.Optimism",2]
# #Corrected Sensitivity corresponding to the best threshold
# ConfMatrixSCZBEST.1$byClass["Sensitivity"]-ResultsRepeatedCVvalSCZ[[1]]["Av.sensBestThresh.Optimism",2]
# #Corrected Specificity corresponding to the best threshold
# ConfMatrixSCZBEST.1$byClass["Specificity"]-ResultsRepeatedCVvalSCZ[[1]]["Av.specBestThresh.Optimism",2]
# #Corrected PPV corresponding to the best threshold
# ConfMatrixSCZBEST.1$byClass["Pos Pred Value"]-ResultsRepeatedCVvalSCZ[[1]]["Av.PPVBestThresh.Optimism",2]
# #Corrected NPV corresponding to the best threshold
# ConfMatrixSCZBEST.1$byClass["Neg Pred Value"]-ResultsRepeatedCVvalSCZ[[1]]["Av.NPVBestThresh.Optimism",2]
# 
# #Recalibration of 1SE model: prediction model to be calibrated (mean prediction from model = observed event proportion)
# SCZ_alpha_corrected[2]+outputSCZ$coef[,2][outputSCZ$coef[,2]!=0][1]*SCZ_beta_corrected[2] #calibrated intercept
# SCZ_beta_corrected[2] *outputSCZ$coef[,2][outputSCZ$coef[,2]!=0][-1] #calibrated coefficients
# LASSOregressioncoef21092020_SCZ_OPTCORR <- as.matrix(SCZ_beta_corrected[2] *outputSCZ$coef[,2][outputSCZ$coef[,2]!=0][-1])
# write.table(LASSOregressioncoef21092020_SCZ_OPTCORR, "sTable18.PredModSZOptCorr.txt", sep="\t")

