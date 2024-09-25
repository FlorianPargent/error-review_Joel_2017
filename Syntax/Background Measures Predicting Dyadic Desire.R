#' ---
#' title: Background Measures Predicting Dyadic Desire.R
#' subtitle: This script computes results from the **Random forests results** section and **Table 2** in the manuscript.
#' ---

library(tree); library(randomForest)
library(VSURF)

#######################################################################################
#Dyadic IV Analyses
#######################################################################################

#Random Forest for Male Dyadic Desire, Sample A

Background_MDyadic_SampleA <- read.csv(file="Level 1 predicting Male Dyadic Desire, Sample A.csv", header=T)
set.seed(605)
SampleA_DyadicMale_VSURF <- VSURF(DiggingRel~., data=Background_MDyadic_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_DyadicMale_VSURF)
SampleA_DyadicMale_VSURF$varselect.thres
SampleA_DyadicMale_VSURF$varselect.interp
SampleA_DyadicMale_VSURF$varselect.pred

ncol(Background_MDyadic_SampleA)

#Sample A Male Dyadic Model, no variable selection
set.seed(45)
Background_MDyadic_SampleA.allpredictors <- randomForest(DiggingRel~., data=Background_MDyadic_SampleA, importance=T, na.action=na.omit, ntree=5000)
Background_MDyadic_SampleA.allpredictors
importance(Background_MDyadic_SampleA.allpredictors)
varImpPlot(Background_MDyadic_SampleA.allpredictors)


#Sample A Male Dyadic model, threshold variables
SampleA_DyadicMale.thresdata <- Background_MDyadic_SampleA[c(250,222,17,259,24,240,103,270,235,46,102,312,87,346,251,88,363)]

set.seed(605)
SampleA_DyadicMale.thresmodel <- randomForest(DiggingRel~., data=SampleA_DyadicMale.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_DyadicMale.thresmodel
importance(SampleA_DyadicMale.thresmodel)
varImpPlot(SampleA_DyadicMale.thresmodel)

#Sample A Male Dyadic model, interpretation variables

SampleA_DyadicMale.interpdata <- Background_MDyadic_SampleA[c(250,363)]

set.seed(605)
SampleA_DyadicMale.interpmodel <- randomForest(DiggingRel~., data=SampleA_DyadicMale.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_DyadicMale.interpmodel
importance(SampleA_DyadicMale.interpmodel)
varImpPlot(SampleA_DyadicMale.interpmodel)

#Sample A Male Dyadic model, prediction variables

SampleA_DyadicMale.preddata <- Background_MDyadic_SampleA[c(250,363)]

set.seed(605)
SampleA_DyadicMale.predmodel <- randomForest(DiggingRel~., data=SampleA_DyadicMale.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_DyadicMale.predmodel
importance(SampleA_DyadicMale.predmodel)
varImpPlot(SampleA_DyadicMale.predmodel)



#Random Forest for Male Dyadic Desire, Sample B

Background_MDyadic_SampleB <- read.csv(file="Level 1 predicting Male Dyadic Desire, Sample B.csv", header=T)
Background_MDyadic_SampleB$getoutoffun <- factor(Background_MDyadic_SampleB$getoutoffun)
Background_MDyadic_SampleB$getoutoftry <- factor(Background_MDyadic_SampleB$getoutoftry)
Background_MDyadic_SampleB$getoutofmeet <- factor(Background_MDyadic_SampleB$getoutofmeet)
Background_MDyadic_SampleB$getoutofscience <- factor(Background_MDyadic_SampleB$getoutofscience)
Background_MDyadic_SampleB$getoutofltfind <- factor(Background_MDyadic_SampleB$getoutofltfind)
Background_MDyadic_SampleB$getoutofstfind <- factor(Background_MDyadic_SampleB$getoutofstfind)
Background_MDyadic_SampleB$Fgetoutoffun <- factor(Background_MDyadic_SampleB$Fgetoutoffun)
Background_MDyadic_SampleB$Fgetoutoftry <- factor(Background_MDyadic_SampleB$Fgetoutoftry)
Background_MDyadic_SampleB$Fgetoutofmeet <- factor(Background_MDyadic_SampleB$Fgetoutofmeet)
Background_MDyadic_SampleB$Fgetoutofscience <- factor(Background_MDyadic_SampleB$Fgetoutofscience)
Background_MDyadic_SampleB$Fgetoutofltfind <- factor(Background_MDyadic_SampleB$Fgetoutofltfind)
Background_MDyadic_SampleB$Fgetoutofstfind <- factor(Background_MDyadic_SampleB$Fgetoutofstfind)

set.seed(605)
SampleB_DyadicMale_VSURF <- VSURF(DiggingRel~., data=Background_MDyadic_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_DyadicMale_VSURF)
SampleB_DyadicMale_VSURF$varselect.thres
SampleB_DyadicMale_VSURF$varselect.interp
SampleB_DyadicMale_VSURF$varselect.pred

ncol(Background_MDyadic_SampleB)

#Sample B Male Dyadic Model, no variable selection
set.seed(45)
Background_MDyadic_SampleB.allpredictors <- randomForest(DiggingRel~., data=Background_MDyadic_SampleB, importance=T, na.action=na.omit, ntree=5000)
Background_MDyadic_SampleB.allpredictors
importance(Background_MDyadic_SampleB.allpredictors)
varImpPlot(Background_MDyadic_SampleB.allpredictors)

#Sample B Male Dyadic model, threshold variables

SampleB_DyadicMale.thresdata <- Background_MDyadic_SampleB[c(151,190,181,183,200,159,87,115,202,178,20,83,92,206,214,154,208,194,89,217,156,193,131,79,104,15,133,171,82,163,78,4,167,52,146,67,175,80,140,199,191,
                                                     197,147,212,192,64,184,166,152,215,203,195,223)]

set.seed(605)
SampleB_DyadicMale.thresmodel <- randomForest(DiggingRel~., data=SampleB_DyadicMale.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_DyadicMale.thresmodel
importance(SampleB_DyadicMale.thresmodel)
varImpPlot(SampleB_DyadicMale.thresmodel)

#Sample B Male Dyadic model, interpretation variables

SampleB_DyadicMale.interpdata <- Background_MDyadic_SampleB[c(151,190,223)]

set.seed(605)
SampleB_DyadicMale.interpmodel <- randomForest(DiggingRel~., data=SampleB_DyadicMale.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_DyadicMale.interpmodel
importance(SampleB_DyadicMale.interpmodel)
varImpPlot(SampleB_DyadicMale.interpmodel)

#Sample A Male Dyadic model, prediction variables

SampleB_DyadicMale.preddata <- Background_MDyadic_SampleB[c(151,190,223)]

set.seed(605)
SampleB_DyadicMale.predmodel <- randomForest(DiggingRel~., data=SampleB_DyadicMale.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleB_DyadicMale.predmodel
importance(SampleB_DyadicMale.predmodel)
varImpPlot(SampleB_DyadicMale.predmodel)




#Random Forest for Female Dyadic Desire, Sample A
Background_FDyadic_SampleA <- read.csv(file="Level 1 predicting Female Dyadic Desire, Sample A.csv", header=T)

set.seed(9845)
SampleA_DyadicFemale_VSURF <- VSURF(FDiggingRel~., data=Background_FDyadic_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_DyadicFemale_VSURF)
SampleA_DyadicFemale_VSURF$varselect.thres
SampleA_DyadicFemale_VSURF$varselect.interp
SampleA_DyadicFemale_VSURF$varselect.pred

ncol(Background_FDyadic_SampleA)

#Sample A Female Dyadic Model, no variable selection
set.seed(45)
Background_FDyadic_SampleA.allpredictors <- randomForest(FDiggingRel~., data=Background_FDyadic_SampleA, importance=T, na.action=na.omit, ntree=5000)
Background_FDyadic_SampleA.allpredictors
importance(Background_FDyadic_SampleA.allpredictors)
varImpPlot(Background_FDyadic_SampleA.allpredictors)


#Sample A Female Dyadic model, threshold variables
SampleA_DyadicFemale.thresdata <- Background_FDyadic_SampleA[c(187,189,260,99,77,26,33,208,49,312,9,188,361,28,290,48,285,83,238,180,340,167,224,130,195,237,5,159,55,269,57,52,69,222,169,101,53,132,129,363)]

set.seed(9845)
SampleA_DyadicFemale.thresmodel <- randomForest(FDiggingRel~., data=SampleA_DyadicFemale.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_DyadicFemale.thresmodel
importance(SampleA_DyadicFemale.thresmodel)
varImpPlot(SampleA_DyadicFemale.thresmodel)

#Sample A Female Dyadic model, interpretation variables

SampleA_DyadicFemale.interpdata <- Background_FDyadic_SampleA[c(187,189,260,99,77,26,33,208,49,312,9,188,361,28,290,48,285,83,238,180,363)]

set.seed(9845)
SampleA_DyadicFemale.interpmodel <- randomForest(FDiggingRel~., data=SampleA_DyadicFemale.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_DyadicFemale.interpmodel
importance(SampleA_DyadicFemale.interpmodel)
varImpPlot(SampleA_DyadicFemale.interpmodel)

#Sample A Female Dyadic model, prediction variables

SampleA_DyadicFemale.preddata <- Background_FDyadic_SampleA[c(187,99,285,363)]

set.seed(9845)
SampleA_DyadicFemale.predmodel <- randomForest(FDiggingRel~., data=SampleA_DyadicFemale.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_DyadicFemale.predmodel
importance(SampleA_DyadicFemale.predmodel)
varImpPlot(SampleA_DyadicFemale.predmodel)



#Random Forest for Female Dyadic Desire, Sample B

Background_FDyadic_SampleB <- read.csv(file="Level 1 predicting Female Dyadic Desire, Sample B.csv", header=T)
Background_FDyadic_SampleB$getoutoffun <- factor(Background_FDyadic_SampleB$getoutoffun)
Background_FDyadic_SampleB$getoutoftry <- factor(Background_FDyadic_SampleB$getoutoftry)
Background_FDyadic_SampleB$getoutofmeet <- factor(Background_FDyadic_SampleB$getoutofmeet)
Background_FDyadic_SampleB$getoutofscience <- factor(Background_FDyadic_SampleB$getoutofscience)
Background_FDyadic_SampleB$getoutofltfind <- factor(Background_FDyadic_SampleB$getoutofltfind)
Background_FDyadic_SampleB$getoutofstfind <- factor(Background_FDyadic_SampleB$getoutofstfind)
Background_FDyadic_SampleB$Fgetoutoffun <- factor(Background_FDyadic_SampleB$Fgetoutoffun)
Background_FDyadic_SampleB$Fgetoutoftry <- factor(Background_FDyadic_SampleB$Fgetoutoftry)
Background_FDyadic_SampleB$Fgetoutofmeet <- factor(Background_FDyadic_SampleB$Fgetoutofmeet)
Background_FDyadic_SampleB$Fgetoutofscience <- factor(Background_FDyadic_SampleB$Fgetoutofscience)
Background_FDyadic_SampleB$Fgetoutofltfind <- factor(Background_FDyadic_SampleB$Fgetoutofltfind)
Background_FDyadic_SampleB$Fgetoutofstfind <- factor(Background_FDyadic_SampleB$Fgetoutofstfind)


set.seed(9845)
SampleB_DyadicFemale_VSURF <- VSURF(FDiggingRel~., data=Background_FDyadic_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_DyadicFemale_VSURF)
SampleB_DyadicFemale_VSURF$varselect.thres
SampleB_DyadicFemale_VSURF$varselect.interp
SampleB_DyadicFemale_VSURF$varselect.pred

ncol(Background_FDyadic_SampleB)

#Sample B Female Dyadic Model, no variable selection
set.seed(45)
Background_FDyadic_SampleB.allpredictors <- randomForest(FDiggingRel~., data=Background_FDyadic_SampleB, importance=T, na.action=na.omit, ntree=5000)
Background_FDyadic_SampleB.allpredictors
importance(Background_FDyadic_SampleB.allpredictors)
varImpPlot(Background_FDyadic_SampleB.allpredictors)

#Sample B Female Dyadic model, threshold variables

SampleB_DyadicFemale.thresdata <- Background_FDyadic_SampleB[c(223,193)]

set.seed(9845)
SampleB_DyadicFemale.thresmodel <- randomForest(FDiggingRel~., data=SampleB_DyadicFemale.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_DyadicFemale.thresmodel
importance(SampleB_DyadicFemale.thresmodel)
varImpPlot(SampleB_DyadicFemale.thresmodel)

#Sample B Female Dyadic model, interpretation variables

SampleB_DyadicFemale.interpdata <- Background_FDyadic_SampleB[c(223,193)]

set.seed(9845)
SampleB_DyadicFemale.interpmodel <- randomForest(FDiggingRel~., data=SampleB_DyadicFemale.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_DyadicFemale.interpmodel
importance(SampleB_DyadicFemale.interpmodel)
varImpPlot(SampleB_DyadicFemale.interpmodel)






