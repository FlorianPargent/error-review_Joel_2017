#' ---
#' title: Background Measures Predicting Partner Desire.R
#' subtitle: This script computes results from the **Random forests results** section and **Table 2** in the manuscript.
#' ---

library(tree); library(randomForest)
library(VSURF)

#####Partner Analyses#####

##################Sample A#######################

#Level 2 Predicting Partner Desire

Background_Partner_SampleA <- read.csv(file="Level 2 predicting PartnerGM, Sample A.csv", header=T)
ncol(Background_Partner_SampleA)

set.seed(605)
SampleA_Partner_VSURF <- VSURF(DiggingPartnerGM~., data=Background_Partner_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_Partner_VSURF)
SampleA_Partner_VSURF$varselect.thres
SampleA_Partner_VSURF$varselect.interp
SampleA_Partner_VSURF$varselect.pred

#Sample A Partner model, no variable selection
set.seed(62)
Background_Partner_SampleA.allpredictors<- randomForest(DiggingPartnerGM~., data=Background_Partner_SampleA, importance=T, na.action=na.omit, ntree=5000)
Background_Partner_SampleA.allpredictors
importance(Background_Partner_SampleA.allpredictors)
varImpPlot(Background_Partner_SampleA.allpredictors)

#Sample A Partner model, threshold variables

SampleA_PartnerGM.thresdata <- Background_Partner_SampleA[c(4,46,147,106,6,49,51,33,139,56,73,43,22,16,2,103,100,70,124,131,153,102,130,5,78,62,31,173,81,178,77,72,9,96,172,65,148,125,17,158,150,54,7,34,3,53,175,141,121,164,176,93,165,119,48,123,57,111,20,183)]

set.seed(605)
SampleA_Partner.thresmodel <- randomForest(DiggingPartnerGM~., data=SampleA_PartnerGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Partner.thresmodel
importance(SampleA_Partner.thresmodel)
varImpPlot(SampleA_Partner.thresmodel)

#Sample A Partner model, interpretation variables

SampleA_PartnerGM.interpdata <- Background_Partner_SampleA[c(4,46,147,106,6,49,51,33,139,56,73,43,183)]

set.seed(605)
SampleA_Partner.interpmodel <- randomForest(DiggingPartnerGM~., data=SampleA_PartnerGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Partner.interpmodel
importance(SampleA_Partner.interpmodel)
varImpPlot(SampleA_Partner.interpmodel)

#Sample A Partner model, prediction variables

SampleA_PartnerGM.preddata <- Background_Partner_SampleA[c(4,46,147,106,6,49,33,139,43,183)]

set.seed(605)
SampleA_Partner.predmodel <- randomForest(DiggingPartnerGM~., data=SampleA_PartnerGM.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Partner.predmodel
importance(SampleA_Partner.predmodel)
varImpPlot(SampleA_Partner.predmodel)



##################Sample B#######################

Background_Partner_SampleB <- read.csv(file="Level 2 predicting PartnerGM, Sample B.csv", header=T)
Background_Partner_SampleB$getoutoffun <- factor(Background_Partner_SampleB$getoutoffun)
Background_Partner_SampleB$getoutoftry <- factor(Background_Partner_SampleB$getoutoftry)
Background_Partner_SampleB$getoutofmeet <- factor(Background_Partner_SampleB$getoutofmeet)
Background_Partner_SampleB$getoutofscience <- factor(Background_Partner_SampleB$getoutofscience)
Background_Partner_SampleB$getoutofltfind <- factor(Background_Partner_SampleB$getoutofltfind)
Background_Partner_SampleB$getoutofstfind <- factor(Background_Partner_SampleB$getoutofstfind)

ncol(Background_Partner_SampleB)

set.seed(605)
SampleB_Partner_VSURF <- VSURF(DiggingPartnerGM~., data=Background_Partner_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_Partner_VSURF)
SampleB_Partner_VSURF$varselect.thres
SampleB_Partner_VSURF$varselect.interp
SampleB_Partner_VSURF$varselect.pred

#MSDS II Partner model, no selection variables
set.seed(62)
Background_Partner_SampleB.allpredictors<- randomForest(DiggingPartnerGM~., data=Background_Partner_SampleB, importance=T, na.action=na.omit, ntree=5000)
Background_Partner_SampleB.allpredictors
importance(Background_Partner_SampleB.allpredictors)
varImpPlot(Background_Partner_SampleB.allpredictors)

#Sample B Partner model, threshold variables

SampleB_PartnerGM.thresdata <- Background_Partner_SampleB[c(44,79,77,68,27,81,103,61,30,63,2,72,66,13,60,7,73,67,42,41,84,96,110,47,14,89,83,34,8,65,49,29,56,31,80,23,43,40,88,36,78,105,33,93,39,57,58,113)]

set.seed(605)
SampleB_Partner.thresmodel <- randomForest(DiggingPartnerGM~., data=SampleB_PartnerGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Partner.thresmodel
importance(SampleB_Partner.thresmodel)
varImpPlot(SampleB_Partner.thresmodel)

#Sample B Partner model, interpretation variables

SampleB_PartnerGM.interpdata <- Background_Partner_SampleB[c(44,79,77,68,27,81,103,113)]

set.seed(605)
SampleB_Partner.interpmodel <- randomForest(DiggingPartnerGM~., data=SampleB_PartnerGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Partner.interpmodel
importance(SampleB_Partner.interpmodel)
varImpPlot(SampleB_Partner.interpmodel)

#Sample B Partner model, prediction variables

SampleB_PartnerGM.preddata <- Background_Partner_SampleB[c(44,81,113)]

set.seed(605)
SampleB_Partner.predmodel <- randomForest(DiggingPartnerGM~., data=SampleB_PartnerGM.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Partner.predmodel
importance(SampleB_Partner.predmodel)
varImpPlot(SampleB_Partner.predmodel)


