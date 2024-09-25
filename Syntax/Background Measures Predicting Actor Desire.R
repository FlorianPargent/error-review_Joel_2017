#' ---
#' title: Background Measures Predicting Actor Desire.R
#' subtitle: This script computes results from the **Random forests results** section and **Table 2** in the manuscript.
#' ---

#Install Required Packages

library(tree); library(randomForest)
library(VSURF)

#####Actor Analyses#####

############## SAMPLE A########################

Background_Actor_SampleA <- read.csv(file="Level 2 predicting ActorGM, Sample A.csv", header=T)

set.seed(605)
SampleA_Actor_VSURF <- VSURF(DiggingActorGM~., data=Background_Actor_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_Actor_VSURF)
SampleA_Actor_VSURF$varselect.thres
SampleA_Actor_VSURF$varselect.interp
SampleA_Actor_VSURF$varselect.pred


#Sample A actor model, no variable selection
set.seed(59)
Background_Actor_SampleA.allpredictors <- randomForest(DiggingActorGM~., data=Background_Actor_SampleA, importance=T, na.action=na.omit, ntree=5000)
Background_Actor_SampleA.allpredictors
importance(Background_Actor_SampleA.allpredictors)
varImpPlot(Background_Actor_SampleA.allpredictors)


#Sample A actor model, threshold variables

SampleA_ActorGM.thresdata <- Background_Actor_SampleA[c(179,29,80,69,3,20,34,116,78,141,73,180,154,41,96,66,159,17,95,149,88,48,40,176,99,120,7,173,1,169,181,91,15,87,160,4,124,77,151,174,74,183)]

set.seed(605)
SampleA_Actor.thresmodel <- randomForest(DiggingActorGM~., data=SampleA_ActorGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Actor.thresmodel
importance(SampleA_Actor.thresmodel)
varImpPlot(SampleA_Actor.thresmodel)


#Sample A actor model, interpretation variables

SampleA_ActorGM.interpdata <- Background_Actor_SampleA[c(179,29,80,69,3,20,34,116,78,141,73,180,154,41,96,66,159,183)]

set.seed(605)
SampleA_Actor.interpmodel <- randomForest(DiggingActorGM~., data=SampleA_ActorGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Actor.interpmodel
importance(SampleA_Actor.interpmodel)
varImpPlot(SampleA_Actor.interpmodel)


#Sample A actor model, prediction variables

SampleA_ActorGM.preddata <- Background_Actor_SampleA[c(179,29,80,69,20,154,41,96,159,183)]

set.seed(605)
SampleA_Actor.predmodel <- randomForest(DiggingActorGM~., data=SampleA_ActorGM.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Actor.predmodel
importance(SampleA_Actor.predmodel)
varImpPlot(SampleA_Actor.predmodel)



############## SAMPLE B########################

Background_Actor_SampleB <- read.csv(file="Level 2 predicting ActorGM, Sample B.csv", header=T)
Background_Actor_SampleB$getoutoffun <- factor(Background_Actor_SampleB$getoutoffun)
Background_Actor_SampleB$getoutoftry <- factor(Background_Actor_SampleB$getoutoftry)
Background_Actor_SampleB$getoutofmeet <- factor(Background_Actor_SampleB$getoutofmeet)
Background_Actor_SampleB$getoutofscience <- factor(Background_Actor_SampleB$getoutofscience)
Background_Actor_SampleB$getoutofltfind <- factor(Background_Actor_SampleB$getoutofltfind)
Background_Actor_SampleB$getoutofstfind <- factor(Background_Actor_SampleB$getoutofstfind)

set.seed(605)
SampleB_Actor_VSURF <- VSURF(DiggingActorGM~., data=Background_Actor_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_Actor_VSURF)
SampleB_Actor_VSURF$varselect.thres
SampleB_Actor_VSURF$varselect.interp
SampleB_Actor_VSURF$varselect.pred

#Sample B actor model, no variable selection

set.seed(59)
Background_Actor_SampleB.allpredictors <- randomForest(DiggingActorGM~., data=Background_Actor_SampleB, importance=T, na.action=na.omit, ntree=5000)
Background_Actor_SampleB.allpredictors
importance(Background_Actor_SampleB.allpredictors)
varImpPlot(Background_Actor_SampleB.allpredictors)

#Sample B actor model, threshold variables

SampleB_ActorGM.thresdata <- Background_Actor_SampleB[c(82,11,94,3,96,107,86,68,91,79,76,75,62,89,95,111,8,72,65,66,67,98,47,37,104,46,61,93,7,83,102,31,90,23,32,100,29,85,81,52,88,64,84,56,113)]

set.seed(605)
SampleB_Actor.thresmodel <- randomForest(DiggingActorGM~., data=SampleB_ActorGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Actor.thresmodel
importance(SampleB_Actor.thresmodel)
varImpPlot(SampleB_Actor.thresmodel)

#Sample B actor model, interpretation variables

SampleB_ActorGM.interpdata <- Background_Actor_SampleB[c(82,11,94,3,96,107,86,68,91,79,76,75,62,89,113)]

set.seed(605)
SampleB_Actor.interpmodel <- randomForest(DiggingActorGM~., data=SampleB_ActorGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Actor.interpmodel
importance(SampleB_Actor.interpmodel)
varImpPlot(SampleB_Actor.interpmodel)

#Sample B actor model, prediction variables

SampleB_ActorGM.preddata <- Background_Actor_SampleB[c(82,94,107,86,68,113)]

set.seed(605)
SampleB_Actor.predmodel <- randomForest(DiggingActorGM~., data=SampleB_ActorGM.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Actor.predmodel
importance(SampleB_Actor.predmodel)
varImpPlot(SampleB_Actor.predmodel)


