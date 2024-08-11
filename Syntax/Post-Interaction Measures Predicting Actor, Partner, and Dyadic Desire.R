library(tree); library(randomForest)
library(VSURF)

############## SAMPLE A########################


#####Post-Interaction Relationship Analyses#####

####Male

PostI_malerel_SampleA <- read.csv(file="Level 1 Post-Interaction predicting Male Dyadic Desire, Sample A.csv", header=T)

set.seed(605)
SampleA_malerel_VSURF_post <- VSURF(DiggingRel~., data=PostI_malerel_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_malerel_VSURF_post)
SampleA_malerel_VSURF_post$varselect.thres
SampleA_malerel_VSURF_post$varselect.interp
SampleA_malerel_VSURF_post$varselect.pred

#Sample A male rel model, threshold variables

SampleA_malerel.thresdata <- PostI_malerel_SampleA[c(3,13,5,19,9,21,17,4,31,25,1,29,18,7,22,33,15,6,10,27,2,32,20,28,35,26,14,23,11,30,34,12,8,36,16,24,37)]

set.seed(605)
SampleA_malerel.thresmodel <- randomForest(DiggingRel~., data=SampleA_malerel.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_malerel.thresmodel
importance(SampleA_malerel.thresmodel)
varImpPlot(SampleA_malerel.thresmodel)

#Sample A male rel model, interpretation variables

SampleA_malerel.interpdata <- PostI_malerel_SampleA[c(3,13,5,19,9,21,17,4,31,25,1,29,18,7,22,33,15,6,10,37)]

set.seed(605)
SampleA_malerel.interpmodel <- randomForest(DiggingRel~., data=SampleA_malerel.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_malerel.interpmodel
importance(SampleA_malerel.interpmodel)
varImpPlot(SampleA_malerel.interpmodel)

#Sample A male rel model, prediction variables

SampleA_malerel.preddata <- PostI_malerel_SampleA[c(3,19,21,17,4,37)]

set.seed(605)
SampleA_malerel.predmodel <- randomForest(DiggingRel~., data=SampleA_malerel.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_malerel.predmodel
importance(SampleA_malerel.predmodel)
varImpPlot(SampleA_malerel.predmodel)

######female

PostI_femalerel_SampleA <- read.csv(file="Level 1 Post-Interaction predicting Female Dyadic Desire, Sample A.csv", header=T)

set.seed(605)
SampleA_femalerel_VSURF_post <- VSURF(FDiggingRel~., data=PostI_femalerel_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_femalerel_VSURF_post)
SampleA_femalerel_VSURF_post$varselect.thres
SampleA_femalerel_VSURF_post$varselect.interp
SampleA_femalerel_VSURF_post$varselect.pred

#Sample A female rel model, threshold variables

SampleA_femalerel.thresdata <- PostI_femalerel_SampleA[c(4,14,2,18,20,3,10,22,6,17,29,21,25,26,30,1,9,8,32,7,13,19,33,5,31,11,35,12,34,27,16,24,15,23,36,37)]

set.seed(605)
SampleA_femalerel.thresmodel <- randomForest(FDiggingRel~., data=SampleA_femalerel.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_femalerel.thresmodel
importance(SampleA_femalerel.thresmodel)
varImpPlot(SampleA_femalerel.thresmodel)

#Sample A female rel model, interpretation variables

SampleA_femalerel.interpdata <- PostI_femalerel_SampleA[c(4,14,2,18,20,3,10,22,6,17,29,21,25,26,30,1,9,8,32,37)]

set.seed(605)
SampleA_femalerel.interpmodel <- randomForest(FDiggingRel~., data=SampleA_femalerel.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_femalerel.interpmodel
importance(SampleA_femalerel.interpmodel)
varImpPlot(SampleA_femalerel.interpmodel)

#Sample A female rel model, prediction variables

SampleA_femalerel.preddata <- PostI_femalerel_SampleA[c(4,37)]

set.seed(605)
SampleA_femalerel.predmodel <- randomForest(FDiggingRel~., data=SampleA_femalerel.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_femalerel.predmodel
importance(SampleA_femalerel.predmodel)
varImpPlot(SampleA_femalerel.predmodel)



#####Post-Interaction Actor Analyses#####

PostI_Actor_SampleA <- read.csv(file="Level 2 Post-Interaction predicting Actor, Sample A.csv", header=T)

set.seed(605)
SampleA_Actor_VSURF_post <- VSURF(CombinedDiggingActorGM~., data=PostI_Actor_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_Actor_VSURF_post)
SampleA_Actor_VSURF_post$varselect.thres
SampleA_Actor_VSURF_post$varselect.interp
SampleA_Actor_VSURF_post$varselect.pred

#Sample A actor model, threshold variables

SampleA_ActorGM.thresdata <- PostI_Actor_SampleA[c(8,3,10,2,12,6,11,23,4,25,9,14,22,16,7,5,17,13,36,15,33,24,20,28,31,34,19,29,38)]

set.seed(605)
SampleA_Actor.thresmodel <- randomForest(CombinedDiggingActorGM~., data=SampleA_ActorGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Actor.thresmodel
importance(SampleA_Actor.thresmodel)
varImpPlot(SampleA_Actor.thresmodel)

#Sample A actor model, interpretation variables

SampleA_ActorGM.interpdata <- PostI_Actor_SampleA[c(8,3,10,2,12,6,38)]

set.seed(605)
SampleA_Actor.interpmodel <- randomForest(CombinedDiggingActorGM~., data=SampleA_ActorGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Actor.interpmodel
importance(SampleA_Actor.interpmodel)
varImpPlot(SampleA_Actor.interpmodel)

#Sample A actor model, prediction variables

SampleA_ActorGM.preddata <- PostI_Actor_SampleA[c(8,3,2,38)]

set.seed(605)
SampleA_Actor.predmodel <- randomForest(CombinedDiggingActorGM~., data=SampleA_ActorGM.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Actor.predmodel
importance(SampleA_Actor.predmodel)
varImpPlot(SampleA_Actor.predmodel)




#####Post-Interaction Partner Analyses#####

PostI_Partner_SampleA <- read.csv(file="Level 2 Post-Interaction predicting Partner, Sample A.csv", header=T)

set.seed(605)
SampleA_Partner_VSURF_post <- VSURF(CombinedDiggingPartnerGM~., data=PostI_Partner_SampleA, na.action=na.omit, ntree=5000)
summary(SampleA_Partner_VSURF_post)
SampleA_Partner_VSURF_post$varselect.thres
SampleA_Partner_VSURF_post$varselect.interp
SampleA_Partner_VSURF_post$varselect.pred


#Sample A Partner model, threshold variables

SampleA_PartnerGM.thresdata <- PostI_Partner_SampleA[c(25,23,20,27,29,19,28,22,34,21,31,24,33,35,30,32,36,26,8,18,37,14,16,38)]

set.seed(605)
SampleA_Partner.thresmodel <- randomForest(CombinedDiggingPartnerGM~., data=SampleA_PartnerGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Partner.thresmodel
importance(SampleA_Partner.thresmodel)
varImpPlot(SampleA_Partner.thresmodel)

#Sample A partner model, interpretation variables

SampleA_PartnerGM.interpdata <- PostI_Partner_SampleA[c(25,23,20,38)]

set.seed(605)
SampleA_Partner.interpmodel <- randomForest(CombinedDiggingPartnerGM~., data=SampleA_PartnerGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleA_Partner.interpmodel
importance(SampleA_Partner.interpmodel)
varImpPlot(SampleA_Partner.interpmodel)




############## SAMPLE B########################



#####Post-Interaction Relationship Analyses#####

####Male

PostI_malerel_SampleB <- read.csv(file="Level 1 Post-Interaction predicting Male Dyadic Desire, Sample B.csv", header=T)

set.seed(605)
SampleB_malerel_VSURF_post <- VSURF(DiggingRel~., data=PostI_malerel_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_malerel_VSURF_post)
SampleB_malerel_VSURF_post$varselect.thres
SampleB_malerel_VSURF_post$varselect.interp
SampleB_malerel_VSURF_post$varselect.pred

#Sample B male rel model, threshold variables

SampleB_malerel.thresdata <- PostI_malerel_SampleB[c(3,13,9,39,1,5,31,17,25,4,26,11,19,15,37,6,29,21,33,14,20,28,18,10,36,7,24,22,32,16,35,34,23,27,30,40,2,12,8,38,41)]

set.seed(605)
SampleB_malerel.thresmodel <- randomForest(DiggingRel~., data=SampleB_malerel.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_malerel.thresmodel
importance(SampleB_malerel.thresmodel)
varImpPlot(SampleB_malerel.thresmodel)

#Sample B male rel model, interpretation variables

SampleB_malerel.interpdata <- PostI_malerel_SampleB[c(3,13,9,39,1,5,31,17,25,4,26,11,41)]

set.seed(605)
SampleB_malerel.interpmodel <- randomForest(DiggingRel~., data=SampleB_malerel.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_malerel.interpmodel
importance(SampleB_malerel.interpmodel)
varImpPlot(SampleB_malerel.interpmodel)


#Sample B male rel model, prediction variables

SampleB_malerel.preddata <- PostI_malerel_SampleB[c(3,41)]

set.seed(605)
SampleB_malerel.predmodel <- randomForest(DiggingRel~., data=SampleB_malerel.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleB_malerel.predmodel
importance(SampleB_malerel.predmodel)
varImpPlot(SampleB_malerel.predmodel)



####Female

PostI_femalerel_SampleB <- read.csv(file="Level 1 Post-Interaction predicting Female Dyadic Desire, Sample B.csv", header=T)

set.seed(605)
SampleB_femalerel_VSURF_post <- VSURF(FDiggingRel~., data=PostI_femalerel_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_femalerel_VSURF_post)
SampleB_femalerel_VSURF_post$varselect.thres
SampleB_femalerel_VSURF_post$varselect.interp
SampleB_femalerel_VSURF_post$varselect.pred

#Sample B female rel model, threshold variables

SampleB_femalerel.thresdata <- PostI_femalerel_SampleB[c(4,18,14,6,20,10,22,40,2,38,26,24,12,15,3,36,28,19,1,34,30,13,16,29,8,9,27,17,25,23,39,21,31,32,37,35,5,33,11,7,41)]

set.seed(605)
SampleB_femalerel.thresmodel <- randomForest(FDiggingRel~., data=SampleB_femalerel.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_femalerel.thresmodel
importance(SampleB_femalerel.thresmodel)
varImpPlot(SampleB_femalerel.thresmodel)

#Sample B female rel model, interpretation variables 

SampleB_femalerel.interpdata <- PostI_femalerel_SampleB[c(4,18,14,6,20,10,22,40,2,38,26,24,12,15,3,36,28,19,1,41)]

set.seed(605)
SampleB_femalerel.interpmodel <- randomForest(FDiggingRel~., data=SampleB_femalerel.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_femalerel.interpmodel
importance(SampleB_femalerel.interpmodel)
varImpPlot(SampleB_femalerel.interpmodel)


#Sample B female rel model, prediction variables 

SampleB_femalerel.preddata <- PostI_femalerel_SampleB[c(4,1,41)]

set.seed(605)
SampleB_femalerel.predmodel <- randomForest(FDiggingRel~., data=SampleB_femalerel.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleB_femalerel.predmodel
importance(SampleB_femalerel.predmodel)
varImpPlot(SampleB_femalerel.predmodel)



#####Post-Interaction Actor Analyses#####


PostI_Actor_SampleB <- read.csv(file="Level 2 Post-Interaction predicting Actor, Sample B.csv", header=T)

set.seed(605)
SampleB_Actor_VSURF_post <- VSURF(DiggingActorGM~., data=PostI_Actor_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_Actor_VSURF_post)
SampleB_Actor_VSURF_post$varselect.thres
SampleB_Actor_VSURF_post$varselect.interp
SampleB_Actor_VSURF_post$varselect.pred


#Sample B actor model, threshold variables

SampleB_ActorGM.thresdata <- PostI_Actor_SampleB[c(14,4,18,10,6,22,20,38,16,12,15,26,9,28,32,23,29,19,13,2,40,25,24,11,39,35,34,17,27,36,5,41,7,30,3,33,37,42)]

set.seed(605)
SampleB_Actor.thresmodel <- randomForest(DiggingActorGM~., data=SampleB_ActorGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Actor.thresmodel
importance(SampleB_Actor.thresmodel)
varImpPlot(SampleB_Actor.thresmodel)

#Sample B actor model, interpretation variables

SampleB_ActorGM.interpdata <- PostI_Actor_SampleB[c(14,4,18,10,42)]

set.seed(605)
SampleB_Actor.interpmodel <- randomForest(DiggingActorGM~., data=SampleB_ActorGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Actor.interpmodel
importance(SampleB_Actor.interpmodel)
varImpPlot(SampleB_Actor.interpmodel)


#Sample B actor model, prediction variables

SampleB_ActorGM.preddata <- PostI_Actor_SampleB[c(14,4,10,42)]

set.seed(605)
SampleB_Actor.predmodel <- randomForest(DiggingActorGM~., data=SampleB_ActorGM.preddata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Actor.predmodel
importance(SampleB_Actor.predmodel)
varImpPlot(SampleB_Actor.predmodel)



#####Post-Interaction Partner Analyses#####


PostI_Partner_SampleB <- read.csv(file="Level 2 Post-Interaction predicting Partner, Sample B.csv", header=T)

set.seed(605)
SampleB_Partner_VSURF_post <- VSURF(FDiggingPartnerGM~., data=PostI_Partner_SampleB, na.action=na.omit, ntree=5000)
summary(SampleB_Partner_VSURF_post)
SampleB_Partner_VSURF_post$varselect.thres
SampleB_Partner_VSURF_post$varselect.interp
SampleB_Partner_VSURF_post$varselect.pred


#Sample B Partner model, threshold variables

SampleB_PartnerGM.thresdata <- PostI_Partner_SampleB[c(15,11,5,19,39,41,23,7,21,3,31,33,13,27,35,9,25,37,34,29,36,18,32,28,2,10,26,17,1,42)]

set.seed(605)
SampleB_Partner.thresmodel <- randomForest(FDiggingPartnerGM~., data=SampleB_PartnerGM.thresdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_Partner.thresmodel
importance(SampleB_Partner.thresmodel)
varImpPlot(SampleB_Partner.thresmodel)

#Sample B partner model, interpretation variables

SampleB_partnerGM.interpdata <- PostI_Partner_SampleB[c(15,11,5,42)]

set.seed(605)
SampleB_partner.interpmodel <- randomForest(FDiggingPartnerGM~., data=SampleB_partnerGM.interpdata, importance=T, na.action=na.omit, ntree=5000)
SampleB_partner.interpmodel
importance(SampleB_partner.interpmodel)
varImpPlot(SampleB_partner.interpmodel)


