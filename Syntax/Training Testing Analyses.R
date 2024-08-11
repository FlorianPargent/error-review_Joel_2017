#Install required packages

library(tree); library(randomForest); library(VSURF)

##########Actor, Grand-Mean-Centred###########

#VSURF analyses on training dataset (Sample A)

ActorGM_trainset <- read.csv(file="Training actor.csv", header=T)

set.seed(741)
ActorGM_trainset_VSURF <- VSURF(DiggingActorGM~., data=ActorGM_trainset, na.action=na.omit, parallel=T, ntree=5000)
summary(ActorGM_trainset_VSURF)
ActorGM_trainset_VSURF$varselect.interp

ncol(ActorGM_trainset)

###Actual value of ActorGM DV in test dataset (Sample B)

ActorGM_testset <- read.csv(file="Testing actor.csv", header=T)
ActorGM.testvalue <- ActorGM_testset$DiggingActorGM


####Actor training/testing 

ActorGM_train.interpdata <- ActorGM_trainset[c(69,36,30,87,65,54,12,59,28,18,88)]

set.seed(605)
ActorGM_train.interpmodel <- randomForest(DiggingActorGM~., data=ActorGM_train.interpdata, importance=T, na.action=na.omit, parallel=T, ntree=5000)
ActorGM_train.interpmodel
importance(ActorGM_train.interpmodel)
varImpPlot(ActorGM_train.interpmodel)

#Predicted value of Actor DV in test dataset, based on interpretation model created with training set
Actor.predictedvalue <- predict(ActorGM_train.interpmodel, newdata=ActorGM_testset)

#Correlated at .19
cor.test(Actor.predictedvalue, ActorGM.testvalue)
plot(Actor.predictedvalue, ActorGM.testvalue)
abline(0,1)

#Test MSE
compare.ActorGM <- data.frame(Actor.predictedvalue, ActorGM.testvalue)
compare.ActorGM <- na.omit(compare.ActorGM)
mean((compare.ActorGM$Actor.predictedvalue- compare.ActorGM$ActorGM.testvalue)^2)


####Partner, Grand-Mean-Centred########

#VSURF analyses on training dataset (Sample A)

PartnerGM_trainset <- read.csv(file="Training partner.csv", header=T)

set.seed(853)
PartnerGM_trainset_VSURF <- VSURF(DiggingPartnerGM~., data=PartnerGM_trainset, na.action=na.omit, parallel=T, ntree=5000)
summary(PartnerGM_trainset_VSURF)
PartnerGM_trainset_VSURF$varselect.interp

ncol(PartnerGM_trainset)

###Actual value of PartnerGM DV in test dataset (Sample B)

PartnerGM_testset <- read.csv(file="Testing partner.csv", header=T)
PartnerGM.testvalue <- PartnerGM_testset$DiggingPartnerGM



####Partner training/testing

PartnerGM_train.interpdata <- PartnerGM_trainset[c(80,8,55,20,3,52,81,60,28,69,49,23,72,88)]

set.seed(605)
PartnerGM_train.interpmodel <- randomForest(DiggingPartnerGM~., data=PartnerGM_train.interpdata, importance=T, na.action=na.omit, parallel=T, ntree=5000)
PartnerGM_train.interpmodel
importance(PartnerGM_train.interpmodel)
varImpPlot(PartnerGM_train.interpmodel)

#Predicted value of Partner DV in test dataset, based on interpretation model created with training set
Partner.predictedvalue <- predict(PartnerGM_train.interpmodel, newdata=PartnerGM_testset)

#Correlated at .26
cor.test(Partner.predictedvalue, PartnerGM.testvalue)
plot(Partner.predictedvalue, PartnerGM.testvalue)
abline(0,1)

#Test MSE
compare.PartnerGM <- data.frame(Partner.predictedvalue, PartnerGM.testvalue)
compare.PartnerGM <- na.omit(compare.PartnerGM)
mean((compare.PartnerGM$Partner.predictedvalue- compare.PartnerGM$PartnerGM.testvalue)^2)



####Dyadic Predicting Male Choice, Grand-Mean-Centred########

#VSURF analyses, Dyadic Male Sample A

male.rel_trainset <- read.csv(file="Training couples, male DV.csv", header=T)

set.seed(741)
male.rel_trainset_VSURF <- VSURF(DiggingRel~., data=male.rel_trainset, na.action=na.omit, parallel=T, ntree=5000)
summary(male.rel_trainset_VSURF)
male.rel_trainset_VSURF$varselect.interp


ncol(male.rel_trainset)

###Actual value of male.rel DV in test dataset (Sample B)

male.rel_testset <- read.csv(file="Testing couples, male DV.csv", header=T)
male.rel.testvalue <- male.rel_testset$DiggingRel


####Male dyadic training/testing

male.rel_train.interpdata <- male.rel_trainset[c(40,173)]

set.seed(605)
male.rel_train.interpmodel <- randomForest(DiggingRel~., data=male.rel_train.interpdata, importance=T, na.action=na.omit, parallel=T, ntree=5000)
male.rel_train.interpmodel
importance(male.rel_train.interpmodel)
varImpPlot(male.rel_train.interpmodel)

#Predicted value of Male Dyadic DV in test dataset, based on interpretation model created with training set
Malerel.predictedvalue <- predict(male.rel_train.interpmodel, newdata=male.rel_testset)

#Correlated at -.06
cor.test(Malerel.predictedvalue, male.rel.testvalue)
plot(Malerel.predictedvalue, male.rel.testvalue)
abline(0,1)

#Test MSE
compare.male.rel <- data.frame(Malerel.predictedvalue, male.rel.testvalue)
compare.male.rel <- na.omit(compare.male.rel)
mean((compare.male.rel$Malerel.predictedvalue- compare.male.rel$male.rel.testvalue)^2)





####Dyadic Predicting Female Choice, Grand-Mean-Centred########

#VSURF analyses, Dyadic Female Sample A

female.rel_trainset <- read.csv(file="Training couples, female DV.csv", header=T)

set.seed(741)
female.rel_trainset_VSURF <- VSURF(FDiggingrel~., data=female.rel_trainset, na.action=na.omit, parallel=T, ntree=5000)
summary(female.rel_trainset_VSURF)
female.rel_trainset_VSURF$varselect.interp


ncol(female.rel_trainset)

###Actual value of female.rel DV in test dataset (Sample B)

female.rel_testset <- read.csv(file="Testing couples, female DV.csv", header=T)
female.rel.testvalue <- female.rel_testset$FDiggingRel


####female dyadic training/testing

female.rel_train.interpdata <- female.rel_trainset[c(171,173)]

set.seed(605)
female.rel_train.interpmodel <- randomForest(FDiggingrel~., data=female.rel_train.interpdata, importance=T, na.action=na.omit, parallel=T, ntree=5000)
female.rel_train.interpmodel
importance(female.rel_train.interpmodel)
varImpPlot(female.rel_train.interpmodel)

#Predicted value of female Dyadic DV in test dataset, based on interpretation model created with training set
Femalerel.predictedvalue <- predict(female.rel_train.interpmodel, newdata=female.rel_testset)

#Correlated at .02
cor.test(Femalerel.predictedvalue, female.rel.testvalue)
plot(Femalerel.predictedvalue, female.rel.testvalue)
abline(0,1)

#Test MSE
compare.female.rel <- data.frame(Femalerel.predictedvalue, female.rel.testvalue)
compare.female.rel <- na.omit(compare.female.rel)
mean((compare.female.rel$Femalerel.predictedvalue- compare.female.rel$female.rel.testvalue)^2)





