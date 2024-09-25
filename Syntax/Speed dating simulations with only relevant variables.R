#' ---
#' title: Speed dating simulations with only relevant variables.R
#' subtitle: This script computes results from the **Subsidiary random forests analyses** section in the manuscript and **Table S16** in the Supplemental Material.
#' ---

#Install required packages
require(MASS)
require(plyr)
require(reshape)
require(reshape2)
require(ecodist)
require(Hmisc)
require(randomForest)
require(VSURF)



###########Generating simulated speed dating predictors################

#gen.speeddate function generates speed dating data with 10 sessions, each with 10 men and 10 women, each with 10 traits and 10 preferences.
#Choose the correlation between all traits and prefs, and the mean of the traits and prefs 

gen.speeddate <- function(cor, mean){Sigma <- matrix(cor,20,20)
diag(Sigma) <- 1
allpreds <-mvrnorm(200, mu=matrix(mean, nrow=20, ncol=1), Sigma, empirical=TRUE)
colnames(allpreds) <- c("trait1", "trait2","trait3","trait4","trait5","trait6","trait7","trait8","trait9","trait_error","pref1","pref2","pref3","pref4","pref5","pref6","pref7","pref8","pref9","pref_error")
gender <- as.integer(rep(0:1, each=100))
session<-as.integer(rep(1:10, each=10))
partid<- as.integer(c(1:200))
allpreds <- data.frame(partid, gender, session, allpreds)
count<- as.integer(c(10))
allpreds <- data.frame(count,allpreds)
dupallpreds <- untable(allpreds, count)
dupallpreds <- subset(dupallpreds, select = -c(count))
malepreds <-subset(dupallpreds, gender<=0)
malepreds <- arrange(malepreds, session, partid)
femalepreds <-subset(dupallpreds, gender>=1)
femalepreds <- arrange(femalepreds, session)
orderedpreds <- rbind(femalepreds, malepreds)
pairid <- as.integer(rep(1:1000, times=2))
orderedpreds <- data.frame(pairid, orderedpreds)
melted_allpreds<-melt.data.frame(orderedpreds, id=c("pairid","session","gender"))
cast_preds <- dcast(melted_allpreds, pairid + session ~ gender + variable)
names(cast_preds) <- sub("^1_", "female_", names(cast_preds))
names(cast_preds) <- sub("^0_", "male_", names(cast_preds))
male_relerror.term <- rnorm(1000, mean=0, sd=1)
female_relerror.term <- rnorm(1000, mean=0, sd=1)
cast_preds$male_relerror.term <- cbind(male_relerror.term)
cast_preds$female_relerror.term <- cbind(female_relerror.term)
cast_preds
}


##############Generating simulated speed dating DVs###################

#gen.dvs function adds DVs to dataset with set amounts of variance explained by actor, partner, relationship
#Adds centered versions of DV (actor, partner, and relationship-centered)
#Returns the dataframe with all new variables added

gen.dvs <- function(dataset,actorvar,partnervar,relvar,actorerr, partnererr, relerr){male_y <- with(dataset, actorvar*(male_pref1) + 
          partnervar*(female_trait1) + relvar*(male_pref1 * female_trait1)+ actorerr*(male_pref_error) + partnererr*(female_trait_error) + relerr*(male_relerror.term))
female_y <- with(dataset, actorvar*(female_pref1) + partnervar*(male_trait1) + relvar*(female_pref1 * male_trait1) + actorerr*(female_pref_error) + partnererr*(male_trait_error) + relerr*(female_relerror.term))
dataset$male_y <- cbind(male_y)
dataset$female_y <- cbind(female_y)
dataset <- ddply(dataset, "male_partid", transform, male_actor=mean(male_y))
dataset <- ddply(dataset, "male_partid", transform, male_partner=mean(female_y))
dataset <- ddply(dataset, "female_partid", transform, female_actor=mean(female_y))
dataset <- ddply(dataset, "female_partid", transform, female_partner=mean(male_y))
dataset$male_relDV <- with(dataset, male_y-(male_actor + female_partner))
dataset$female_relDV <- with(dataset, female_y-(female_actor + male_partner))
dataset}


#gen.ap function restructures the dataset to be able to predict actor and partner desire

gen.ap <-function(dataset){male_subset <- subset(dataset, select= c(male_partid:male_pref_error,male_actor,male_partner,male_relerror.term))
female_subset <- subset(dataset, select= c(female_partid:female_pref_error,female_actor,female_partner,female_relerror.term))
male_subset <- male_subset[!duplicated(male_subset$male_partid),]
female_subset <- female_subset[!duplicated(female_subset$female_partid),]
ap_data1 <- male_subset
ap_data2 <- female_subset
names(ap_data1) <- sub("male_", "", names(ap_data1))
names(ap_data2) <- sub("female_", "", names(ap_data2))
ap_data <- rbind(ap_data1,ap_data2)
gender <- as.integer(rep(0:1, each=100))
ap_data <- data.frame(gender, ap_data)
ap_data}



##############Calculate Results for N Datasets###################

#This loop conducts random forest models and regression models for each simulated dataset
#Actor, partner, male relationship, and female relationship desire are predicted from pref, trait, and pref*trait


values.loop <- function(simnum){
  avariance<-data.frame("avariance"=1:simnum)
  aregress<-data.frame("aregress"=1:simnum)
  pvariance<-data.frame("pvariance"=1:simnum)
  pregress<-data.frame("pregress"=1:simnum)
  frelvariance<-data.frame("frelvariance"=1:simnum)
  fregress<-data.frame("fregress"=1:simnum)
  mrelvariance<-data.frame("mrelvariance"=1:simnum)
  mregress<-data.frame("mregress"=1:simnum)
  simulation<-data.frame("simulation"=1:simnum)
  
  for(i in 1:simnum){dataset <- gen.speeddate(0, 0)
  gen.dv <- gen_effects(dataset)
  RF_Fdyadic <- randomForest(female_relDV~female_pref1 + male_trait1, data=gen.dv, importance=T, na.action=na.omit, parallel=TRUE, ntree=5000)
  FVar <- 100*(RF_Fdyadic$rsq[5000])
  RF_Mdyadic <- randomForest(male_relDV~male_pref1 + female_trait1, data=gen.dv, importance=T, na.action=na.omit, parallel=TRUE, ntree=5000)
  MVar <- 100*(RF_Mdyadic$rsq[5000])
  FReg <- summary(lm(female_relDV ~ female_pref1 + male_trait1 + female_pref1*male_trait1, data=gen.dv))$adj.r.squared
  MReg <- summary(lm(male_relDV ~ male_pref1 + female_trait1 + male_pref1*female_trait1, data=gen.dv))$adj.r.squared
  frelvariance[i,] <- FVar
  mrelvariance[i,] <- MVar
  fregress[i,] <- FReg
  mregress[i,] <- MReg
  ap.data <- gen.ap(gen.dv)
  RF_Actor <- randomForest(actor~trait1 + pref1, data=ap.data, importance=T, na.action=na.omit, parallel=TRUE, ntree=5000)
  AVar <- 100*(RF_Actor$rsq[5000])
  RF_Partner <- randomForest(partner~trait1 + pref1, data=ap.data, importance=T, na.action=na.omit, parallel=TRUE, ntree=5000)
  PVar <- 100*(RF_Partner$rsq[5000])
  AReg <- summary(lm(actor ~ trait1 + pref1 + trait1*pref1, data=ap.data))$adj.r.squared
  PReg <- summary(lm(partner ~ trait1 + pref1 + trait1*pref1, data=ap.data))$adj.r.squared
  avariance[i,] <- AVar
  pvariance[i,] <- PVar
  aregress[i,] <- AReg
  pregress[i,] <- PReg
  }
  
  variance_frame <-cbind(simulation,avariance,aregress,pvariance,pregress,frelvariance,fregress,mrelvariance,mregress)
  variance_frame
}


################Simulations for Paper###########################


#All models use 50 speed dating datasets, each with 10 men and 10 women for each of 10 speed dating sessions
#Traits and prefs at a mean of 0 and a correlation of 0
#Set for function: dataset,actorvar,partnervar,relvar,actorerr, partnererr, relerr


gen_effects <- function(dataset){gen.dvs(dataset, .2,.2,.0,.1,.1,.2)}
variance_frame1 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.2,.1,.1,.1,.2)}
variance_frame2 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.2,.2,.1,.1,.2)}
variance_frame3 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.2,.3,.1,.1,.2)}
variance_frame4 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .0,.2,.2,.1,.1,.2)}
variance_frame5 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .1,.2,.2,.1,.1,.2)}
variance_frame6 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.2,.2,.1,.1,.2)}
variance_frame7 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .3,.2,.2,.1,.1,.2)}
variance_frame8 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.0,.2,.1,.1,.2)}
variance_frame9 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.1,.2,.1,.1,.2)}
variance_frame10 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.2,.2,.1,.1,.2)}
variance_frame11 <- values.loop(50)

gen_effects <- function(dataset){gen.dvs(dataset, .2,.3,.2,.1,.1,.2)}
variance_frame12 <- values.loop(50)


modelgroup_means <- rbind((colMeans(variance_frame5)),(colMeans(variance_frame6)),(colMeans(variance_frame7)),(colMeans(variance_frame8)),(colMeans(variance_frame9)),(colMeans(variance_frame10)),(colMeans(variance_frame11)),(colMeans(variance_frame12)),(colMeans(variance_frame1)),(colMeans(variance_frame2)),(colMeans(variance_frame3)),(colMeans(variance_frame4)))


write.csv(modelgroup_means, file="simulated speed data summary all models, Jan 12 2017.csv", row.names=TRUE)







