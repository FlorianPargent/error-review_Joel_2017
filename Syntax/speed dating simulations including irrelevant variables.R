#' ---
#' title: speed dating simulations including irrelevant variables.R
#' subtitle: This script computes results from the **Subsidiary random forests analyses** section in the manuscript and **Table S17** in the Supplemental Material.
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

gen.dvfun <- function(dataset,actorvar,partnervar,relvar,actorerr, partnererr, relerr){male_y <- with(dataset, actorvar*(male_pref1) + 
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
#actor, partner, male relationship, and female relationship desire are predicted from five prefs, traits, and pref*trait interactions
#Four additional traits, prefs, and interactions are included in the models that are not predictive of the DV
#VSURF is used to drop irrelevant predictors (interpretation step is used)


values.loop.complex <- function(simnum){
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
  gen.dv.complex <- gen_effects.comp(dataset)
  RF_Fdyadic_VSURF <- VSURF(female_relDV~female_pref1 + female_pref2 + female_pref3 + female_pref4 + female_pref5 + male_trait1 + male_trait2 + male_trait3 + male_trait4 + male_trait5, data=gen.dv.complex, na.action=na.omit, parallel=TRUE, ntree=5000)
  fdyad_interp <- rbind(RF_Fdyadic_VSURF$varselect.interp)
  fname.preds <- c(female_pref1=1, female_pref2=2, female_pref3=3, female_pref4=4,  female_pref5=5, male_trait1=6, male_trait2=7, male_trait3=8, male_pref4=9, male_pref5=10)
  fdyad_interppreds <- (names(fname.preds)[match(fdyad_interp, fname.preds)])
  fdyad_interppreds <- toString(fdyad_interppreds) 
  fdyad_interppreds <- gsub(",", "+", fdyad_interppreds)
  fdyad_form <- as.formula(paste('female_relDV ~', paste(fdyad_interppreds)))
  RF_Fdyadic <- randomForest(fdyad_form, data=gen.dv.complex, importance=T, na.action=na.omit, ntree=5000)
  FVar <- 100*(RF_Fdyadic$rsq[5000])
  RF_Mdyadic_VSURF <- VSURF(male_relDV~male_pref1 + male_pref2 + male_pref3 + male_pref4 + male_pref5 + female_trait1 + female_trait2 + female_trait3 + female_trait4 + female_trait5, data=gen.dv.complex, na.action=na.omit, parallel=TRUE, ntree=5000)
  mdyad_interp <- rbind(RF_Mdyadic_VSURF$varselect.interp)
  mname.preds <- c(male_pref1=1, male_pref2=2, male_pref3=3, male_pref4=4,  male_pref5=5, female_trait1=6, female_trait2=7, female_trait3=8, female_pref4=9, female_pref5=10)
  mdyad_interppreds <- (names(mname.preds)[match(mdyad_interp, mname.preds)])
  mdyad_interppreds <- toString(mdyad_interppreds) 
  mdyad_interppreds <- gsub(",", "+", mdyad_interppreds)
  mdyad_form <- as.formula(paste('male_relDV ~', paste(mdyad_interppreds)))
  RF_Mdyadic <- randomForest(mdyad_form, data=gen.dv.complex, importance=T, na.action=na.omit, ntree=5000)
  MVar <- 100*(RF_Mdyadic$rsq[5000])
  FReg <- summary(lm(female_relDV ~ female_pref1 + male_trait1 + female_pref1*male_trait1 +
                       female_pref2 + male_trait2 + female_pref2*male_trait2 +
                     female_pref3 + male_trait3 + female_pref3*male_trait3 +
                     female_pref4 + male_trait4 + female_pref4*male_trait4 +
                     female_pref5 + male_trait5 + female_pref5*male_trait5, data=gen.dv.complex))$adj.r.squared
  MReg <- summary(lm(male_relDV ~ male_pref1 + female_trait1 + male_pref1*female_trait1 +
                       male_pref2 + female_trait2 + male_pref2*female_trait2 +
                       male_pref3 + female_trait3 + male_pref3*female_trait3 +
                       male_pref4 + female_trait4 + male_pref4*female_trait4 +
                       male_pref5 + female_trait5 + male_pref5*female_trait5, data=gen.dv.complex))$adj.r.squared
  frelvariance[i,] <- FVar
  mrelvariance[i,] <- MVar
  fregress[i,] <- FReg
  mregress[i,] <- MReg
  ap.data <- gen.ap(gen.dv.complex)
  RF_Actor_VSURF <- VSURF(actor~trait1 + pref1 + trait2 + pref2 + trait3 + pref3 + trait4 + pref4 + trait5 + pref5, data=ap.data, na.action=na.omit, parallel=TRUE, ntree=5000)
  actor_interp <- rbind(RF_Actor_VSURF$varselect.interp)
  aname.preds <- c(trait1=1, pref1=2, trait2=3, pref2=4, trait3=5, pref3=6, trait4=7, pref4=8, trait5=9, pref5=10)
  actor_interppreds <- (names(aname.preds)[match(actor_interp, aname.preds)])
  actor_interppreds <- toString(actor_interppreds) 
  actor_interppreds <- gsub(",", "+", actor_interppreds)
  actor_form <- as.formula(paste('actor ~', paste(actor_interppreds)))
  RF_Actor <- randomForest(actor_form, data=ap.data, importance=T, na.action=na.omit, ntree=5000)
  AVar <- 100*(RF_Actor$rsq[5000])
  RF_Partner_VSURF <- VSURF(partner~trait1 + pref1 + trait2 + pref2 + trait3 + pref3 + trait4 + pref4 + trait5 + pref5, data=ap.data, na.action=na.omit, parallel=TRUE, ntree=5000)
  partner_interp <- rbind(RF_Partner_VSURF$varselect.interp)
  pname.preds <- c(trait1=1, pref1=2, trait2=3, pref2=4, trait3=5, pref3=6, trait4=7, pref4=8, trait5=9, pref5=10)
  partner_interppreds <- (names(pname.preds)[match(partner_interp, pname.preds)])
  partner_interppreds <- toString(partner_interppreds) 
  partner_interppreds <- gsub(",", "+", partner_interppreds)
  partner_form <- as.formula(paste('partner ~', paste(partner_interppreds)))
  RF_Partner <- randomForest(partner_form, data=ap.data, importance=T, na.action=na.omit, ntree=5000)
  PVar <- 100*(RF_Partner$rsq[5000])
  AReg <- summary(lm(actor ~ trait1 + pref1 + trait1*pref1 + trait2 + pref2 + trait2*pref2 + trait3 + pref3 + trait3*pref3 + trait4 + pref4 + trait4*pref4 + trait5 + pref5 + trait5*pref5, data=ap.data))$adj.r.squared
  PReg <- summary(lm(partner ~ trait1 + pref1 + trait1*pref1 + trait2 + pref2 + trait2*pref2 + trait3 + pref3 + trait3*pref3 + trait4 + pref4 + trait4*pref4 + trait5 + pref5 + trait5*pref5, data=ap.data))$adj.r.squared
  avariance[i,] <- AVar
  pvariance[i,] <- PVar
  aregress[i,] <- AReg
  pregress[i,] <- PReg
  }
  
  variance_frame <-cbind(simulation,avariance,aregress,pvariance,pregress,frelvariance,fregress,mrelvariance,mregress)
  variance_frame
}


################Simulations for Paper###########################


#All models use 20 speed dating datasets, each with 10 men and 10 women for each of 10 speed dating sessions
#Traits and prefs at a mean of 0 and a correlation of 0


gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.2,.0,.1,.1,.2)}
variance_frame1.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.2,.1,.1,.1,.2)}
variance_frame2.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.2,.2,.1,.1,.2)}
variance_frame3.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.2,.3,.1,.1,.2)}
variance_frame4.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .0,.2,.2,.1,.1,.2)}
variance_frame5.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .1,.2,.2,.1,.1,.2)}
variance_frame6.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.2,.2,.1,.1,.2)}
variance_frame7.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .3,.2,.2,.1,.1,.2)}
variance_frame8.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.0,.2,.1,.1,.2)}
variance_frame9.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.1,.2,.1,.1,.2)}
variance_frame10.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.2,.2,.1,.1,.2)}
variance_frame11.comp <- values.loop.complex(20)

gen_effects.comp <- function(dataset){gen.dvfun(dataset, .2,.3,.2,.1,.1,.2)}
variance_frame12.comp <- values.loop.complex(20)


modelgroup_means.complex <- rbind((colMeans(variance_frame5.comp)),(colMeans(variance_frame6.comp)),(colMeans(variance_frame7.comp)),(colMeans(variance_frame8.comp)),(colMeans(variance_frame9.comp)),(colMeans(variance_frame10.comp)),(colMeans(variance_frame11.comp)),(colMeans(variance_frame12.comp)),(colMeans(variance_frame1.comp)),(colMeans(variance_frame2.comp)),(colMeans(variance_frame3.comp)),(colMeans(variance_frame4.comp)))


write.csv(modelgroup_means.complex, file="simulated speed data summary all complex models, January 14 2017.csv", row.names=TRUE)








