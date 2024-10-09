#' ---
#' title: Document Errors.R
#' author: Florian Pargent
#' subtitle: This script documents errors I detected during my ERROR review
#' ---

#' ## 1. Missing gender value in testset 
#' The manuscript states that the testset data *"Sample B consisted of 187 undergraduate students (93 women and 94 men; mean age = 19.6 years, SD = 1.2)"*.
#' However, in the two data files *Testing actor.csv* and *Testing partner.csv* (which both contain the predictor variable `Gender`), one gender value is missing.
#' Either the reported descriptive gender statistics in the manuscript are incorrect, or the missing data point in the testing datasets is incorrect.

# load testing data
ActorGM_testset <- read.csv(file="Testing actor.csv", header=T)
PartnerGM_testset <- read.csv(file="Testing partner.csv", header=T)
# compute tables for gender

table(ActorGM_testset$Gender, useNA = "always")
which(is.na(ActorGM_testset$Gender))

table(PartnerGM_testset$Gender, useNA = "always")
which(is.na(PartnerGM_testset$Gender))

#' Note that I was not able to check whether the reported age statistics in the manuscript are inconsistent, because age was not used in any analyses and is thus not included in the publicly shared datasets.
