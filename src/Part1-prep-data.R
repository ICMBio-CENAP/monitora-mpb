#
## Amazon mammal community structure
## Elildo Carvalho Jr @ ICMBio/CENAP 2020-07-12
#
#
## Part1 - prepare data for the HMSC analysis
# Code to create the Y matrix required for HMSC
## written by Elildo Carvalho Jr @ ICMBio/CENAP
## benefited from a script written by Jorge Ahumada @ Conservation International
#

## ---- load libraries --------
library(TeachingDemos)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(dplyr)
library(here)


## ---- Source this file --------
source(here("bin", "ahumada_codes.R"))


## ---- Load data -------
# note: you must have a local copy of dataCENAP in your computer or you can create it using join-datasets.R in the "bin" directory
# dataCENAP is never uploaded in the repo because it was included in gitignore

# Warning: you can jump to line 55 to save time!!!
dataCENAP <- read.csv(here("data", "dataCENAP.csv"))

# some fixes
dataCENAP$Camera.Trap.Name <- as.factor(dataCENAP$Camera.Trap.Name)
dataCENAP$Camera.Start.Date <- as.Date(dataCENAP$Camera.Start.Date)
dataCENAP$Camera.End.Date <- as.Date(dataCENAP$Camera.End.Date)
dataCENAP$Photo.Date <- as.Date(dataCENAP$Photo.Date)
dataCENAP$td.photo <- as.POSIXct(paste(dataCENAP$Photo.Date, dataCENAP$Photo.time, sep=" "))
dataCENAP$Order <- gsub("ciNGULATA", "CINGULATA", dataCENAP$Order)


## ----Filter independent records-------
# Group by events that are 60 minutes apart (and from different species)
dataCENAP <- f.separate.events(dataCENAP, 60)
# filter retaining only independent records (distinctive combinations of bin and grp)
dataCENAP2 <- distinct(dataCENAP, bin, grp, .keep_all = TRUE)

# work only with mammal photos
mammals <- filter(dataCENAP2, Class == "MAMMALIA") # keeping only species in species.list

# save as csv to save time
write.csv(mammals, here("data", "mammals.csv"), row.names = FALSE)

# save as rds so it can be uploaded (csv is not read because of gitignore)
saveRDS(mammals, here("data", "mammals.rds"))

# start again from this point
mammals <- readRDS(here("data", "mammals.rds"))
mammals$Camera.Trap.Name <- as.factor(mammals$Camera.Trap.Name)
mammals$Camera.Start.Date <- as.Date(mammals$Camera.Start.Date)
mammals$Camera.End.Date <- as.Date(mammals$Camera.End.Date)
mammals$Photo.Date <- as.Date(mammals$Photo.Date)
mammals$td.photo <- as.POSIXct(paste(mammals$Photo.Date, mammals$Photo.time, sep=" "))
mammals <- mammals[order(mammals$Camera.Trap.Name),] # order by Camera.Trap.Name


## ----Subset the data-----------------

table(mammals$bin) # checking number of records per species
rowSums(table(mammals$bin, by=mammals$Camera.Trap.Name) != 0) # number of sites where each species was recorded
unique(mammals$bin) # a list of recorded species to create species.list

## ----For HMSC, lets work with mammals at the Genus level -----------------

# Select genera amenable to camera trapping (i.e., terrestrial > 0.5 kg)
genera <- sort(unique(mammals$Genus))
generaToUse <- c("Atelocynus", "Cabassous", "Cerdocyon", "Cuniculus", "Dasyprocta",
               "Dasypus", "Didelphis", "Eira", "Euphractus", "Galictis", "Hydrochoerus",
               "Leopardus", "Mazama", "Myrmecophaga", "Nasua", "Nasuella", "Panthera", "Pecari", 
               "Priodontes", "Procyon", "Puma", "Speothos", "Sylvilagus", "Tamandua",
               "Tapirus", "Tayassu")
mammals <- filter(mammals, Genus %in% generaToUse) # keeping only species in species.list

# identifications for some genera are unreliable, fix or remove:
mammals$Genus[mammals$Genus == "Nasuella"] <- "Nasua"
mammals$Genus <- factor(mammals$Genus) # to remove excluded species from factor levels otherwise they will end up as zeros in the paMatrix
table(mammals$Genus)

## ----Create community matrix Y-----------------

# SXY: study design (S) and/or covariates (X) and species data (Y) 
# S: study design, including units of study and their possible coordinates (named as Route_x and Route_y to indicate that they relate to the units of Route)
# X: covariates to be used as predictors
# Y: species data
# If you don't have variables that define the study design, indicate this by S=NULL
# If you don't have covariate data, indicate this by X=NULL
# What is not always easy is to decide what goes to S and what to X.
# As a general rule, include in S those variables that you think should be modeled as random effect,
# and in X those that you think should be modeled as fixed effects.
# Don't worry if you are not sure which one is the "right choice", we will discuss this with you.

##----- S: study design -----
S <- distinct(mammals, Camera.Trap.Name, Sampling.Event, Longitude, Latitude, Project.Name)
S <- S[, c("Camera.Trap.Name", "Sampling.Event", "Longitude", "Latitude", "Project.Name")] # reorder columns to match other dataframes
# note: I included Project.Name in S so that it can be modeled as a random effect

# NB! S was initially designed for a multi-year analysis, however, our covariates are static among years
# So let us simplify S so that there is a single entry per site
#S <- distinct(S, Camera.Trap.Name, Longitude, Latitude, Project.Name)
#View(S)
dim(S) # check

##----- X: covariates -----
# X must have the same number of rows as S and one column for each covariate
X <- read.csv(here("data", "variables.csv"))
names(X)[6] <- "elevation"
names(X)[7] <- "slope"
names(X)[8] <- "dist.water"
X$matchcolumn <- X$Camera.Trap.Name # temporary column for matching row order
#target <- paste(S$Camera.Trap.Name, S$Sampling.Event, sep=".") # target for matching row order
target <- S$Camera.Trap.Name # optional target if pooling all years
X <- X[match(target, X$matchcolumn),]
#X <- X[match(target, X$Camera.Trap.Name),]  # optional if pooling all years
X$matchcolumn <- NULL # get rid of temporary column
rownames(X) <- rownames(S)
X <- X[, c(6:13)]
X$slope <- round(X$slope, 2)
X$dist.water <- round(X$dist.water, 2)
#View(X)
dim(X)

##----- Y: species data-----
# Y must have the same number of rows as S and one column for each species
mammals$Camera.Event <- paste(mammals$Camera.Trap.Name, mammals$Sampling.Event, sep=".")
Y <- as.data.frame.matrix(with(mammals, table(mammals$Camera.Event,Genus)))
#Y <- as.data.frame.matrix(with(mammals, table(mammals$Camera.Trap.Name,Genus)))
# do this just to ensure that S and Y rows are in the same order:
Y$matchcolumn <- rownames(Y) # temporary column for matching row order
target <- paste(S$Camera.Trap.Name, S$Sampling.Event, sep=".") # target for matching row order
#target <- S$Camera.Trap.Name # use this target if not doing multi-year analysis 
Y <- Y[match(target, Y$matchcolumn),]
Y$matchcolumn <- NULL # get rid of temporary column
rownames(Y) <- rownames(S)
#View(Y)

# correct Y for sampling effort
eff <- distinct(mammals, Camera.Trap.Name, Sampling.Event, Camera.Start.Date, Camera.End.Date)
eff$effort <- as.numeric(eff$Camera.End.Date - eff$Camera.Start.Date)

# aggregate (sum) effort from all years since we are using data from several years (but not in a multi-year analysis)
#eff <- aggregate(eff$effort, by=list(Camera.Trap.Name=eff$Camera.Trap.Name), FUN="sum")
#names(eff)[2] <- "effort"
#eff$matchcolumn <- eff$Camera.Trap.Name

eff$matchcolumn <- paste(eff$Camera.Trap.Name, eff$Sampling.Event, sep=".") # for matching
eff <- eff[match(target, eff$matchcolumn),] # match to ensure row order in eff is the same as in Y
#Y <- round(Y/eff$effort, 5) # correct Y for effort
# instead of correcting for effort, lets add effort as a covariate to X
X <- cbind(X, eff$effort)
names(X)[9] <- "effort"
head(X)
dim(X)
#View(Y)
#dim(Y)


##----- TP: Traits data-----

# feeding guilds: another version using EltonTraits (more recent than PanTheria)
traits <- read.csv("/home/elildojr/Documents/r/databases/MamFuncDat.txt", sep="\t") # read elton traits table
traits$Genus <- gsub(" .*$", "", traits$Scientific) # Create genus column in traits
traits <- filter(traits, Genus %in% generaToUse) # keeping only species in species.list
traits <- distinct(traits, Genus, .keep_all=TRUE) # remove duplicates in Genus

# Assign each species to a feeding guild
# e.g. primary consumer, secondary consumer, omnivore
# e.g. Oberosler et al. 2020 used:
# carnivore (>50% of diet based on vertebrates)
# herbivore (include grazers, browsers, granivores and frugivores, with >50% plant material)
# insectivore (>50% invertebrates),
# omnivore (generally both plant and animal material;
traits <- traits %>% add_column(herbivore = traits$Diet.Fruit+traits$Diet.Seed+traits$Diet.PlantO)
traits$herbivore <- ifelse(traits$herbivore > 50, traits$herbivore <- 1, traits$herbivore <- 0)
traits <- traits %>% add_column(carnivore = traits$Diet.Inv+traits$Diet.Vend+traits$Diet.Vunk)
traits$carnivore <- ifelse(traits$carnivore > 50, traits$carnivore <- 1, traits$carnivore <- 0)
traits <- traits %>% add_column(insectivore = traits$Diet.Inv)
traits$insectivore <- ifelse(traits$insectivore > 50, traits$insectivore <- 1, traits$insectivore <- 0)
traits$omnivore <- ifelse(traits$carnivore+traits$herbivore+traits$insectivore > 0, traits$omnivore <- 0, traits$omnivore <- 1)
traits$omnivore <- ifelse(traits$carnivore+traits$herbivore > 0, traits$omnivore <- 0, traits$omnivore <- 1)

traits <- traits[,c(27:31,24)] # select traits to be used (dietary classes and body mass)
traits <- traits[match(colnames(Y), traits$Genus),] # put rows in same order as in Y columns
traits$BodyMass.Value <- round(traits$BodyMass.Value)
TP <- traits



##----- Save files-----

# NB! the script S1_read_data.R reveals that row 971 has missing data for both X and Y
which(is.na(S[,3]))
which(is.na(X[,1]))
# this is not allowed so let us remove it from S, X and Y:
S <- S[-c(971),] 
X <- X[-c(971),] 
Y <- Y[-c(971),] 


## Save SXY file as csv and rds
dim(S); dim(X); dim(Y)
SXY <- cbind(S,X,Y)
dim(SXY)
write.csv(SXY, file=here("data", "SXY.csv"), row.names = FALSE) 
saveRDS(SXY, file=here("data", "SXY.rds")) 

## save as csv (csv is in gitignore so will not be uploaded to github)
write.csv(S, file=here("data", "S.csv"), row.names = FALSE) 
write.csv(X, file=here("data", "X.csv"), row.names = FALSE) 
write.csv(Y, file=here("data", "Y.csv"), row.names = FALSE) 
write.csv(TP, file=here("data", "TP.csv"), row.names = FALSE) 

## save as rds
saveRDS(S, file=here("data", "S.rds")) 
saveRDS(X, file=here("data", "X.rds")) 
saveRDS(Y, file=here("data", "Y.rds")) 
saveRDS(TP, file=here("data", "TP.rds")) 
