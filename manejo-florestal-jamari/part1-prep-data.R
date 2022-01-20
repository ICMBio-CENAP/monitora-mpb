
#----- load library
library(here)
library(tidyverse)
library(stringr)
library(gsheet)

## ---- source this file
source(here("bin", "ahumada_codes.R"))

#----- read data

# read Jamari Wildlife Insights data from Google Drive
deployments <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1qzexPLq39Fc2xfTco1C_p2Hpj-h3rhyXEQx-lyovEy8/edit?usp=sharing"))
deployments
images <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1FJIJH8So5snTs9VOqtQjVd-CsQSW_52y41OSh2TzRtI/edit?usp=sharing"))
images

# get coordinates and start/end dates from deployments file and add this into images
jamari <- left_join(images, deployments[,c("deployment_id", "placename", "longitude", "latitude", "start_date","end_date")])
jamari <- jamari[order(jamari$placename),] # order by camera trap name
jamari

# some fixes
jamari$bin <- paste(jamari$genus, jamari$species, sep=" ") # binomial names
jamari$placename <- as.factor(jamari$placename)
jamari$start_date <- as.Date(jamari$start_date)
jamari$end_date <- as.Date(jamari$end_date)
jamari$photo_date <- as.Date(jamari$timestamp)
jamari$photo_time <- strftime(jamari$timestamp, format="%H:%M:%S")
jamari$sampling_event <- format(jamari$start_date, format="%Y") 
jamari$timestamp <- as.POSIXct(jamari$timestamp)


# ---- filter independent records
# Group by events that are 60 minutes apart (and from different species)
jamari <- f.separate.events(jamari, 60)
# filter retaining only independent records (distinctive combinations of genus and grp)
jamari <- distinct(jamari, genus, grp, .keep_all = TRUE)

# work only with mammal photos
#jamari <- filter(jamari, Class == "Mammalia") # keeping only species in species.list

#---- check and fix species IDs
table(jamari$bin) # checking number of records per species
rowSums(table(jamari$bin, by=jamari$location) != 0) # number of sites where each species was recorded
unique(jamari$bin) # a list of recorded species to create species.list

#NB!fix species names pending, for now let us proceed using genus:
# select genera amenable to camera trapping (i.e., terrestrial > 0.5 kg)
genera <- sort(unique(jamari$genus))
genera
generaToUse <- c("Atelocynus", "Crypturellus", "Cuniculus", "Dasyprocta",
                 "Dasypus", "Didelphis", "Eira", "Herpailurus", "Hydrochoerus",
                 "Leopardus", "Mazama", "Metachirus", "Mitu", "Myrmecophaga", "Nasua",
                 "Nothocrax", "Odontophorus", "Panthera", "Pecari", "Penelope",
                 "Priodontes", "Procyon", "Psophia", "Puma", "Sciurus", "Speothos", "Sylvilagus",
                 "Tamandua", "Tapirus", "Tayassu", "Tinamus")
jamari <- filter(jamari, genus %in% generaToUse) # keeping only species in species.list



# ----Create community matrix Y

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
S <- distinct(jamari, placename, sampling_event, longitude, latitude)
S <- S[, c("placename", "sampling_event", "longitude", "latitude")] # reorder columns to match other dataframes
# NB! some coordinates are wrong, don't forget to fix it later!!!
# NB! if we decide to run static analysis, we will have to simplify S so that there is a single entry per site
#S <- distinct(S, placename, longitude, latitude)
#View(S)
#dim(S) # check
S <- as_tibble(S)
S


#----- X: covariates -----
# NB! using a generic covariates file just for this test
# X must have the same number of rows as S and one column for each covariate
#X <- read_csv(here("data", "variables.csv"))
X <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/19ndf6lf8fpaKxH8Xv1QqvUhvjYJDtSyZjg_4rIMy6Eo/edit?usp=sharing"))
X <- X %>% 
  rename(placename=Camera.Trap.Name, elevation=altitude, slope=declividade, dist_water=water_dist) %>%
  filter(placename %in% S$placename) %>%
  mutate(placename=as.factor(placename)) %>%
  select(placename, elevation, slope, dist_water, hfi)
target <- S$placename
X <- X[match(target, X$placename),] # put sites in same order as S
#rownames(X) <- rownames(S)
X # check


#----- Y: species data -----
# Y must have the same number of rows as S and one column for each species
jamari$placename_event <- paste(jamari$placename, jamari$sampling_event, sep="_")
Y <- as.data.frame.matrix(with(jamari, table(jamari$placename_event, genus)))
#Y <- as.data.frame.matrix(with(mammals, table(mammals$Camera.Trap.Name,Genus)))
# do this just to ensure that S and Y rows are in the same order:
target <- as.factor(paste(S$placename, S$sampling_event, sep="_"))
Y <- Y[match(target, row.names(Y)),]
Y <- as_tibble(Y)
#rownames(Y) <- rownames(S)
#Y$placename <- S$placename
Y



# correct Y for sampling effort
eff <- as_tibble(distinct(jamari, placename, sampling_event, start_date, end_date))
eff$effort <- as.numeric(eff$end_date - eff$start_date)
# NB! if using static (non-multi-year) analysis, aggregate (sum) effort from all years
#eff <- aggregate(eff$effort, by=list(Camera.Trap.Name=eff$Camera.Trap.Name), FUN="sum")
target <- S$placename
eff <- eff[match(target, eff$placename),] # match to ensure row order in eff is the same as in Y
eff
#Y <- round(Y/eff$effort, 5) # correct Y for effort
# instead of correcting for effort, lets add effort as a covariate to X
#X <- left_join(X, eff[,c("placename", "effort")], by="placename")
X <- X %>%
  mutate(effort = eff$effort)
X


#----- TP: Traits data -----

# feeding guilds: another version using EltonTraits (more recent than PanTheria)
#traits <- read.csv("/home/elildojr/Documents/r/databases/MamFuncDat.txt", sep="\t") # read elton traits table
#traits$genus <- gsub(" .*$", "", traits$Scientific) # Create genus column in traits
#traits <- filter(traits, genus %in% generaToUse) # keeping only species in species.list
#traits <- distinct(traits, genus, .keep_all=TRUE) # remove duplicates in Genus
#write.csv(traits, here("data", "traits.csv"), row.names = FALSE)
traits <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1HYNHvnZAQ3NPbR58KJZRWocaA46PPVT-AtfIIkVj4uc/edit?usp=sharing"))
traits

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

traits <- traits %>%
  select(genus, herbivore, carnivore, insectivore, omnivore, BodyMass.Value) %>%
  rename(body_mass=BodyMass.Value)
traits <- traits[match(colnames(Y), traits$genus),] # put rows in same order as in Y columns
TP <- traits
# !NB bird traits missing from Elton Traits



##----- Save files-----

# NB! NAs are not allowed in HMSC script so let usimput arbitrary data for the test
which(is.na(S)) # ok, no NAs

which(is.na(X)) # covariate data missing from some sites
# let us input arbitrary values to these missing rows
X$placename <- S$placename # use S placenames to replace NAs ...
# ... and arbitrarily input values from 1st row to all rows with missing data 
for(i in 1:nrow(X)) {
  if(is.na(X[i,2])) {X[i,2:5] <- X[1,2:5] }
}

which(is.na(Y))

which(is.na(TP))
TP$genus <- colnames(Y) # use Y colnames (genera names) to replace NAs ...
# ... and arbitrarily input values from 1st row to all rows with missing data 
for(i in 1:nrow(TP)) {
  if(is.na(TP[i,2])) {TP[i,2:6] <- TP[1,2:6] }
}
TP # check


## Save SXY file as csv and rds
dim(S); dim(X); dim(Y)
X$placename <- NULL
#Y$placename <- NULL
SXY <- cbind(S,X,Y)
dim(SXY)
write.csv(SXY, file=here("data", "SXY.csv"), row.names = FALSE) 
#saveRDS(SXY, file=here("data", "SXY.rds")) 

## save as csv (csv is in gitignore so will not be uploaded to github)
#write.csv(S, file=here("data", "S.csv"), row.names = FALSE) 
#write.csv(X, file=here("data", "X.csv"), row.names = FALSE) 
#write.csv(Y, file=here("data", "Y.csv"), row.names = FALSE) 
write.csv(TP, file=here("data", "TP.csv"), row.names = FALSE) 

## save as rds
#saveRDS(S, file=here("data", "S.rds")) 
#saveRDS(X, file=here("data", "X.rds")) 
#saveRDS(Y, file=here("data", "Y.rds")) 
#saveRDS(TP, file=here("data", "TP.rds")) 



