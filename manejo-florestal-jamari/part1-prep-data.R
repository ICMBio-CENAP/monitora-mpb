
#----- load library
library(here)
library(tidyverse)
library(stringr)
library(gsheet)
library(TeachingDemos)
library(ggmap)

## ---- source this file
source(here("bin", "ahumada_codes.R"))
source(here("bin", "fix_species_names.R"))

#----- read data

# read Jamari Wildlife Insights data from Google Drive
deployments <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1NByCoaEnTX6Ot2cHaB3bKClGOdT-j0iIgJOpVL_onEI/edit?usp=sharing"))
deployments
images <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1jCr2QMCE4lK78H4MW2cjJjckIQZEIdM1mAmXeZ2YtFc/edit?usp=sharing"))
images

# get coordinates and start/end dates from deployments file and add this into images
jamari <- left_join(images, deployments[,c("deployment_id", "placename", 
                                   "longitude", "latitude", "start_date","end_date")]) %>%
  arrange(placename) # order by camera trap name # jamari[order(jamari$placename),]
jamari

# plot to check coordinates and save csv so others can get covariate values
lat <- jamari %>% distinct(latitude) %>% summarize(mean = mean(latitude)) %>% pull()
lon <- jamari %>% distinct(longitude) %>% summarize(mean = mean(longitude)) %>% pull()
ph_basemap <- get_map(location=c(lon = lon, lat = lat), zoom=11, maptype = 'terrain-background', source = 'stamen')
ggmap(ph_basemap)
jamari %>% 
  distinct(placename, latitude, longitude) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point()
  # + ggmap(ph_basemap)
sites <- jamari %>% 
  distinct(placename, latitude, longitude)
sites
write.csv(sites, here("data", "jamari_sites.csv"), row.names = FALSE)


# some fixes
jamari <- jamari %>%
  mutate(bin = factor(str_c(genus, species, sep=" ")),
         placename = as.factor(placename),
         start_date = as.Date(start_date),
         end_date = as.Date(end_date),
         photo_date = as.Date(timestamp),
         photo_time = strftime(timestamp, format="%H:%M:%S"),
         sampling_event = format(start_date, format="%Y"), 
         timestamp = as.POSIXct(timestamp)
  )

# fix species names
jamari <- f.fix.species.names(jamari)

# ---- filter independent records
# Group by events that are 60 minutes apart (and from different species)
jamari <- f.separate.events(jamari, 60)
jamari <- as_tibble(jamari)

# filter retaining only independent records (distinctive combinations of genus and grp)
# and keeping only mammals
jamari <- jamari %>%
  distinct(genus, grp, .keep_all = TRUE) %>%
  filter(class == "Mammalia") %>%
  mutate(bin = factor(bin))

#---- check and fix species IDs
# number of records per species
jamari %>%
  group_by(bin) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=Inf)


# number of sites where each species was recorded
n_sites_per_spp <- jamari %>%
  distinct(bin, placename) %>%
  group_by(bin) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=Inf)

# keep only species recorded in > 10% of sites
spp_to_use <- n_sites_per_spp %>% 
  filter(n >= length(unique(jamari$placename))/10) %>%
  mutate(bin = as.character(bin)) %>%
  pull(bin)
spp_to_use

jamari <- jamari %>%
  filter(bin %in% spp_to_use)


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
S <- jamari %>% distinct(placename, sampling_event, longitude, latitude) %>%
  dplyr::select(placename, sampling_event, longitude, latitude)
# NB! some coordinates are wrong, don't forget to fix it later!!!
# NB! if we decide to run static analysis, we will have to simplify S so that there is a single entry per site
#S <- distinct(S, placename, longitude, latitude)
#View(S)
dim(S) # check
S


#----- X: covariates -----
# NB! using a generic covariates file just for this test
# X must have the same number of rows as S and one column for each covariate

# in X, the same site can appear multiple times, e.g. it can have
# different values for a given covariable across years

# e.g. logging status can vary across years for the same site
# keep this in mind when creating the covariates file

# read covars file
covars <- readRDS(here("data", "jamari_covars_2022.rds"))
covars

# read distance to reainage file
dist_water <- read_csv(here("data", "distanciaEuclidianaDrenagem_editado.txt")) %>%
  rename(dist_water = RASTERVALU) %>%
  select(-c(FID, longitude, latitude))
dist_water

# add dist_water to covars
covars <- covars %>%
  left_join(dist_water, by = "placename")
covars

# remove intensity because we will use a year-specific logging intensity variable
covars <- covars %>%
  dplyr::select(-c(longitude, latitude, umf, upa, intensity_250, intensity_500)) %>%
  mutate(dummy_variable1 = runif(nrow(covars), 0,4),
         dummy_variable2 = rnorm(nrow(covars), 0,1))
covars

# create X using left_join
# so that X will also have duplicated sites (as this is a multi-year study)
X <- S %>%
  left_join(covars, by = "placename")
X

# add multi-year logging intensity to X
logging_intensity <- readRDS(here("data", "logging_intensity_multi_year.rds")) %>%
  rename(sampling_event = year) %>%
  mutate(sampling_event = as.character(sampling_event))
logging_intensity

X <- left_join(X, logging_intensity, by = c("placename", "sampling_event"))
X


#----- Y: species data -----
# Y must have the same number of rows as S and one column for each species
jamari <- jamari %>%
  mutate(placename_event  = str_c(placename, sampling_event, sep="_"))

Y <- jamari %>% group_by(placename_event, bin) %>%
  count() %>%
  separate(placename_event, c("placename", "sampling_event"), "_") %>%
  pivot_wider(names_from = bin, values_from = n) %>%
  arrange(placename, sampling_event) %>% # ensure Y rows are in same order as S and X
  replace(is.na(.), 0) %>%
  dplyr::select(-c(placename, sampling_event))
Y


# correct Y for sampling effort
eff <- as_tibble(distinct(jamari, placename, sampling_event, start_date, end_date)) %>%
  mutate(effort = as.numeric(end_date - start_date)) %>%
  arrange(placename, sampling_event) %>%
  dplyr::select(placename, sampling_event, effort)
eff
# NB! if using static (not multi-year) analysis, aggregate (sum) effort from all years
#eff <- aggregate(eff$effort, by=list(Camera.Trap.Name=eff$Camera.Trap.Name), FUN="sum")

# now we either divide Y columns by eff$effort column
#Y <- round(Y/eff$effort, 5) # correct Y for effort
# or instead of correcting for effort, we add effort as a covariate to X
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
traits <- traits %>%
  mutate(herbivore = Diet.Fruit+Diet.Seed+Diet.PlantO,
         herbivore <- ifelse(herbivore > 50, herbivore <- 1, herbivore <- 0),
         carnivore = Diet.Inv+Diet.Vend+Diet.Vunk,
         carnivore = ifelse(carnivore > 50, carnivore <- 1, carnivore <- 0),
         insectivore = Diet.Inv,
         insectivore = ifelse(insectivore > 50, insectivore <- 1, insectivore <- 0),
         omnivore = ifelse(carnivore+herbivore+insectivore > 0, omnivore <- 0, omnivore <- 1),
         omnivore = ifelse(carnivore+herbivore > 0, omnivore <- 0, omnivore <- 1)) %>%
  dplyr::select(genus, herbivore, carnivore, insectivore, omnivore, BodyMass.Value) %>%
  rename(body_mass = BodyMass.Value)

genus_names <- colnames(Y)
genus_names <- word(genus_names, 1, sep = "\\ ")
traits <- traits[match(genus_names, traits$genus),] # put rows in same order as in Y columns
TP <- traits
TP
# !NB bird traits missing from Elton Traits


#-------------- TESTE
# if using traits from F. Lima table, skip L.159-180
#traits <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/13pazl0Qvzim1kQmDtM59mCIsBoZfYppwNEq7xYtkYk0/edit?usp=sharing"))

#traits <- traits %>%
#  mutate(bin = gsub("_", " ", especies)) %>%
#  mutate(bin = gsub( " .*$", "", bin)) %>% # only activate if using only genera
#  relocate(bin, .before = especies) %>%
#  dplyr::select(-especies)

#species <- tibble(bin = genus_names)
#species

#traits %>% left_join(traits, species, by = grepl(bin))

#traits <- traits[match(colnames(Y), traits$bin),] # put rows in same order as in Y columns
#TP <- traits
#-------------- FIM TESTE

##----- Save files-----

# NB! NAs are not allowed in HMSC script so let input arbitrary data for the test
which(is.na(S)) # ok, no NAs

which(is.na(X)) # covariate data missing from some sites
# this is not allowed in HMSC
X[!complete.cases(X), ] # check which cases are not complete
# let us remove the offending sites from S, Y, and X
offending <- X %>%
  rowid_to_column() %>%
  filter_all(any_vars(is.na(.))) %>%
  pull(rowid)
offending
X <- X[-offending, ]
which(is.na(X)) # check again

# do the same for S and Y
S <- S[-offending, ]
Y <- Y[-offending, ]

# check again
which(is.na(S))
which(is.na(X))
which(is.na(Y))
which(is.na(TP))

#TP$genus <- colnames(Y) # use Y colnames (genera names) to replace NAs ...
# ... and arbitrarily input values from 1st row to all rows with missing data 
#for(i in 1:nrow(TP)) {
#  if(is.na(TP[i,2])) {TP[i,2:6] <- TP[1,2:6] }
#}
TP # check


## Save SXY file as csv and rds
dim(S); dim(X); dim(Y)

# remove stuff from X
X <- X %>%
  dplyr::select(-c(sampling_event, longitude, latitude))

# remove stuff from S
S <- S %>%
  dplyr::select(-c(placename))

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



