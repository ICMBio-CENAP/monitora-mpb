
# Prepare SXY data for HMSC analysis

#----- load libraries
library(here)
library(tidyverse)
#library(stringr)
#library(gsheet)
#library(TeachingDemos)
#library(ggmap)


#---- source files
#source(here("manejo-florestal-jamari", "functions", "ahumada_codes.R"))
#source(here("manejo-florestal-jamari", "functions", "fix_species_names.R"))


#---- read pre-processed data from 01_clean_camera_trap_data
jamari <- readRDS(here("manejo-florestal-jamari", "data", "data_for_part02.rds"))


#---- create community matrix Y

# reminder:
# SXY: study design (S) and/or covariates (X) and species data (Y) 
# S: study design, including units of study and their possible coordinates (named as Route_x and Route_y to indicate that they relate to the units of Route)
# X: covariates to be used as predictors
# Y: species data
# if you don't have variables that define the study design, indicate this by S=NULL
# if you don't have covariate data, indicate this by X=NULL
# what is not always easy is to decide what goes to S and what to X.
# as a general rule, include in S variables that should be modeled as random effect,
# and in X those that should be modeled as fixed effects


#----- S: study design -----
S <- jamari %>% distinct(placename, sampling_event, longitude, latitude) %>%
  dplyr::select(placename, sampling_event, longitude, latitude)
dim(S) # check
S
#View(S)


#----- X: covariates -----
# X must have the same number of rows as S and one column for each covariate
# in X, the same site can appear multiple times, e.g. it can have
# different values for a given covariable across years
# e.g. logging status can vary across years for the same site
# keep this in mind when creating the covariates file

# read covars file
# and remove intensity as we will use a year-specific intensity variable
#covars <- readRDS(here("manejo-florestal-jamari", "data", "jamari_covars_2022.rds")) %>%
#  dplyr::select(-c(longitude, latitude, umf, upa, intensity_250, intensity_500))
#covars
#View(covars)

# read distance to drainage file
dist_water <- read_csv(here("manejo-florestal-jamari", "data", "distanciaEuclidianaDrenagem_editado.txt")) %>%
  rename(dist_water = RASTERVALU) %>%
  select(-c(FID, longitude, latitude))
dist_water

# add dist_water to covars
#covars <- covars %>%
#  left_join(dist_water, by = "placename")
#covars

# create X using left_join
# so that X will also have duplicated sites (as this is a multi-year study)
X <- S %>%
  left_join(covars, by = "placename")
X

# add multi-year logging intensity to X
logging_intensity <- readRDS(here("manejo-florestal-jamari", "data", "logging_intensity_multi_year.rds")) %>%
  rename(sampling_event = year) %>%
  mutate(sampling_event = as.character(sampling_event))
logging_intensity

# check
logging_intensity %>%
  select(- intensity_250) %>%
  pivot_wider(names_from = sampling_event,
              values_from = intensity_500) %>%
  print(n=Inf)

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

# use genus as colnames for y so they match TP rows
colnames(Y) <- as_tibble(colnames(Y)) %>%
  separate(value, c("genus", "species")) %>%
  pull(genus)


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
source(here("bin", "get_traits_deprecated.R"))
TP

#----- Fernando Lima Traits data -----
# note: there are some problems that must be solved before using this
# for now lets use the TP created above

# read F. Lima's trait data
traits <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/13pazl0Qvzim1kQmDtM59mCIsBoZfYppwNEq7xYtkYk0/edit?usp=sharing"))

# filter traits using only species in Y
# 1st get pattern for genus in Y
genus_pattern <- colnames(Y) %>%
  str_sub(start = 1L, end = 4L)
genus_pattern
# 2nd get pattern for genus in traits and filter unique genera
traits <- traits %>%
  mutate(especies = str_sub(especies, start = 1L, end = 4L)) %>%
  distinct(especies, .keep_all = TRUE)
# 3rd now filter traits keeping only genera in Y
traits %>% 
  filter(especies %in% genus_pattern) # WARNING: there are species in Y without traits!!!

# 4th select traits to be used
# WARNING: pending work
traits <- traits %>%
  mutate(bin = gsub("_", " ", especies)) %>%
  mutate(bin = gsub( " .*$", "", bin)) %>% # only activate if using only genera
  relocate(bin, .before = especies) %>%
  dplyr::select(-especies)

#species <- tibble(bin = genus_names)
#species

#traits %>% left_join(traits, species, by = grepl(bin))

#traits <- traits[match(colnames(Y), traits$bin),] # put rows in same order as in Y columns
#TP <- traits


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

