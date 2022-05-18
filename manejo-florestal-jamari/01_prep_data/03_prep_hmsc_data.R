
# Prepare SXY data for HMSC analysis

#----- load libraries
library(here)
library(tidyverse)


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
  select(placename, sampling_event, longitude, latitude) %>%
  mutate(sampling_event = as.numeric(sampling_event))
dim(S) # check
S
#View(S)


#----- X: covariates -----
# X must have the same number of rows as S and one column for each covariate
# in X, the same site can appear multiple times, e.g. it can have
# different values for a given covariable across years
# e.g. logging status can vary across years for the same site
# keep this in mind when creating the covariates file

# read covars file (created with 02_prep_covariate_data)
covars <- readRDS(here("manejo-florestal-jamari", "data", "jamari_covars_for_part_03.rds"))
covars


# create X using left_join
# so that X will also have duplicated sites (as this is a multi-year study)
X <- S %>%
  left_join(covars, by = c("placename", "sampling_event", "longitude", "latitude"))
X


#----- Y: species data -----
# Y must have the same number of rows as S and one column for each species
jamari <- jamari %>%
  mutate(placename_event  = str_c(placename, sampling_event, sep="_"))

Y <- jamari %>% group_by(placename_event, genus) %>% # using GENUS instead of BIN
  count() %>%
  separate(placename_event, c("placename", "sampling_event"), "_") %>%
  pivot_wider(names_from = genus, values_from = n) %>%
  arrange(placename, sampling_event) %>% # ensure Y rows are in same order as S and X
  mutate(sampling_event = as.numeric(sampling_event)) %>%
  replace(is.na(.), 0) 
# ensure that Y rows are in same order as S and X
my_order <- X[ , c("placename", "sampling_event")]
Y <- my_order %>%
  left_join(Y, by = c("placename", "sampling_event")) %>%
  select(-c(placename, sampling_event))
Y


# correct Y for sampling effort
# or use effort as an additional predictor in X
eff <- as_tibble(distinct(jamari, placename, sampling_event, start_date, end_date)) %>%
  mutate(effort = as.numeric(end_date - start_date),
         sampling_event = as.numeric(sampling_event)) %>%
  arrange(placename, sampling_event) %>%
  select(placename, sampling_event, effort)
eff
# ensure that eff rows are in same order as Y
eff <- my_order %>%
  left_join(eff, by = c("placename", "sampling_event"))
eff
# now we either divide Y columns by eff$effort column
#Y <- round(Y/eff$effort, 5) # correct Y for effort
# or instead of correcting for effort, we add effort as a covariate to X
X <- X %>%
  left_join(eff, by = c("placename", "sampling_event"))
X


#----- TP: Traits data -----
source(here("manejo-florestal-jamari", "functions", "get_traits_deprecated.R"))
TP
TP <- TP %>%
  mutate(body_mass = log(body_mass)) %>%
  print(n=Inf)
 

#----- Fernando Lima Traits data -----
#traits <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/13pazl0Qvzim1kQmDtM59mCIsBoZfYppwNEq7xYtkYk0/edit?usp=sharing"))
#traits%>%
  
# get 4-letter genus-species pattern for all jamari species
#species_to_use <- tibble(bin = unique(jamari$bin)) %>% 
#  separate(bin, c("genus", "species")) %>%
#  mutate(especie = paste(str_sub(genus, start = 1L, end = 4L),
#                         str_sub(species, start = 1L, end = 4L),
#                         sep="_")) %>%
#  mutate(especie = case_when(especie == "Maza_spp" ~ "Maza_amer",
#                             especie == "Leop_spp" ~ "Leop_pard",
#                             especie == "Dasy_spp" ~ "Dasy_nove",
#                             TRUE ~ as.character(especie)) ) %>%
#  pull(especie)

# filter traits keeping only species_to_use
#traits <- traits %>% 
#  filter(especies %in% species_to_use) %>%
#  print(n=Inf)

# add genus column
#traits <- traits %>% 
#  separate(especies, c("genus", "species"))
# get 4-letter genus pattern for extraction
#genus_pattern <- colnames(Y) %>%
#  str_sub(start = 1L, end = 4L)
#genus_pattern
#traits <- traits %>% 
#  filter(genus %in% genus_pattern) %>%
#  print(n=Inf)

#traits %>% left_join(traits, species, by = grepl(bin))

#traits <- traits[match(colnames(Y), traits$bin),] # put rows in same order as in Y columns
#TP <- traits


##----- Save files-----

# NB! NAs are not allowed in HMSC script so let input arbitrary data for the test
which(is.na(S)) # ok, no NAs

which(is.na(X)) # covariate data missing from some sites

# if code above returns > 0, this is not allowed in HMSC so:
#X[!complete.cases(X), ] # check which cases are not complete
# let us remove the offending sites from S, Y, and X
#offending <- X %>%
#  rowid_to_column() %>%
#  filter_all(any_vars(is.na(.))) %>%
#  pull(rowid)
#offending
#X <- X[-offending, ]
#which(is.na(X)) # check again
# and do the same for S and Y:
#S <- S[-offending, ]
#Y <- Y[-offending, ]

# check again
which(is.na(S))
which(is.na(X))
which(is.na(Y))
which(is.na(TP))


## Save SXY file as csv and rds
dim(S); dim(X); dim(Y)

# remove stuff from X
X <- X %>%
  dplyr::select(-c(sampling_event, longitude, latitude, year_logged))

# remove stuff from S
S <- S %>%
  dplyr::select(-c(placename))

SXY <- cbind(S,X,Y)

dim(SXY)
write.csv(SXY, file=here("manejo-florestal-jamari", "data", "SXY.csv"), row.names = FALSE) 
#saveRDS(SXY, file=here("data", "SXY.rds")) 

## save as csv (csv is in gitignore so will not be uploaded to github)
#write.csv(S, file=here("data", "S.csv"), row.names = FALSE) 
#write.csv(X, file=here("data", "X.csv"), row.names = FALSE) 
#write.csv(Y, file=here("data", "Y.csv"), row.names = FALSE) 
write.csv(TP, file=here("manejo-florestal-jamari", "data", "TP.csv"), row.names = FALSE) 

## save as rds
#saveRDS(S, file=here("data", "S.rds")) 
#saveRDS(X, file=here("data", "X.rds")) 
#saveRDS(Y, file=here("data", "Y.rds")) 
#saveRDS(TP, file=here("data", "TP.rds")) 

