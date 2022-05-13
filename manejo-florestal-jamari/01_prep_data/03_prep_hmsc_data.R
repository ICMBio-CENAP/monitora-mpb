
# Prepare SXY data for HMSC analysis

#----- load libraries
library(here)
library(tidyverse)
library(sf)
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

# read covars file (created with 02_prep_covariate_data)
covars <- readRDS(here("manejo-florestal-jamari", "data", "jamari_covars_2022.rds")) %>%
  select(- c(intensity_250, intensity_500))
covars
#View(covars)

# convert covars to shapefile
covars_geo <- st_as_sf(covars, coords=c("longitude","latitude")) %>%
  mutate(longitude = st_coordinates(.)[,1],
         latitude = st_coordinates(.)[,2])
# check CRS
st_is_longlat(covars_geo)
# CRS missing, so add CRS (WGS 84)
covars_geo <- st_set_crs(covars_geo, 4326)
st_is_longlat(covars_geo) # check
# transform to UTM because later we will calculate distances etc in metres
covars_geo <- st_transform(covars_geo, "+proj=utm +zone=20S +datum=WGS84 +units=km")


# read tree data and filter logged trees
trees <- read_csv(here("manejo-florestal-jamari", "data", "all_trees_updated_feb2022.csv"))
trees <- trees %>%
  filter(status == "explored")

# convert trees to shapefile
trees_geo <- st_as_sf(trees, coords=c("lon","lat"))
# check CRS
st_is_longlat(trees_geo)
# CRS missing, so add CRS (WGS 84)
trees_geo <- st_set_crs(trees_geo, 4326)
st_is_longlat(trees_geo) # check
# transform to UTM because later we will calculate distances etc in metres
trees_geo <- st_transform(trees_geo, "+proj=utm +zone=20S +datum=WGS84 +units=km")


# create buffers around placenames to assign logged trees to each site
buffer_250m = st_buffer(covars_geo, 0.25) %>%
  dplyr::select(-c(longitude, latitude, umf, upa))
buffer_500m = st_buffer(covars_geo, 0.5) %>%
  dplyr::select(-c(longitude, latitude, umf, upa))
plot(buffer_250m)
plot(buffer_500m)


# associate trees to buffers
trees_250m <- st_join(trees_geo, buffer_250m, join=st_within) %>%
  filter(!is.na(placename)) %>% # remove trees outside buffers
  st_drop_geometry()
trees_250m

trees_500m <- st_join(trees_geo, buffer_500m, join=st_within) %>%
  filter(!is.na(placename)) %>% # remove trees outside buffers
  st_drop_geometry()
trees_500m

# calculate basal area in m2 for each individual tree
trees_250m <- trees_250m %>%
  mutate(ba = pi*(dbh/2)^2 )
trees_500m <- trees_500m %>%
  mutate(ba = pi*(dbh/2)^2 )

# sum basal area of harvested trees by location
harvested_250 <- trees_250m %>%
  filter(status == "explored") %>%
  group_by(placename) %>%
  summarise(intensity_250 = sum(ba))
harvested_250

harvested_500 <- trees_500m %>%
  filter(status == "explored") %>%
  group_by(placename) %>%
  summarise(intensity_500 = sum(ba))
harvested_500

# now add harvest intensity to covars
covars <- covars %>%
  left_join(harvested_250, by = "placename") %>%
  left_join(harvested_500, by = "placename") %>%
  replace(is.na(.), 0)
covars %>%
  print(n=Inf)

# get years in which each site was harvested
harvested_500 <- trees_500m %>%
  filter(status == "explored") %>%
  group_by(placename, year_explored) %>%
  summarise(intensity_500 = sum(ba)) %>%
  rename(year = year_explored)
harvested_500
harvested_500 %>% 
  print(n=Inf)

# sum basal area of harvested trees by location and year
harvested_250 <- trees_250m %>%
  filter(status == "explored") %>%
  group_by(placename, year_explored) %>%
  summarise(intensity_250 = sum(ba)) %>%
  rename(year = year_explored)
harvested_250

harvest <- left_join(harvested_500, harvested_250, by=c("placename", "year")) %>%
  replace_na(list(intensity_500 = 0, intensity_250 = 0)) %>%
  arrange(placename)
harvest %>%
  print(n=Inf)

# some trees have been explored but do not have year explored
# causing trouble in harvest object
trees %>%
  filter(status == "explored", is.na(year_explored)) %>%
  View()
  distinct(umf, upa)
#umf-3 upa-01, umf-3 upa-12
# I checked externally and they were logged in 2010 and 2018 respectively
# so let us add this to harvest
# 1st check which cams belong to each umf/upa
covars %>%
  filter(umf == "umf-3",
         upa %in% c("upa-01", "upa-12")) %>%
  distinct(umf, upa, placename)
# and now fix harvest
harvest <- harvest %>%
  mutate(year = replace(year, placename %in% c("CT-473", "CT-512", "CT-549",
                                               "CT-472", "CT-510", "CT-474", 
                                               "CT-511", "CT-509", "CT-434"),
                        2018)) %>%
  mutate(year = replace(year, placename %in% c("CT-283", "CT-242", "CT-244",
                                          "CT-245", "CT-282", "CT-321", 
                                          "CT-320", "CT-204", "CT-205"),
                        2010))
harvest %>%
  print(n=Inf)


# put covars data in multi-year format for HMSC
# to assign logging intensities to sites we must check in which year each site was logged
# lets do this:

# get location and years in which they were sampled
locations_and_years <- S %>%
  rename(year = sampling_event) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(placename) %>%
  select(-c(longitude, latitude))
locations_and_years


locations_and_years <- locations_and_years %>%
  mutate(intensity_500 = as.numeric(NA),
         intensity_250 = as.numeric(NA))
locations_and_years



# fill all columns of locations_and_years with intensity values
for(i in 1:nrow(locations_and_years)) {
  for(j in 1:nrow(harvest)) {
    if(locations_and_years[i, "placename"] == harvest[j, "placename"] &
       locations_and_years[i, "year"] >= harvest[j, "year"]) {
      locations_and_years[j, c("intensity_500", "intensity_250")] <-
        harvest[i, c("intensity_500", "intensity_250")]
    } else {
      locations_and_years[j, c("intensity_500", "intensity_250")] <- 0
    }
  }
}
locations_and_years %>%
  print(n=Inf)



############################################################################

# read distance to drainage file
dist_water <- read_csv(here("manejo-florestal-jamari", "data", "distanciaEuclidianaDrenagem_editado.txt")) %>%
  rename(dist_water = RASTERVALU) %>%
  select(-c(FID, longitude, latitude))
dist_water

# add dist_water to covars
covars <- covars %>%
  left_join(dist_water, by = "placename")
covars


############################################################################

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

