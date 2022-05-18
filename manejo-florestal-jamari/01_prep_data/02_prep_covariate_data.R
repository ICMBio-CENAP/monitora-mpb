
# Prepare covariate data for HMSC analysis


#----- load library
library(here)
library(tidyverse)
library(sf)


# read covars file (created with 02_prep_covariate_data)
covars <- readRDS(here("manejo-florestal-jamari", "data", "jamari_covars_2022.rds")) %>%
  select(- c(intensity_250, intensity_500))
covars
#View(covars)

# read umf and upa data
umf_upas <- read_csv(here("manejo-florestal-jamari", "data", "sfb_controle_upa_umf_ano.csv")) 
# some upas were logged for more than one year
# lets assume the 2nd year is just the completion of the work
# so select only the first row by umf_upa
umf_upas <- umf_upas %>%
  filter(umf != "umf-2-4") %>%
  mutate(umf_upa = paste(umf, upa, sep="_")) %>%
  group_by(umf_upa) %>%
  arrange(year) %>% # add a negative signal to get LAST year logged instead of 1st
  filter(row_number()==1) %>%
  ungroup() %>%
  select(- umf_upa) %>%
  rename(year_logged = year) %>%
  print(n=Inf)

# join covars and umf_upas
covars <- covars %>%
  left_join(umf_upas, by=c("umf", "upa")) %>%
  print(n=Inf)

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


# put covars data in multi-year format for HMSC
# to assign logging intensities to sites we must check in which year each site was logged
# lets do this:

# get location and years in which they were sampled
locations_and_years <- readRDS(here("manejo-florestal-jamari", "data", "data_for_part02.rds")) %>% distinct(placename, sampling_event, longitude, latitude) %>%
  select(placename, sampling_event, longitude, latitude) %>%
  mutate(sampling_event = as.numeric(sampling_event)) %>%
  arrange(placename) %>%
  select(-c(longitude, latitude))
locations_and_years

# join covars and locations_and_years and impute zero intensity
# if site was sampled before it was logged
covars <- locations_and_years %>%
  left_join(covars, by = "placename") %>%
  mutate(intensity_250 = case_when(year_logged > sampling_event ~ 0,
                                   TRUE ~ intensity_250),
         intensity_500 = case_when(year_logged > sampling_event ~ 0,
                                   TRUE ~ intensity_500))
covars

# add recovery time (since logging) column by using difference  between sampling_event and year_logged
# for unlogged sites/years lets impute the value of 30 (length of cutting cycle)  
covars <- covars %>%
  mutate(recovery_time = sampling_event-year_logged) %>%
  mutate(recovery_time = case_when(recovery_time > 10 ~ 30,
                                   recovery_time < 0 ~ 30,
                                   TRUE ~ recovery_time)) %>%
  print(n=Inf)


# read distance to drainage file
dist_water <- read_csv(here("manejo-florestal-jamari", "data", "distanciaEuclidianaDrenagem_editado.txt")) %>%
  rename(dist_water = RASTERVALU) %>%
  select(-c(FID, longitude, latitude))
dist_water

# add dist_water to covars
covars <- covars %>%
  left_join(dist_water, by = "placename")
covars

# save rdf for later use
saveRDS(covars, here("manejo-florestal-jamari", "data", "jamari_covars_for_part_03.rds"))
  

  