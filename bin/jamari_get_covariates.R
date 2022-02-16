
#----- load library
library(here)
library(tidyverse)
library(lubridate)
#library(stringr)
library(gsheet)
library(sf)

## ---- source this file
#source(here("bin", "ahumada_codes.R"))
#source(here("bin", "fix_species_names.R"))

#----- read data

# read Jamari Wildlife Insights data from Google Drive
deployments <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1NByCoaEnTX6Ot2cHaB3bKClGOdT-j0iIgJOpVL_onEI/edit?usp=sharing"))
deployments

# get unique locations
locations <- deployments %>% 
  distinct(placename, latitude, longitude)
locations


#----- add umf and upa data

# convert locations to sf (shapefile) format
locations_geo <- st_as_sf(locations, coords=c("longitude","latitude"), remove=FALSE)
# check CRS
st_is_longlat(locations_geo)
# CRS missing, so add CRS (WGS 84)
locations_geo <- st_set_crs(locations_geo, 4326)
st_is_longlat(locations_geo) # check
# transform to UTM because later we will calculate distances etc in metres
locations_geo <- st_transform(locations_geo, "+proj=utm +zone=20S +datum=WGS84 +units=km") # transform to metric  

# read umf and upa data and do some fixes
umf_upas <- st_read("/home/elildojr/Documents/gis/jamari/sfb/umf_upas_utm.shp")
umf_upas <- st_transform(umf_upas, "+proj=utm +zone=20S +datum=WGS84 +units=km") # transform to metric 
umf_upas <- umf_upas %>%
  dplyr::select(UMF, UPA) %>%
  rename(umf = UMF,
         upa = UPA) %>%
  mutate(umf = tolower(umf),
         upa = tolower(upa),
         umf = str_replace(umf, "iii", "3"),
         umf = str_replace(umf, "i", "1"),
         upa = str_replace(upa, " ", "-") )
umf_upas %>%
  print(n=Inf)

# join location and upa data
locations_in_upas <- st_join(locations_geo, umf_upas, join=st_within) %>%
  dplyr::select(placename, longitude, latitude, umf, upa, geometry) %>%
  mutate(umf = ifelse(is.na(umf), "team", umf),
         upa = ifelse(is.na(upa), "team", upa))
locations_in_upas %>%
  print(n=Inf)



#----- create buffers so that we can add tree covariates for each individual camera

# create buffers:
buffer_250m = st_buffer(locations_in_upas, 0.25) %>%
  dplyr::select(-c(longitude, latitude, umf, upa))
buffer_500m = st_buffer(locations_in_upas, 0.5) %>%
  dplyr::select(-c(longitude, latitude, umf, upa))
plot(buffer_250m)
plot(buffer_500m)

# read tree data and selected only logged trees
trees <- read_csv(here("data", "all_trees_updated_feb2022.csv"))
trees <- trees %>%
  filter(status == "explored")

# convert to shapefile
trees_geo <- st_as_sf(trees, coords=c("lon","lat"))
# check CRS
st_is_longlat(trees_geo)
# CRS missing, so add CRS (WGS 84)
trees_geo <- st_set_crs(trees_geo, 4326)
st_is_longlat(trees_geo) # check
# transform to UTM because later we will calculate distances etc in metres
trees_geo <- st_transform(trees_geo, "+proj=utm +zone=20S +datum=WGS84 +units=km")

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

# now add harvest intensity to locations_in_upas
locations_with_trees <- locations_in_upas %>%
  st_drop_geometry() %>%
  left_join(harvested_250, by = "placename") %>%
  left_join(harvested_500, by = "placename") %>%
  replace(is.na(.), 0)
locations_with_trees %>%
  print(n=Inf)

# save covars file
saveRDS(locations_with_trees, here("data", "jamari_covars_2022.rds"))

#----- put covariate data in multi-year format for HMSC

# to assign logging intensities to sites we must check in which year each site was logged
# lets do this:

# get location and years in which they were sampled
locations_and_years <- deployments %>%
  mutate(start_date = ymd_hms(start_date),
         sampling_event = format(start_date, format="%Y")) %>%
  distinct(placename, sampling_event) %>%
  rename(year = sampling_event) %>%
  mutate(year = as.numeric(year)) %>%
  arrange(placename)
locations_and_years

# get years in which each location was harvested
harvested_500 <- trees_500m %>%
  filter(status == "explored") %>%
  group_by(placename, year_explored) %>%
  summarise(intensity_500 = sum(ba)) %>%
  rename(year = year_explored)
harvested_500

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


locations_and_years <- locations_and_years %>%
  mutate(intensity_500 = as.numeric(NA),
         intensity_250 = as.numeric(NA))
locations_and_years

# fill all columns of locations_and_years with intensity values
for(i in 1:nrow(harvest)) {
  for(j in 1:nrow(locations_and_years)) {
    if(harvest[i, "placename"] == locations_and_years[i, "placename"] &
       locations_and_years[i, "year"] >= harvest[i, "year"]) {
      locations_and_years[i, c("intensity_500", "intensity_250")] <-
        harvest[i, c("intensity_500", "intensity_250")]
    } else {
      locations_and_years[i, c("intensity_500", "intensity_250")] <- 0
    }
  }
}
locations_and_years

# save  file
saveRDS(locations_and_years, here("data", "logging_intensity_multi_year.rds"))



# an alternative that was not needed
#harvest500 <- harvest %>% 
#  dplyr::select(-intensity_250) %>%
#  pivot_wider(names_from = "year", values_from = intensity_500 ) %>%
#  dplyr::select(`2011`, `2012`, `2013`, `2014`, `2015`, `2016`, `2017`, `2018`, `2019`, `NA`) %>%
#  print(n=Inf)

