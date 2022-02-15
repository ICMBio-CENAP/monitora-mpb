
#----- load library
library(here)
library(tidyverse)
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
  select(UMF, UPA) %>%
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
  select(placename, longitude, latitude, umf, upa, geometry) %>%
  mutate(umf = ifelse(is.na(umf), "team", umf),
         upa = ifelse(is.na(upa), "team", upa))
locations_in_upas %>%
  print(n=Inf)



#----- create buffers so that we can add tree covariates for each individual camera

# create buffers:
buffer_250m = st_buffer(locations_in_upas, 0.25) %>%
  select(-c(longitude, latitude, umf, upa))
buffer_500m = st_buffer(locations_in_upas, 0.5) %>%
  select(-c(longitude, latitude, umf, upa))
plot(buffer_250m)
plot(buffer_500m)

# read tree data and selected only logged trees
trees <- read_csv("/home/elildojr/Documents/r/primates-and-trees/jamari_trees/all_trees.csv")
trees <- trees %>%
  filter(status == "explored")%>%
  rename(umf = UMF,
         upa = UPA) %>%
  mutate(umf = tolower(umf),
         upa = tolower(upa))

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
trees_250m <- st_join(trees_geo, buffer_250m, join=st_within)
trees_500m <- st_join(trees_geo, buffer_500m, join=st_within)

# calculate basal area in m2 for each individual tree
trees_250m <- trees_250m %>%
  mutate(ba = pi*(dbh/2)^2 )
trees_500m <- trees_500m %>%
  mutate(ba = pi*(dbh/2)^2 )

# sum basal area of harvested trees by location
harvested_250 <- trees_250m %>%
  st_drop_geometry() %>% 
  filter(status == "explored") %>%
  group_by(placename) %>%
  summarise(intensity_250 = sum(ba))
harvested_250

harvested_500 <- trees_500m %>%
  st_drop_geometry() %>% 
  filter(status == "explored") %>%
  group_by(placename) %>%
  summarise(intensity_500 = sum(ba))
harvested_500

# now add harvest intensity to locations_in_upas
locations_with_trees <- locations_in_upas %>%
  left_join(harvested_250, by = "placename") %>%
  left_join(harvested_500, by = "placename") %>%
  st_drop_geometry()
locations_with_trees %>%
  print(n=Inf)

# save covars file
saveRDS(locations_with_trees, here("data", "jamari_covars_2022.rds"))


# put covariate data in multi-year format for HMSC

images <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1jCr2QMCE4lK78H4MW2cjJjckIQZEIdM1mAmXeZ2YtFc/edit?usp=sharing"))
# get coordinates and start/end dates from deployments file and add this into images
jamari <- left_join(images, deployments[,c("deployment_id", "placename", 
                                           "longitude", "latitude", "start_date","end_date")])
jamari %>%
  group_by(placename, sampling_event) %>%
  count()

trees_250m %>%
  st_drop_geometry() %>%
  group_by(umf, upa, year_explored) %>%
  count() %>%
  mutate(n = ifelse(n > 0, 1, 0)) %>%
  print(n = Inf)

trees_500m %>%
  st_drop_geometry() %>%
  group_by(umf, upa, year_explored) %>%
  count() %>%
  mutate(n = ifelse(n > 0, 1, 0)) %>%
  print(n = Inf)

# create logging status by year (logged/unlogged)
logStatus <- table(umf.upas$Camera.Trap.Name, umf.upas$YearExplored) # based on umf.upas
#logStatus <- table(covars$Camera.Trap.Name, covars$YearExplored) # based on covars
#dimnames(logStatus)[[2]] <- c("2008", "2012","2013","2014","2015","2016","2017","2018","2019")
for(i in 1:nrow(logStatus)) {
  logStatus[i,] <- cummax(logStatus[i,])
}
logStatus[logStatus >= 1] <- 1
# we only need 2016 to 2019 data
logStatus <- logStatus[,6:9]
logStatus <- as.data.frame.matrix(logStatus)
head(logStatus)

logStatus$Camera.Trap.Name <- rownames(logStatus)
logStatus <- logStatus[,c("Camera.Trap.Name", "2016", "2017", "2018", "2019")]
row.names(logStatus) <- NULL
names(logStatus) <- c("Camera.Trap.Name", "status.16", "status.17", "status.18", "status.19")
head(logStatus)
dim(logStatus)

logStatus <- merge(logStatus, umf.upas[c("Camera.Trap.Name", "UMF")], by="Camera.Trap.Name")
#logStatus <- subset(logStatus, UMF != "TEAM")
logStatus$UMF <- NULL
head(logStatus)
dim(logStatus)


