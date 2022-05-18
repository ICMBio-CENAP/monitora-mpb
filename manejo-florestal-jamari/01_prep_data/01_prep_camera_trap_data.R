
# Prepare camera trap data for HMSC analysis

#----- load libraries
library(here)
library(tidyverse)
library(stringr)
library(gsheet)
library(TeachingDemos)
library(ggmap)


#---- source files
source(here("manejo-florestal-jamari", "functions", "ahumada_codes.R"))
source(here("manejo-florestal-jamari", "functions", "fix_species_names.R"))


#---- read and fix data

# read Jamari Wildlife Insights data from Google Drive
deployments <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1NByCoaEnTX6Ot2cHaB3bKClGOdT-j0iIgJOpVL_onEI/edit?usp=sharing"))
images <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1jCr2QMCE4lK78H4MW2cjJjckIQZEIdM1mAmXeZ2YtFc/edit?usp=sharing"))


# merge images/deployments to get coordinates and start/end dates
jamari <- left_join(images, deployments[,c("deployment_id", "placename", 
                                   "longitude", "latitude", "start_date","end_date")]) %>%
  arrange(placename) # order by camera trap name # jamari[order(jamari$placename),]
jamari


# some data fixes
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

# select mammals only
jamari <- jamari %>%
  filter(class == "Mammalia")

# check species names
jamari %>%
  group_by(class, order, family, bin) %>%
  count() %>%
  print(n=Inf)

# fix species names and check again
jamari <- f.fix.species.names(jamari)
jamari %>%
  group_by(class, order, family, bin) %>%
  count() %>%
  print(n=Inf)

# check if coordinates are OK
jamari %>% 
  distinct(placename, latitude, longitude) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point()

# we will need covariates for each individual site
# let us save the list of sites with coordinates for later use  
sites <- jamari %>% 
  distinct(placename, latitude, longitude)
sites
write.csv(sites, here("manejo-florestal-jamari", "data", "jamari_sites.csv"), row.names = FALSE)


# filter independent records
# group by events that are 60 minutes apart
jamari <- f.separate.events(jamari, 60)
jamari <- as_tibble(jamari)

# filter retaining only independent records
jamari <- jamari %>%
  distinct(genus, grp, .keep_all = TRUE) %>%
  mutate(bin = factor(bin))


#---- check number of records and filter data to be used

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
  ungroup(bin) %>%
  mutate(bin = as.character(bin)) %>%
  pull(bin)
spp_to_use

jamari <- jamari %>%
  filter(bin %in% spp_to_use)

# save for to use in part 02_prep_hmsc_data
saveRDS(jamari, here("manejo-florestal-jamari", "data", "data_for_part02.rds"))
