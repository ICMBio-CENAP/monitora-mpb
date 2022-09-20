
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
deployments <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1Rh1WosR3QgZ2-BoBMeKNbtBfMCkSIvUjw4xcWsaDs4o/edit?usp=sharing"))
deployments
images <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1mEykChupnBsqL-QXBVxp4ZgXrLS3-JaYTaMb7r512SM/edit?usp=sharing"))
images


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


# before checking time lag, let us check it there are weird dates
# in photos
jamari %>%
  group_by(sampling_event) %>%
  summarize(min = min(photo_date),
            max = max(photo_date))


# check the time lag between start date and 1st photo, last photo and end date
# if lag is > 5 days, redefine start and end dates
to_fix_start_and_end <- jamari %>%
  group_by(deployment_id) %>%
  summarise(start_date = min(start_date),
            end_date = min(end_date),
            min_photo_date = min(photo_date),
            max_photo_date = max(photo_date)) %>%
  mutate(sampling_length = as.numeric(end_date - start_date),
         start_lag = as.numeric(min_photo_date - start_date),
         end_lag = as.numeric(end_date - max_photo_date) ) %>%
  #filter(start_lag > 7 | start_lag < 0 | end_lag > 7 | end_lag  < 0) %>%
  mutate(start_date = if_else(start_lag > 5 | start_lag < 0, # condition
                              min_photo_date-5, # true
                              start_date), # false
         end_date = if_else(end_lag > 5 | end_lag < 0, # condition
                            max_photo_date+5, # true
                            end_date)) %>% # false
  mutate(new_start_lag = as.numeric(min_photo_date - start_date),
         new_end_lag = as.numeric(end_date - max_photo_date)) %>%
  print(n=Inf)

to_fix_start_and_end <- to_fix_start_and_end %>%
  select(deployment_id, start_date, end_date)
to_fix_start_and_end

# replace start and end dates in jamari using the new ones:
jamari <- jamari %>%
  select(-(c(start_date, end_date))) %>%
  left_join(to_fix_start_and_end, by="deployment_id") %>%
  print()

# to check if it worked
jamari %>%
  group_by(deployment_id) %>%
  summarise(start_date = min(start_date),
            end_date = min(end_date),
            min_photo_date = min(photo_date),
            max_photo_date = max(photo_date)) %>%
  mutate(sampling_length = as.numeric(end_date - start_date),
         start_lag = as.numeric(min_photo_date - start_date),
         end_lag = as.numeric(end_date - max_photo_date) ) %>%
  filter(start_lag > 5 | start_lag < 0 | end_lag > 5 | end_lag  < 0) %>%
  print(n=Inf)



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
