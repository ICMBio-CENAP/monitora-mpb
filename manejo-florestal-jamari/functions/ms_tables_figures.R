
# Prepare some figures and tables for jamari-monitora-mpb manuscript

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

# fix species names and check
jamari <- f.fix.species.names(jamari)
jamari %>%
  group_by(class, order, family, bin) %>%
  count() %>%
  print(n=Inf)


# filter independent records
# group by events that are 60 minutes apart
jamari <- f.separate.events(jamari, 60)
jamari <- as_tibble(jamari)

# filter retaining only independent records
jamari <- jamari %>%
  distinct(genus, grp, .keep_all = TRUE) %>%
  mutate(bin = factor(bin))

# em quantos sitios cada especie foi registrada?
sites <- jamari %>%
  distinct(genus, placename) %>%
  count(genus) %>%
  rename(sites = n) %>%
  arrange(desc(sites)) %>%
  print()

# table 1
table_S1 <- jamari %>%
  group_by(genus) %>%
  count() %>%
  rename(records = n) %>%
  left_join(jamari %>%
              select(order, family, genus), by="genus") %>%
  left_join(sites, by="genus") %>%
  distinct(genus, .keep_all = TRUE) %>%
  select(order, family, genus, sites, records) %>%
  arrange(order, family, genus) %>%
  print(n=Inf)


# total records
table_S1 %>%
  ungroup() %>%
  summarize(total_records = sum(records))

# total effort
eff <- as_tibble(distinct(jamari, placename, sampling_event, start_date, end_date)) %>%
  mutate(effort = as.numeric(end_date - start_date),
         sampling_event = as.numeric(sampling_event)) %>%
  arrange(placename, sampling_event) %>%
  select(placename, sampling_event, effort) %>%
  summarize(total_effort = sum(effort)) %>%
  print()



######################


#---- read pre-processed data from 01_clean_camera_trap_data
jamari <- readRDS(here("manejo-florestal-jamari", "data", "data_for_part02.rds"))

S <- jamari %>% distinct(placename, sampling_event, longitude, latitude) %>%
  select(placename, sampling_event, longitude, latitude) %>%
  mutate(sampling_event = as.numeric(sampling_event))

covars <- readRDS(here("manejo-florestal-jamari", "data", "jamari_covars_for_part_03.rds"))

X <- S %>%
  left_join(covars, by = c("placename", "sampling_event", "longitude", "latitude"))
X


# save csv for Figure 1 map
fig1_map <- X %>%
  distinct(placename, longitude, latitude, umf, upa)
write.csv(fig1_map, "/home/elildojr/Desktop/fig1_map.csv", row.names = FALSE)

# quantos sitios foram explorados ou nao explorados 
X %>% 
  group_by(placename) %>%
  summarize(max_logging = max(intensity_500)) %>%
  filter(max_logging > 0) %>%
  print(n = Inf)

98/191
# media e range da intensidade de exploracao
X %>% 
  group_by(placename) %>%
  summarize(max_logging = max(intensity_500)) %>%
  filter(max_logging > 0) %>%
  summarize(media = mean(max_logging), min = min(max_logging), max = max(max_logging)) %>%
  print(n = Inf)

# sumario de distancia da agua
X %>% 
  group_by(placename) %>%
  count(dist_water) %>%
  ungroup() %>%
  filter(dist_water > 0) %>%
  summarize(media = mean(dist_water), min = min(dist_water), max = max(dist_water)) %>%
  print(n = Inf)


