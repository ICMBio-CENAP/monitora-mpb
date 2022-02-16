# merge MADEFLONA and AMATA tree shapefiles
# provided by SFB - Serviço Florestal Brasileiro
# data is messy
# # fix what can be fixed

# to install rgdal in Ubuntu:
#sudo apt-get install gdal-bin proj-bin libgdal-dev libproj-dev
#install.packages("rgdal")

# load library
library(here)
library(sf)
library(raster)
library(dplyr)
library(tidyverse)
library(spData)
library(spDataLarge)


# read previous trees dataset
trees <- read_csv("/home/elildojr/Documents/r/primates-and-trees/jamari_trees/all_trees.csv")
trees <- trees %>%
  rename(umf = UMF,
         upa = UPA) %>%
  mutate(umf = tolower(umf),
         upa = tolower(upa),
         upa = gsub("-0", "-", upa))
names(trees)
trees


# the trees dataset is becoming older
# new upas were inventoried and others were logged:
# umf-1 upa-7 logged in 2020
# umf-1 upa-13 logged in 2021
# umf-3 upa-15 inventoried
# umf-3 upa-16 inventoried

# let us add the new data to trees


#----- umf-1 upa-7

# we will have to combine Shaura's SFB shapefile and the SCC csv file to get the coordinates of trees
# otherwise there is no way to left_join it with trees
# read pre-harvest shapefile
JAM1_UPA07 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2020_UPA7/JAM1_UPA7_Arvores_v4.shp")
JAM1_UPA07 <- st_transform(JAM1_UPA07, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA07)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA07 <- bind_cols(JAM1_UPA07,coords)
JAM1_UPA07 <- JAM1_UPA07 %>%
  rename(id_arvore = Árvore,
         especie = Nome_cient)

# read SCC post-harvest csv
jam1_upa07 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2020_UPA_7.csv")
jam1_upa07 <- jam1_upa07 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = `VOLUME M³`)  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora),
         status = "explored") %>%
  group_by(id_arvore, especie_pos, status) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))
jam1_upa07

# join pre and post and fix names to match trees dataset
temp_df1 <- JAM1_UPA07 %>%
  left_join(jam1_upa07, by = c("id_arvore")) %>%
  rename(common_name = Nome_comum,
         dbh = DAP,
         height = Altura,
         species = especie) %>%
  mutate(umf = "umf-1",
         upa = "upa-7",
         year_explored = 2020,
         family = NA,
         genus = NA,
         broad_fruit_type = NA,
         dispersal_category = NA) %>%
  dplyr::select(umf, upa, year_explored, family, genus, species, common_name,
                broad_fruit_type, dbh, height, status, dispersal_category, lon, lat)
temp_df1
temp_df1 %>%
  st_drop_geometry() %>%
  group_by(status) %>%
  count()

# get function to fix tree names
source("/home/elildojr/Documents/r/jamari-trees/bin/useful_functions_trees.R")
temp_df1 <- f.fix.common.names(temp_df1)
sort(unique(temp_df1$common_name))
sort(unique(temp_df1$species))

# test left_join with subset of trees
temp_df2 <- trees %>%
  filter(umf == "umf-1", upa == "upa-7")

# create bindcol
temp_df1$bindcol <- paste(round(temp_df1$lon, 5), round(temp_df1$lat, 6), sep = "")
temp_df1 <- temp_df1 %>%
  st_drop_geometry() %>%
  rename(status2 = status,
         year_explored2 = year_explored) %>% # new status column so we do not mess with the original one
  dplyr::select(bindcol, status2, year_explored2)
temp_df1
temp_df2$bindcol <- paste(round(temp_df2$lon, 5), round(temp_df2$lat, 6), sep = "")
# left_join
temp_df3 <- left_join(temp_df2, temp_df1, by="bindcol")
temp_df3
temp_df3 %>%
  group_by(status2) %>%
  count()
temp_df3 <- temp_df3 %>% 
  mutate(status = status2,
         year_explored = year_explored2) %>% 
  dplyr::select(-c(bindcol, status2, year_explored2))
head(temp_df3)
temp_df3 %>%
  group_by(status) %>%
  count()


# finally, remove data from upa-7 from the tree dataset and replace with temp_df3!
trees <- trees %>%
  filter(!(umf == "umf-1" & upa == "upa-7"))

trees %>%
  group_by(umf, upa) %>%
  count() %>%
  print(n=Inf)

# finally, bind jam1_upa13 to trees dataset
trees <- trees %>%
  bind_rows(temp_df3)
trees



#----- umf-1 upa-13 (logged in 2021)

# check:
trees %>%
  filter(umf == "umf-1", upa == "upa-13") # there is no data from this UPA
# in this case we have to read the new file and put in the same format as trees
# and then use bind_rows()

# read pre-harvest shapefile
JAM1_UPA13 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2021_UPA13/JAM1_UPA13_IF.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA13 <- st_transform(JAM1_UPA13, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA13)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA13 <- bind_cols(JAM1_UPA13,coords)
JAM1_UPA13 <- JAM1_UPA13 %>%
  rename(id_arvore = Árvore,
         especie = Nome_cient)

# read SCC post-harvest csv
jam1_upa13 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2021_UPA_13.csv")
jam1_upa13 <- jam1_upa13 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = `VOLUME M³`)  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora),
         status = "explored") %>%
  group_by(id_arvore, especie_pos, status) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))
jam1_upa13

# join pre and post and fix names to match trees dataset
jam1_upa13 <- JAM1_UPA13 %>%
  left_join(jam1_upa13, by = c("id_arvore")) %>%
  rename(common_name = Nome_comum,
         species = especie,
         dbh = DAP,
         height = Altura) %>%
  mutate(umf = "umf-1",
         upa = "upa-13",
         year_explored = 2021,
         family = NA,
         genus = NA,
         broad_fruit_type = NA,
         dispersal_category = NA) %>%
  dplyr::select(umf, upa, year_explored, family, genus, species, common_name,
                broad_fruit_type, dbh, height, status, dispersal_category, lon, lat) %>%
  st_drop_geometry()
jam1_upa13

# get function to fix tree names
source("/home/elildojr/Documents/r/jamari-trees/bin/useful_functions_trees.R")
jam1_upa13 <- f.fix.common.names(jam1_upa13)
sort(unique(jam1_upa13$common_name))
sort(unique(jam1_upa13$species))
jam1_upa13[which(jam1_upa13$species == "A identificar"),]

#NB! later we will have to fix species names and add family, genus fruit type etc

# finally, bind jam1_upa13 to trees dataset
trees <- trees %>%
  bind_rows(jam1_upa13)
trees



#----- umf-3 upa-15 (inventoried... and logged?)
#----- umf-3 upa-16 (inventoried... and logged?)

# I have post-harvest data from 15 and 15... but no pre-harvest
# anyway there are no camera points at these UPAs so


# save updated trees dataset
write.csv(trees, here("data", "all_trees_updated_feb2022.csv"), row.names = FALSE)
