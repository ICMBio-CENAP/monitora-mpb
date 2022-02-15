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
         upa = tolower(upa) )
names(trees)
trees

# the trees dataset is becoming older
# new upas were inventoried and others were logged:
# umf-1 upa-7 logged in 2020
# umf-1 upa-13 logged in 2021
# umf-3 upa-15 inventoried
# umf-3 upa-16 inventoried

# let us add the new data to trees

# umf-1 upa-7
# we will have to combine Shaura's SFB shapefile and the SCC csv file to get the coordinates of trees
# otherwise there is no way to left_join it with trees
# read pre-harvest shapefile
JAM1_UPA07 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2020_UPA7/JAM1_UPA7_Arvores_v4.shp")
JAM1_UPA07 <- st_transform(JAM1_UPA07, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA07)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA07 <- bind_cols(JAM1_UPA07,coords)
# read SCC post-harvest csv
jam1_upa07 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2020_UPA_7.csv")
# standardize tree id and join
JAM1_UPA07 <- JAM1_UPA07 %>%
  rename(id_arvore = Árvore,
         bin = Nome_cient)

jam1_upa07 <- jam1_upa07 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`) %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore) ) %>%
  group_by(id_arvore, lat, lon) %>%

jam1_upa07 <- left_join(JAM1_UPA07, jam1_upa07, by = ("id_arvore")) %>%
  filter(!is.na(UPA)) %>%
  select(lon, lat, SEÇÃO) %>% # SEÇÃO is just for checking
  st_drop_geometry()


test <- trees %>%
  filter(umf == "umf-1", upa == "upa-07")
test

test2 <- left_join(test, jam1_upa07, by = c("lat", "lon"))
jam1_upa07 %>%
  group_by(SEÇÃO) %>%
  count()


jam1_upa07 <- jam1_upa07 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2007,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa07 <- left_join(JAM1_UPA07, jam1_upa07, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa07

# read pre-harvest shapefile
JAM1_UPA07 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2020_UPA7/JAM1_UPA7_Arvores_v4.shp")

dim(JAM1_UPA07)
trees %>%
  filter(UMF == "UMF-1", UPA == "UPA-07") %>%
  nrow()

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA07 <- st_transform(JAM1_UPA07, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA07)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA07 <- bind_cols(JAM1_UPA07,coords)

head(JAM1_UPA07)

# fix column names and formats
names(JAM1_UPA07) # check names
JAM1_UPA07 <- JAM1_UPA07 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Área_basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-7",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

n_before<- dim(JAM1_UPA07)[1]

# post-harvest
jam1_upa07 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2020_UPA_7.csv")
jam1_upa07 <- jam1_upa07 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2007,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa07 <- left_join(JAM1_UPA07, jam1_upa07, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa07

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa07)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa07)