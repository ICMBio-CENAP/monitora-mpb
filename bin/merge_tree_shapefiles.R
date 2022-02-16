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


# read and fix UPA tree data from UMF-1 and UMF-2

##---------- UMF 1 ----------#

#-----UPA-01

# read pre-harvest shapefile
#JAM1_UPA01 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2010_UPA1/UPA_1_Arvores.shp")
JAM1_UPA01 <- st_read("/media/elildojr/elildojr/Dados-2021-12-20/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA01_wgs84.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA01 <- st_transform(JAM1_UPA01, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA01)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA01 <- bind_cols(JAM1_UPA01,coords)

# fix column names and formats
names(JAM1_UPA01) # check names
JAM1_UPA01 <- JAM1_UPA01 %>%
  rename(id_arvore = Árv,
         especie_pre = Nome_Cient,
         nome_comum = Nome_Vulga,
         dap = DAP,
         area_basal = Área_Basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-1",
         status = NA) %>%
  dplyr::select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)
head(JAM1_UPA01)


# post-harvest
jam1_upa01 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/UMF1_UPA1_Campos_Movimentação.csv")
jam1_upa01 <- jam1_upa01 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))
jam1_upa01

# join pre and post-harvest
jam1_upa01 <- left_join(JAM1_UPA01, jam1_upa01, by = "id_arvore") %>%
  dplyr::select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa01

# check if dap is in metres
summary(jam1_upa01$dap, na.rm=TRUE)

# compare especie_pre and especie_pos to see if left_join was correct
print(jam1_upa01[!is.na(jam1_upa01$especie_pos),], n=30)

# check map
options(sf_max.plot=1)
plot(jam1_upa01)


#-----UPA-02

# read pre-harvest shapefile
#JAM1_UPA02 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2011_UPA2/UPA_2_Arvores.shp")
JAM1_UPA02 <- st_read("/media/elildojr/elildojr/Dados-2021-12-20/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA02_wgs84.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA02 <- st_transform(JAM1_UPA02, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA02)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA02 <- bind_cols(JAM1_UPA02,coords)
head(JAM1_UPA02)

# fix column names and formats
names(JAM1_UPA02) # check names
JAM1_UPA02 <- JAM1_UPA02 %>%
  rename(id_arvore = Individuo) %>%
  mutate(umf = "umf-1",
         upa = "upa-2",
         especie_pre = NA,
         nome_comum = NA,
         dap = NA,
         area_basal = NA,
         altura = NA,
         volume = NA,
         status = NA) %>%
  dplyr::select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, 
         volume, status)

n_before <- dim(JAM1_UPA02)[1]

# post-harvest
jam1_upa02 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/UMF1_UPA2_Campos_Movimentação.csv")
jam1_upa02 <- jam1_upa02 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))
jam1_upa02

# join pre and post-harvest
jam1_upa02 <- left_join(JAM1_UPA02, jam1_upa02, by = "id_arvore") %>%
  dplyr::select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa02

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa02)[1]
n_before-n_after

# check if UPA was harvested in more than a single year
table(jam1_upa02$ano_exploracao)

# check if dap is in metres
summary(jam1_upa02$dap, na.rm=TRUE)

# check
options(sf_max.plot=1)
plot(jam1_upa02)


#-----UPA-03
# shapefile for upa 03 is missing, use this alternative
JAM1_UPA03 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2012_UPA3/JAM1_UPA03_wgs84.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA03 <- st_transform(JAM1_UPA03, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA03)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA03 <- bind_cols(JAM1_UPA03,coords)

head(JAM1_UPA03)

# fix column names and formats
names(JAM1_UPA03) # check names
JAM1_UPA03 <- JAM1_UPA03 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_Cient,
         nome_comum = Nome_Vulga,
         dap = DAP,
         area_basal = Área_Basa,
         altura = Alt,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-4",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

n_before <- dim(JAM1_UPA03)[1]

# post-harvest
jam1_upa03 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/UMF1_UPA3_Campos_Movimentação.csv")
jam1_upa03 <- jam1_upa03 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))
jam1_upa03

# join pre and post-harvest
jam1_upa03 <- left_join(JAM1_UPA03, jam1_upa03, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, data_corte, n_toras, volume,
         lat, lon)
jam1_upa03

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa03)[1]
n_before-n_after

# check if dap is in metres
summary(jam1_upa01$dap, na.rm=TRUE)

# check
options(sf_max.plot=1)
plot(jam1_upa03)

# there is a problem: infinite values in geometry
# remove them
st_bbox(JAM1_UPA03) # check bounding box: there are Inf values
# redefine bounding box
newXmax <- tail(sort((st_coordinates(st_centroid(JAM1_UPA03))[,1])))
newXmax <- max(newXmax[!is.infinite(newXmax)])
newYmax <- tail(sort((st_coordinates(st_centroid(JAM1_UPA03))[,2])))
newYmax <- max(newYmax[!is.infinite(newYmax)])
# create a polygon to clip off outliers (Inf values) 
bb <- st_bbox(JAM1_UPA03)
bb[3] <- newXmax
bb[4] <- newYmax
# Make this a polygon
bpoly <- st_as_sfc(bb)
# Crop JAM1_UPA03 data with this polygon
JAM1_UPA03 <- st_intersection(JAM1_UPA03, bpoly)
# Plot it
plot(JAM1_UPA03)


#-----UPA-04

# read pre-harvest shapefile
#
JAM1_UPA04 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2013_UPA4/Árvores2.shp")
# not reading Shaura SFB shapefile, use this alternative
#JAM1_UPA04 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2013_UPA4/JAM1_UPA04_wgs84.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA04 <- st_transform(JAM1_UPA04, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA04)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA04 <- bind_cols(JAM1_UPA04,coords)

head(JAM1_UPA04)

# fix column names and formats
names(JAM1_UPA04) # check names
JAM1_UPA04 <- JAM1_UPA04 %>%
  rename(id_arvore = INDICE) %>% # assuming this is the ID column
  mutate(umf = "umf-1",
         upa = "upa-4",
         status = NA,
         especie_pre = NA,
         nome_comum = NA,
         dap = NA,
         area_basal = NA,
         altura = NA,
         volume = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

# check trees were lost or duplicated
n_before <- dim(JAM1_UPA04)[1]

# post-harvest
jam1_upa04 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/UMF1_UPA4_Campos_Movimentação.csv")
jam1_upa04 <- jam1_upa04 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa04 <- left_join(JAM1_UPA04, jam1_upa04, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, data_corte, n_toras, volume,
         lat, lon)
jam1_upa04

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa04)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa04)



#-----UPA-05

# read pre-harvest shapefile
JAM1_UPA05 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2014_UPA5/ÁRVORE_SIRGAS.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA05 <- st_transform(JAM1_UPA05, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA05)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA05 <- bind_cols(JAM1_UPA05,coords)

head(JAM1_UPA05)

# fix column names and formats
names(JAM1_UPA05) # check names
JAM1_UPA05 <- JAM1_UPA05 %>%
  rename(id_arvore = INDICE, # assuming this is the ID column
         nome_comum = Espécie,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-5",
         especie_pre = NA,
         dap = NA,
         area_basal = NA,
         altura = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

n_before <- dim(JAM1_UPA05)[1]

# post-harvest
# MISSING DATA!

# check trees were lost or duplicated
#n_before 
#n_after <- dim(jam1_upa05)[1]
#n_before-n_after


#-----UPA-06

# read pre-harvest shapefile
JAM1_UPA06 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2016_UPA6/00_Árvores.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA06 <- st_transform(JAM1_UPA06, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA06)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA06 <- bind_cols(JAM1_UPA06,coords)

head(JAM1_UPA06)

# fix column names and formats
names(JAM1_UPA06) # check names
JAM1_UPA06 <- JAM1_UPA06 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Área_basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-6",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

n_before <- dim(JAM1_UPA06)[1]

# post-harvest
jam1_upa06 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2016_UPA_6.csv")
jam1_upa06 <- jam1_upa06 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2016,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa06 <- left_join(JAM1_UPA06, jam1_upa06, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa06

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa06)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa06)


#-----UPA-07

# read pre-harvest shapefile
JAM1_UPA07 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2020_UPA7/JAM1_UPA7_Arvores_v4.shp")

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


#-----UPA-08

# read pre-harvest shapefile
JAM1_UPA08 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2019_UPA8/JAM1_UPA8_Arvores.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA08 <- st_transform(JAM1_UPA08, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA08)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA08 <- bind_cols(JAM1_UPA08,coords)

head(JAM1_UPA08)

# fix column names and formats
names(JAM1_UPA08) # check names
JAM1_UPA08 <- JAM1_UPA08 %>%
  rename(id_arvore = Arvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Area_basal,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-8",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

n_before <- dim(JAM1_UPA08)[1]

# post-harvest
jam1_upa08 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2019_UPA_8.csv")
jam1_upa08 <- jam1_upa08 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2019,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa08 <- left_join(JAM1_UPA08, jam1_upa08, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa08

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa08)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa08)


#-----UPA-09

# read pre-harvest shapefile
JAM1_UPA09 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2018_UPA9/09_Arvores.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA09 <- st_transform(JAM1_UPA09, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA09)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA09 <- bind_cols(JAM1_UPA09,coords)

head(JAM1_UPA09)

# fix column names and formats
names(JAM1_UPA09) # check names
JAM1_UPA09 <- JAM1_UPA09 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Área_basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-9",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

n_before <- dim(JAM1_UPA09)[1]

# post-harvest
jam1_upa09 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2018_UPA_9.csv")
jam1_upa09 <- jam1_upa09 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2018,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa09 <- left_join(JAM1_UPA09, jam1_upa09, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa09

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa09)[1]
n_before-n_after


# check
options(sf_max.plot=1)
plot(jam1_upa09)


#-----UPA-10

# read pre-harvest shapefile
JAM1_UPA10 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2017_UPA10/00_Árvore.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA10 <- st_transform(JAM1_UPA10, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA10)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA10 <- bind_cols(JAM1_UPA10,coords)

head(JAM1_UPA10)
n_before <- dim(JAM1_UPA10)[1]

# fix column names and formats
names(JAM1_UPA10) # check names
JAM1_UPA10 <- JAM1_UPA10 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Área_basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-10",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

# post-harvest
jam1_upa10 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2017_UPA_10.csv")
jam1_upa10 <- jam1_upa10 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2017,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa10 <- left_join(JAM1_UPA10, jam1_upa10, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa10

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa10)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa10)



#-----UPA-11

# read pre-harvest shapefile
JAM1_UPA11 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2015_UPA11/1_Arvores.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA11 <- st_transform(JAM1_UPA11, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA11)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA11 <- bind_cols(JAM1_UPA11,coords)

head(JAM1_UPA11)
n_before <- dim(JAM1_UPA11)[1]

# fix column names and formats
names(JAM1_UPA11) # check names
JAM1_UPA11 <- JAM1_UPA11 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Área_basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-11",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

# post-harvest
jam1_upa11 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2015_UPA_11.csv")
jam1_upa11 <- jam1_upa11 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2015,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa11 <- left_join(JAM1_UPA11, jam1_upa11, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa11

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa11)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa11)


#-----UPA-13

# read pre-harvest shapefile
JAM1_UPA13 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2021_UPA13/JAM1_UPA13_IF.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM1_UPA13 <- st_transform(JAM1_UPA13, 4326)
coords <- data.frame(st_coordinates(JAM1_UPA13)) %>%
  rename(lat = Y,
         lon = X)
JAM1_UPA13 <- bind_cols(JAM1_UPA13,coords)

head(JAM1_UPA13)
n_before <- dim(JAM1_UPA13)[1]

# fix column names and formats
names(JAM1_UPA13) # check names
JAM1_UPA13 <- JAM1_UPA13 %>%
  rename(id_arvore = Árvore,
         especie_pre = Nome_cient,
         nome_comum = Nome_comum,
         dap = DAP,
         area_basal = Área_basa,
         altura = Altura,
         volume = Volume) %>%
  mutate(umf = "umf-1",
         upa = "upa-13",
         status = NA) %>%
  select(umf, upa, id_arvore, especie_pre, nome_comum, lat, lon, dap, area_basal, altura, volume,
         status)

# post-harvest
jam1_upa13 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/Relatorio_Exploração_por_Safra_JAM_UMF_I_2021_UPA_13.csv")
jam1_upa13 <- jam1_upa13 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         #data_corte = `Data Corte`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = str_remove(id_arvore, "^0+"), # remove leading zeros
         id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2021,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam1_upa13 <- left_join(JAM1_UPA13, jam1_upa13, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, volume,
         status, data_corte, n_toras, volume_toras,
         lat, lon)
jam1_upa13

# check trees were lost or duplicated
n_before 
n_after <- dim(jam1_upa13)[1]
n_before-n_after

# check
options(sf_max.plot=1)
plot(jam1_upa13)

##-----

# join UMF-1 shapefiles into a single file
jam1 <- bind_rows(jam1_upa01, jam1_upa02, jam1_upa03, jam1_upa04, jam1_upa06,
          jam1_upa07, jam1_upa08, jam1_upa09, jam1_upa10, jam1_upa11, jam1_upa13)
# NB! jam1_upa05 is not on the list because it lacked post-harvest data
dim(jam1)

# just a check:
# compare especie_pre and especie_pos to see if left_join was correct
head(jam1_upa01[!is.na(jam1_upa01$especie_pos),])
head(jam1_upa02[!is.na(jam1_upa02$especie_pos),])
head(jam1_upa03[!is.na(jam1_upa03$especie_pos),])
head(jam1_upa04[!is.na(jam1_upa04$especie_pos),])
head(jam1_upa06[!is.na(jam1_upa06$especie_pos),])
head(jam1_upa07[!is.na(jam1_upa07$especie_pos),])
head(jam1_upa08[!is.na(jam1_upa08$especie_pos),])
head(jam1_upa09[!is.na(jam1_upa09$especie_pos),])
head(jam1_upa10[!is.na(jam1_upa10$especie_pos),])
head(jam1_upa11[!is.na(jam1_upa11$especie_pos),])
head(jam1_upa13[!is.na(jam1_upa13$especie_pos),])



##---------- UMF 3 ----------#

#-----UPA-01

# read pre-harvest shapefile
JAM3_UPA01 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2011_UPA1/censo.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA01 <- st_transform(JAM3_UPA01, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA01)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA01 <- bind_cols(JAM3_UPA01,coords)

head(JAM3_UPA01)
n_before <- dim(JAM3_UPA01)[1]

# fix column names and formats
names(JAM3_UPA01) # check names
JAM3_UPA01 <- JAM3_UPA01 %>%
  rename(id_arvore = Nº_da_Árv_,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__m_,
         altura = Altura__m_,
         volume = Volume_IBD) %>%
  mutate(umf = "umf-3",
         upa = "upa-1",
         id_arvore = as.numeric(id_arvore),
         area_basal = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa01 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/UMF3_UPA1_Campos_Movimentação.csv")
jam3_upa01 <- jam3_upa01 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa01 <- left_join(JAM3_UPA01, jam3_upa01, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa01

# check trees were lost or duplicated
n_before 
n_after <- dim(jam3_upa01)[1]
n_before-n_after

print(jam3_upa01[!is.na(jam3_upa01$especie_pos),], n=50)
# NB! pre and post names do not match!!!!
# this mean we cant use id_arvore to match the two datasets!

# check
options(sf_max.plot=1)
plot(jam3_upa01)



#-----UPA-02

# read pre-harvest shapefile
# Shaura's shapefile for upa 02 is missing, use this alternative
JAM3_UPA02 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/UPA2/JAM3_UPA02_wgs84.shp")


# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA02 <- st_transform(JAM3_UPA02, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA02)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA02 <- bind_cols(JAM3_UPA02,coords)

head(JAM3_UPA02)
n_before <- dim(JAM3_UPA02)[1]

# fix column names and formats
names(JAM3_UPA02) # check names
JAM3_UPA02 <- JAM3_UPA02 %>%
  rename(id_arvore = Nº_da_Ár,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__cm_,
         altura = Altura__m_,
         volume = Volume_Aju) %>%
  mutate(umf = "umf-3",
         upa = "upa-2",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         volume = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa02 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/UMF3_UPA2_Campos_Movimentação.csv")
jam3_upa02 <- jam3_upa02 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa02 <- left_join(JAM3_UPA02, jam3_upa02, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa02

# check trees were lost or duplicated
n_before 
n_after <- dim(jam3_upa02)[1]
n_before-n_after

print(jam3_upa02[!is.na(jam3_upa02$especie_pos),], n=50)

# check
options(sf_max.plot=1)
plot(jam3_upa02)


#-----UPA-03

# read pre-harvest shapefile
# Shaura's shapefile for upa 03 is missing, use this alternative
JAM3_UPA03 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/UPA3/JAM3_UPA03_wgs84.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA03 <- st_transform(JAM3_UPA03, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA03)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA03 <- bind_cols(JAM3_UPA03,coords)

head(JAM3_UPA03)
n_before <- dim(JAM3_UPA03)[1]

# fix column names and formats
names(JAM3_UPA03) # check names
JAM3_UPA03 <- JAM3_UPA03 %>%
  rename(id_arvore = Nº_da_Ár,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__cm_,
         altura = Altura__m_,
         volume = Vol_aj__CS) %>%
  mutate(umf = "umf-3",
         upa = "upa-3",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         volume = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa03 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/UMF3_UPA3_Campos_Movimentação.csv")
jam3_upa03 <- jam3_upa03 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa03 <- left_join(JAM3_UPA03, jam3_upa03, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa03

# check trees were lost or duplicated
n_before 
n_after <- dim(jam3_upa03)[1]
n_before-n_after

print(jam3_upa03[!is.na(jam3_upa03$especie_pos),], n=50)
# NB! pre and post names do not match!!!!
# this mean we cant use id_arvore to match the two datasets!

# check
options(sf_max.plot=1)
plot(jam3_upa03)




#-----UPA-04

# read pre-harvest shapefile
JAM3_UPA04 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2014_UPA4/UPA04_censo_BD2.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA04 <- st_transform(JAM3_UPA04, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA04)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA04 <- bind_cols(JAM3_UPA04,coords)

head(JAM3_UPA04)
n_before <- dim(JAM3_UPA04)[1]

# fix column names and formats
names(JAM3_UPA04) # check names
JAM3_UPA04 <- JAM3_UPA04 %>%
  rename(id_arvore = Nº_da_Árv_,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__cm_,
         altura = Altura__m_,
         volume = Volume_aj) %>%
  mutate(umf = "umf-3",
         upa = "upa-4",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         volume = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
# NB! post-harvest data is missing from Shaura's SFB files
jam3_upa04 <- read_csv("")
jam3_upa04 <- jam3_upa04 %>%
  rename(id_arvore = Árvore,
         especie_pos = Espécie,
         data_corte = `Data Corte`,
         volume_tora = `Volume Tora`)  %>%
  mutate(data_corte = as.Date(data_corte, format = "%d/%m/%Y"),
         ano_exploracao = lubridate::year(data_corte),
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa04 <- left_join(JAM3_UPA04, jam3_upa04, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa04

# check trees were lost or duplicated
n_before 
n_after <- dim(jam3_upa04)[1]
n_before-n_after

print(jam3_upa04[!is.na(jam3_upa04$especie_pos),], n=50)
# NB! pre and post names do not match!!!!
# this mean we cant use id_arvore to match the two datasets!

# check
options(sf_max.plot=1)
plot(jam3_upa04)




#-----UPA-05

# read pre-harvest shapefile
JAM3_UPA05 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2015_UPA5/id_arvores.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA05 <- st_transform(JAM3_UPA05, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA05)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA05 <- bind_cols(JAM3_UPA05,coords)

# fix column names and formats
names(JAM3_UPA05) # check names
JAM3_UPA05 <- JAM3_UPA05 %>%
  rename(id_arvore = num_id_arvore,
         nome_comum = nom_com,
         especie_pre = nom_cient,
         dap = dap_cm,
         altura = altura_m) %>%
  mutate(umf = "umf-3",
         upa = "upa-5",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         volume = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa05 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_III_2015_UPA_5.csv")
jam3_upa05 <- jam3_upa05 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2015,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa05 <- left_join(JAM3_UPA05, jam3_upa05, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa05

# check
options(sf_max.plot=1)
plot(jam3_upa05)






#-----UPA-06

# read pre-harvest shapefile
JAM3_UPA06 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2016_UPA6/id_arvores3.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA06 <- st_transform(JAM3_UPA06, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA06)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA06 <- bind_cols(JAM3_UPA06,coords)

# fix column names and formats
names(JAM3_UPA06) # check names
JAM3_UPA06 <- JAM3_UPA06 %>%
  rename(id_arvore = num_id_arvore,
         nome_comum = nom_com,
         especie_pre = nom_cient,
         dap = dap_cm,
         altura = altura_m) %>%
  mutate(umf = "umf-3",
         upa = "upa-6",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         volume = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa06 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_III_2016_UPA_6.csv")
jam3_upa06 <- jam3_upa06 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2016,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa06 <- left_join(JAM3_UPA06, jam3_upa06, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa06

# check
options(sf_max.plot=1)
plot(jam3_upa06)



#-----UPA-11

# read pre-harvest shapefile
JAM3_UPA11 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2018_UPA11/UPA_11_Censo.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA11 <- st_transform(JAM3_UPA11, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA11)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA11 <- bind_cols(JAM3_UPA11,coords)

# fix column names and formats
names(JAM3_UPA11) # check names
JAM3_UPA11 <- JAM3_UPA11 %>%
  rename(id_arvore = Id_Árvore,
         nome_comum = Espécie_C,
         especie_pre = Field10,
         dap = DAP,
         altura = Altura,
         volume = Volume_Est) %>%
  mutate(umf = "umf-3",
         upa = "upa-11",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa11 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_III_2018_UPA_11.csv")
jam3_upa11 <- jam3_upa11 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2018,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa11 <- left_join(JAM3_UPA11, jam3_upa11, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa11

# check
options(sf_max.plot=1)
plot(jam3_upa11)



#-----UPA-12

# read pre-harvest shapefile
JAM3_UPA12 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2019_UPA12/UPA_12_Censo.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA12 <- st_transform(JAM3_UPA12, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA12)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA12 <- bind_cols(JAM3_UPA12,coords)

# fix column names and formats
names(JAM3_UPA12) # check names
JAM3_UPA12 <- JAM3_UPA12 %>%
  rename(id_arvore = Id,
         nome_comum = Espécie_C,
         especie_pre = Espécie_1,
         dap = DAP,
         altura = Altura,
         volume = Volume_Est) %>%
  mutate(umf = "umf-3",
         upa = "upa-12",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa12 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_III_2019_UPA_12.csv")
jam3_upa12 <- jam3_upa12 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2019,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa12 <- left_join(JAM3_UPA12, jam3_upa12, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa12

# check
options(sf_max.plot=1)
plot(jam3_upa12)


#-----UPA-14

# read pre-harvest shapefile
JAM3_UPA14 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2017_UPA14/UPA_14_Censo.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA14 <- st_transform(JAM3_UPA14, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA14)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA14 <- bind_cols(JAM3_UPA14,coords)

# fix column names and formats
names(JAM3_UPA14) # check names
JAM3_UPA14 <- JAM3_UPA14 %>%
  rename(id_arvore = Nº_da_Ár,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__cm_,
         altura = Altura__m_,
         volume = Volume_Aju) %>%
  mutate(umf = "umf-3",
         upa = "upa-14",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa14 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_III_2017_UPA_14.csv")
jam3_upa14 <- jam3_upa14 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2017,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa14 <- left_join(JAM3_UPA14, jam3_upa14, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa14

# check
options(sf_max.plot=1)
plot(jam3_upa14)




#-----UPA-15

# read pre-harvest shapefile
# Shaura's shapefile for upa 15 is missing, use this alternative
#JAM3_UPA15 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2017_UPA15/UPA_15_Censo.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA15 <- st_transform(JAM3_UPA15, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA15)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA15 <- bind_cols(JAM3_UPA15,coords)

# fix column names and formats
names(JAM3_UPA15) # check names
JAM3_UPA15 <- JAM3_UPA15 %>%
  rename(id_arvore = Nº_da_Ár,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__cm_,
         altura = Altura__m_,
         volume = Volume_Aju) %>%
  mutate(umf = "umf-3",
         upa = "upa-15",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa15 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_IV_2021_UPA_15.csv")
jam3_upa15 <- jam3_upa15 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2021,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa15 <- left_join(JAM3_UPA15, jam3_upa15, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa15

# check
options(sf_max.plot=1)
plot(jam3_upa15)




#-----UPA-16

# read pre-harvest shapefile
# Shaura's shapefile for upa 16 is missing
#JAM3_UPA16 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_III/2017_UPA16/UPA_16_Censo.shp")

# convert from utm to decimal degrees and add lat lon columns
JAM3_UPA16 <- st_transform(JAM3_UPA16, 4326)
coords <- data.frame(st_coordinates(JAM3_UPA16)) %>%
  rename(lat = Y,
         lon = X)
JAM3_UPA16 <- bind_cols(JAM3_UPA16,coords)

# fix column names and formats
names(JAM3_UPA16) # check names
JAM3_UPA16 <- JAM3_UPA16 %>%
  rename(id_arvore = Nº_da_Ár,
         nome_comum = Nome_Vulga,
         especie_pre = Nome_cient,
         dap = DAP__cm_,
         altura = Altura__m_,
         volume = Volume_Aju) %>%
  mutate(umf = "umf-3",
         upa = "upa-16",
         id_arvore = as.numeric(id_arvore),
         dap = dap/100,
         area_basal = NA,
         status = NA) %>%
  select(umf, upa, id_arvore, lat, lon, especie_pre, nome_comum, dap, area_basal, altura, 
         volume, status)

# post-harvest
jam3_upa16 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_III/Relatorio_Exploração_por_Safra_JAM_UMF_IV_2020_UPA_16.csv")
jam3_upa16 <- jam3_upa16 %>%
  rename(id_arvore = `NÚMERO DA ÁRVORE`,
         especie_pos = `NOME CIENTÍFICO`,
         volume_tora = "VOLUME M³")  %>%
  mutate(id_arvore = as.numeric(id_arvore),
         data_corte = NA,
         ano_exploracao = 2020,
         volume_tora = str_replace(volume_tora, ",", "."),
         volume_tora = as.numeric(volume_tora)) %>%
  group_by(id_arvore, especie_pos, data_corte, ano_exploracao) %>%
  summarize(n_toras = n(),
            volume_toras = sum(volume_tora, na.rm = TRUE))

# join pre and post-harvest
jam3_upa16 <- left_join(JAM3_UPA16, jam3_upa16, by = "id_arvore") %>%
  select(umf, upa, ano_exploracao, id_arvore, especie_pre, especie_pos, nome_comum,
         dap, area_basal, altura, 
         status, volume, data_corte, n_toras, volume_toras,
         lat, lon)
jam3_upa16

# check
options(sf_max.plot=1)
plot(jam3_upa16)


##-----

# join UMF-3 shapefiles into a single file
jam3 <- bind_rows(jam3_upa01, jam3_upa02, jam3_upa03, jam3_upa05, jam3_upa06,
                  jam3_upa11, jam3_upa12, jam3_upa14)
# NB! jam3_upa04, jam3_upa15, jam3_upa16 is not on the list because it lacked post-harvest data
dim(jam3)



##--------------------------------------------------
#--------------------------------------------------

# join everything into a single file
jam <- bind_rows(jam1, jam3)
# NB! jam3_upa04, jam3_upa15, jam3_upa16 is not on the list because it lacked post-harvest data
dim(jam)
options(sf_max.plot=1)
plot(jam)
# there is a problem: infinite values in geometry
# remove them
st_bbox(jam) # check bounding box: there are Inf values
# redefine bounding box
newXmax <- sort((st_coordinates(st_centroid(jam))[,1]))
newXmax <- max(newXmax[!is.infinite(newXmax)])
newYmax <- sort((st_coordinates(st_centroid(jam))[,2]))
newYmax <- max(newYmax[!is.infinite(newYmax)])
# create a polygon to clip off outliers (Inf values) 
bb <- st_bbox(jam)
bb[3] <- newXmax
bb[4] <- newYmax
# Make this a polygon
bpoly <- st_as_sfc(bb)
# Crop JAM1_UPA04 data with this polygon
jam <- st_intersection(jam, bpoly)
# Plot it
plot(jam)

#----------------------------
# compare with previous trees dataset
trees <- read_csv("/home/elildojr/Documents/r/primates-and-trees/jamari_trees/all_trees.csv")
trees %>% 
  group_by(UMF, UPA) %>%
  count()

trees %>%
  filter(UMF == "UMF-1", UPA == "UPA-01") %>%
  group_by(status) %>%
  count()

trees %>%
  filter(UMF == "UMF-3", UPA == "UPA-02") %>%
  group_by(status) %>%
  count()

trees %>%
  filter(UMF == "UMF-3", UPA == "UPA-03") %>%
  group_by(status) %>%
  count()

trees %>%
  group_by(UMF, UPA, status) %>%
  count() %>% print(n=Inf)


# trees does not have id_arvore ID for matching
# try matching using lat lon, but pay attention to decimal precision
test <- left_join(jam, trees, by = c("lat", "lon"))
dim(jam)
dim(trees)
dim(test)
test %>% filter(status2 == "explored") %>%
  select(especie_pos, species, dap, dbh) %>%
  print(n=100)

jam <- jam %>%
  filter(!is.na(id_arvore))

jam %>% nrow()

jam %>%
  filter(is.na(id_arvore)) %>% nrow()

jam %>%
  distinct(umf, upa, id_arvore) %>% nrow()

#----- write csv for next step -----

jam <- st_drop_geometry(jam) # transform to dataframe for left_join
write.csv(jam, here("data", "trees_merged.csv"), row.names = FALSE)



##--------------------------------------------------
#--------------------------------------------------

#---------- harvested trees ----------#
harvested_all <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/id_arvores-abatidas-2020-10-10.shp")
options(sf_max.plot=1)
plot(harvested_all)

# there is a problem: infinite values in geometry
# remove them
st_bbox(harvested_all) # check bounding box: there are Inf values
# redefine bounding box
newXmax <- sort((st_coordinates(st_centroid(harvested_all))[,1]))
newXmax <- max(newXmax[!is.infinite(newXmax)])
newYmax <- sort((st_coordinates(st_centroid(harvested_all))[,2]))
newYmax <- max(newYmax[!is.infinite(newYmax)])
# create a polygon to clip off outliers (Inf values) 
bb <- st_bbox(harvested_all)
bb[3] <- newXmax
bb[4] <- newYmax
# Make this a polygon
bpoly <- st_as_sfc(bb)
# Crop JAM1_UPA04 data with this polygon
harvested_all <- st_intersection(harvested_all, bpoly)
# Plot it
plot(harvested_all)

# some fixes
harvested_all <- harvested_all %>%
  rename(especie = bin,
         nome_comum = nome.comum,
         dap = DAP,
         area_basal = area.basal) %>%
  select(especie, nome_comum, dap, area_basal, altura, volume, status, lon, lat)











# standardize values
unique(harvested_all$status)

# small fix
names(harvested_all)[12] <- "lon"

# simplify it
harvested_all <- harvested_all[,-c(1,2,14)]
head(harvested_all)
summary(harvested_all$DAP)
harvested_all$DAP <- round(harvested_all$DAP/100, 2)
# some trees have dbh=0, replace by mean values
# tree censuses only considered dbh > 0.4, so: 
harvested_all$DAP[which(harvested_all$DAP < 0.3)] <- mean(harvested_all$DAP)
summary(harvested_all$DAP)

# convert to dataframe
harvested_all <- st_drop_geometry(harvested_all) # transform to dataframe for left_join

# create bindcol
harvested_all$bindcol <- paste(round(harvested_all$lon, 5), round(harvested_all$lat, 6), sep = "")
JAM$bindcol <- paste(round(JAM$lon, 5), round(JAM$lat, 6), sep = "")

# left_join
test1 <- left_join(JAM, harvested_all, by="bindcol")

