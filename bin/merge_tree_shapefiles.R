# merge MADEFLONA and AMATA tree shapefiles
# data is not in a standard format
# columns and units vary depending on concession and year
# fix what can be fixed

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

#---------- UMF 1 ----------#

# read shapefiles
# all trees
JAM1_UPA01 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2010_UPA1/UPA_1_Arvores.shp")
# logged trees
jam1_upa01 <- read_csv("/home/elildojr/Documents/gis/jamari/sfb/scc_reports/UMF_I/UMF1_UPA1_Campos_Movimentação.csv")

JAM1_UPA02 <- st_read("/home/elildojr/Documents/gis/jamari/sfb/tree_shapefiles/UMF_I/2011_UPA2/UPA_2_Arvores.shp")

JAM1_UPA03 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA03_wgs84.shp")
JAM1_UPA04 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA04_wgs84.shp")
JAM1_UPA05 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA05_wgs84.shp")
JAM1_UPA06 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA06_wgs84.shp")
JAM1_UPA07 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA07_wgs84.shp")
JAM1_UPA08 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA08_wgs84.shp")
JAM1_UPA09 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA09_wgs84.shp")
JAM1_UPA10 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA10_wgs84.shp")
JAM1_UPA11 <- st_read("/media/elildojr/Dados/gis/shp/rondonia/Shapes - SFB/UMFI_ArvoresAbatidas/JAM1_UPA11_wgs84.shp")

## standardize attribute (column) names and types (classes)
#JAM1_UPA01
names(JAM1_UPA01) # check names
JAM1_UPA01$UMF <- "UMF-1" 
JAM1_UPA01$UPA <- "UPA-1"
JAM1_UPA01$ano.exploracao <- 2011
#JAM1_UPA01$DAP <- JAM1_UPA01$CAP/pi
names(JAM1_UPA01)[5] <- "bin" # binomial name
names(JAM1_UPA01)[7] <- "nome.comum"
names(JAM1_UPA01)[11] <- "altura"
#names(JAM1_UPA01)[12] <- "basal.area"
#names(JAM1_UPA01)[13] <- "volume"
#names(JAM1_UPA01)[19] <- "destinacao"
names(JAM1_UPA01)[21] <- "X"
names(JAM1_UPA01)[22] <- "Y"
names(JAM1_UPA01)[20] <- "status"
JAM1_UPA01 <- JAM1_UPA01[c("UMF", "UPA", "ano.exploracao", "bin", "nome.comum", "altura", "DAP", "status")]
options(sf_max.plot=1)
plot(JAM1_UPA01)
summary(JAM1_UPA01$DAP) # metres


