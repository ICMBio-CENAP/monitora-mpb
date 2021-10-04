
# load library
library(here)
library(tidyverse)
library(gsheet)

# read from Google Drive
cams <- as_tibble(gsheet2tbl("https://drive.google.com/file/d/1bidbfkJg_4Slnijut0N4kFpW1slExi2h/view?usp=sharing"))
#head(cams)

