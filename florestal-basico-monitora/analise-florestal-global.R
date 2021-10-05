
#----- load library
library(here)
library(tidyverse)
library(stringr)
library(gsheet)

## ---- source this file
source(here("bin", "ahumada_codes.R"))

#----- read data

# read monitora data
monitora <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1E-KYF0O3Jf695wlnMHsPVMn2OBAFIzJBbt--BOJz3Is/edit?usp=sharing"))
monitora <- monitora %>% 
  rename(uc_name = "Local - Nome da Unidade de Conservação",
         station = "Número da Estação Amostral",
         effort = "Esforço de amostragem tamanho da trilha (m)",
         date = "data da amostragem",
         year = "Ano",
         class = "Classe", order = "Ordem", family="Família", genus = "Gênero",
         species = "Espécies validadas para análise do ICMBio",
         group_size = "n° de animais", distance = "distância (m)     do animal em relação a trilha") %>%
  mutate(uc_name=as.factor(uc_name), station=as.character(station),
         effort=as.numeric(effort/1000), date = as.Date(date, format = "%d/%m/%Y"),
         year=as.numeric(year), group_size=as.numeric(group_size), distance=as.numeric(distance) ) %>%
  select(uc_name, station, effort, date, year, class, order, family, genus,
         species, group_size, distance) %>%
  filter(year!=2020)
monitora


# how many protected areas per year
adesao <- monitora %>%
  distinct(uc_name, year) %>%
  count(year) %>%
  arrange(year)
adesao
with(adesao, plot(year, n, type = "b",  las=1, ylim=c(0, max(n+5)),
                  pch = 19, xlab = "Ano", ylab = "UCs com protocolo"))

# how many stations per year
estacoes <- monitora %>%
  distinct(uc_name, station, year) %>%
  count(year) %>%
  arrange(year)
estacoes
with(estacoes, plot(year, n, type = "b",  las=1, ylim=c(0, max(n+5)),
                  pch = 19, xlab = "Ano", ylab = "Estações implementadas"))


# how many kilometres per year
esforco <- monitora %>%
  group_by(year) %>%
  summarize(total = sum(effort, na.rm = TRUE))
esforco
with(esforco, plot(year, total, type = "b",  las=1, ylim=c(0, max(total+500)),
                    pch = 19, xlab = "Ano", ylab = "km percorridos"))


# cumulative effort
#create a new vector to hold the cumulative sum
esforco_acumulado <- monitora %>%
  filter(!is.na(effort)) %>%
  mutate(cumm_effort = cumsum(effort)) %>%
  group_by(year) %>%
  summarize(total = max(cumm_effort, na.rm = TRUE)) %>%
  mutate(acumulado_geral = cumsum(total))
esforco_acumulado
with(esforco_acumulado, plot(year, acumulado_geral, type = "b", las=1,
                   pch = 19, xlab = "Ano", ylab = "km percorridos (acumulado)"))


# how many records per year
registros <- monitora %>%
  group_by(year) %>%
  count()
registros
with(registros, plot(year, n, type = "b",  las=1, ylim=c(0, max(n+500)),
                   pch = 19, xlab = "Ano", ylab = "Numero de registros"))


# cumulative records
#create a new vector to hold the cumulative sum
acumulado <- cumsum(registros$n)
  registros$acumulado <- acumulado
with(registros, plot(year, acumulado, type = "b", las=1, ylim=c(0, max(acumulado+500)),
                             pch = 19, xlab = "Ano", ylab = "km percorridos (acumulado)"))



# how many families, genus, species
monitora %>%
  group_by(class) %>%
  count() %>%
  arrange(desc(n)) %>%
  print()

monitora %>%
  group_by(family) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=20)

monitora %>%
  group_by(genus) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=20)

monitora %>%
  group_by(species) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=20)

