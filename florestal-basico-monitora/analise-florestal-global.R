
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
  rename(cduc = "CDUC", uc_name = "Local - Nome da Unidade de Conservação",
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
  select(cduc, uc_name, station, effort, date, year, class, order, family, genus,
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
                  pch = 19, xlab = "Ano", ylab = "UCs no programa"))

# how many stations per year
estacoes <- monitora %>%
  distinct(uc_name, station, year) %>%
  count(year) %>%
  arrange(year)
estacoes
with(estacoes, plot(year, n, type = "b",  las=1, ylim=c(0, max(n+5)),
                  pch = 19, xlab = "Ano", ylab = "Estações amostrais"))


# how many kilometres per year
esforco <- monitora %>%
  group_by(year) %>%
  summarize(total = sum(effort, na.rm = TRUE))
esforco
with(esforco, plot(year, total, type = "b",  las=1, ylim=c(0, max(total+500)),
                    pch = 19, xlab = "Ano", ylab = "km percorridos no ano"))


# cumulative effort
#create a new vector to hold the cumulative sum
esforco_acumulado <- esforco %>%
  mutate(cumm_effort = cumsum(total))
esforco_acumulado
with(esforco_acumulado, plot(year, cumm_effort, type = "b", las=1, ylim=c(0, max(cumm_effort+1000)),
                   pch = 19, xlab = "Ano", ylab = "km percorridos"))
par(new = TRUE)
with(esforco_acumulado, plot(year, total, type = "b",  las=1, ylim=c(0, max(cumm_effort+1000)),
                   pch = 19, col="red", xlab = "Ano", ylab = ""), add=TRUE)
# Add a legend
legend("topleft", 
       legend = c("eforço anual", "esforço acumulado"), 
       col = c("red", "black"), 
       pch = c(19,19), 
       bty = "n", 
       pt.cex = 1, 
       cex = 1, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.1, 0.1))
dev.off()

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


# ----- select the area with best set of data for detailed analysis
cazumba <- monitora %>%
  filter(uc_name == "RESEX Cazumbá-Iracema" | uc_name == "Resex Cazumbá-Iracema" )
cazumba

# load library
library(R2jags)

# load fuctions
source(here("bin", "population-functions.R"))

#tabela 1 com esforco por ano
tabela1 <- tibble(
  'Ano' = sort(unique(cazumba$year)),
  'Transectos' = 0,
  'Esforço (km)' = 0,
  'Registros' = 0
)
for(i in 1:nrow(tabela1)) {
  df1 <- subset(cazumba, year == unique(cazumba$year)[i])
  tabela1[i, "Transectos"] <- length(unique(df1$station))
  tabela1[i, "Esforço (km)"] <- sum(df1$effort, na.rm=T)
  tabela1[i, "Registros"] <- nrow(df1)
}
tabela1


# tabela com especies registradas
tabela2 <- cazumba %>% group_by(genus) %>% summarize(n = n(), ) %>% arrange(genus)
tabela2$'grupos/10km' <- round((tabela2$n/sum(cazumba$effort, na.rm=T))*10, 2)
print(tabela2, n=20)

# taxas de encontro anuais
encounter.rate(cazumba, "genus")
encounter_rate
# se taxa avistamento for = 0, alterar para 0.001
encounter_rate[encounter_rate == 0] <- 0.001
encounter_rate

# selecionar somente espécies que atendem a criterios minimos
# (taxa de avistamento >= 0.5) e pelo menos 3 anos de dados
encounter_rate$mean <- rowMeans(encounter_rate[,c(3:ncol(encounter_rate))], na.rm = TRUE)
use.this <- subset(encounter_rate, mean >= 0.1) 
encounter_rate <- use.this
encounter_rate$taxon <- factor(encounter_rate$taxon)
encounter_rate$mean <- NULL
encounter_rate
dim(encounter_rate)

encounter_rate$taxon

# modelo populacional bayesiano

# check species names for analysis
species <- encounter_rate$taxon

# criar tabela para receber resultados do loop
tabela3 <- tibble(
  Genero = character(),
  'r' = numeric(),
  IC = character(),
  'Prob. declinio' = character(),
  'Prob. aumento' = character(),
)

# criar lista para receber N.est
N.est_all_species <- list()

#loop para rodar modelo para todas as spp
for(i in 1:length(species)) {
  y  <- as.numeric(encounter_rate[species[i], 3:ncol(encounter_rate)])
  state.space.model(y, n.years)
  # plot trends
  jpeg(here("florestal-basico-monitora", paste(gsub(" ", "_", species)[i], "cazumba.jpg", sep="_")), width=1000, height=600, res=120) # Open jpeg file
  pop.trends()
  dev.off()
  # save ssm results to be used for making figures later independent of loop and add them to list
  N.est_all_species[[paste(gsub(" ", "_", species)[i], "ssm", sep="_")]] <- ssm$BUGSoutput$sims.list$N.est
  # build table 1
  tabela3[i,] <- list(species[i],
                      round(mean(meanR), 2),
                      paste(round(quantile(meanR, probs = c(0.025)), 2), " a ", round(quantile(meanR, probs = c(0.975)), 2), sep=""),
                      paste((round(length(which(meanR < 0))/length(meanR), 2)*100), "%", sep= ""),
                      paste((round(length(which(meanR > 0))/length(meanR), 2)*100), "%", sep= ""))
}
tabela3
#write.csv(tabela3, here("experimental", "tabela3_cazumba.csv"))
class(tabela3)

