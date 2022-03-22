
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
                  pch = 19, xlab = "", ylab = "UCs no programa"))

# how many stations per year
estacoes <- monitora %>%
  distinct(uc_name, station, year) %>%
  count(year) %>%
  arrange(year)
estacoes
with(estacoes, plot(year, n, type = "b",  las=1, ylim=c(0, max(n+5)),
                  pch = 19, xlab = "", ylab = "Estações amostrais"))


# how many kilometres per year
esforco <- monitora %>%
  group_by(year) %>%
  summarize(total = sum(effort, na.rm = TRUE))
esforco
with(esforco, plot(year, total, type = "b",  las=1, ylim=c(0, max(total+500)),
                    pch = 19, xlab = "", ylab = "km percorridos no ano"))


# cumulative effort
#create a new vector to hold the cumulative sum
esforco_acumulado <- esforco %>%
  mutate(cumm_effort = cumsum(total))
esforco_acumulado
with(esforco_acumulado, plot(year, cumm_effort, type = "b", las=1, ylim=c(0, max(cumm_effort+1000)),
                   pch = 19, xlab = "", ylab = "km percorridos"))
with(esforco_acumulado, lines(year, total, type = "b", las=1, pch = 19, col="red"))

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
                   pch = 19, xlab = "", ylab = "Numero de registros"))


# cumulative records
#create a new vector to hold the cumulative sum
acumulado <- cumsum(registros$n)
  registros$acumulado <- acumulado
with(registros, plot(year, acumulado, type = "b", las=1, ylim=c(0, max(acumulado+500)),
                             pch = 19, xlab = "", ylab = "km percorridos (acumulado)"))



# how many families, genus, species
monitora %>%
  group_by(class) %>%
  filter(!is.na(class)) %>%
  count() %>%
  arrange(desc(n)) %>%
  print()

monitora %>%
  group_by(family) %>%
  filter(!is.na(family)) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=20)

monitora %>%
  group_by(genus) %>%
  filter(!is.na(genus)) %>%
  count() %>%
  arrange(desc(n)) %>%
  print(n=20)

monitora %>%
  group_by(species) %>%
  filter(!is.na(species)) %>%
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

#---------------------------------------
#---------------------------------------
#---------------------------------------
# TESTE!
# Nova versao de analise para taxa de encontro
# modelo bayesiano atualizado incorpora incerteza ENTRE trilhas
# ainda precisa ajustar alguns probleminhas...

# funcao para gerar y por trilha para sp selecionada
calcular.registros.por.trilha <- function(nome_especie, nome_uc) {
  
  # esforco por ano por trilha
  esforco_anual_trilha <- nome_uc %>%
    group_by(year, station) %>%
    summarize(esforco = sum(esforco, na.rm = TRUE)) %>%
    pivot_wider(names_from = year, values_from = esforco) %>%
    select(station, `2014`, `2015`, `2016`, `2017`,  `2018`, `2019`)
  esforco_anual_trilha
  
  # numero de registros por ano por trilha
  n_registros_trilha <- nome_uc %>%
    filter(species == nome_especie) %>%
    group_by(year, station) %>%
    count() %>%
    mutate(n = as.numeric(n)) %>%
    pivot_wider(names_from = year, values_from = n) %>%
    select(station, `2014`, `2015`, `2016`, `2017`,  `2018`, `2019`) %>%
    replace(is.na(.), 0.001)
  n_registros_trilha
  
  # combinar registros e esforco para obter taxa de encontro
  esforco_anual_trilha <- esforco_anual_trilha %>%
    select(-station)
  n_registros_trilha <- n_registros_trilha %>%
    ungroup() %>%
    select(-station)
  
  y <- n_registros_trilha/(esforco_anual_trilha/1000)
  
  return(y)
  
}


# funcao para rodar modelo populacional bayesiano
state.space.model <- function(y) {
  # especificar modelo na linguagem BUGS
  sink(here("analise-florestal-global", "ssm.jags"))
  cat("
  model {
  # Priors and constraints
  for(ea in 1:3) {
  logN.est[ea, 1] ~ dnorm(0, 0.01)       # Prior for initial population size
  mean.r[ea] ~ dnorm(1, 0.001)             # Prior for mean growth rate
  }
  sigma.proc ~ dunif(0, 1)             # Prior for sd of state process
  sigma2.proc <- pow(sigma.proc, 2)
  tau.proc <- pow(sigma.proc, -2)
  sigma.obs ~ dunif(0, 1)              # Prior for sd of observation process
  sigma2.obs <- pow(sigma.obs, 2)
  tau.obs <- pow(sigma.obs, -2)
  
  # Likelihood
  # State process
  for (year in 1:(n_years-1)) {
    for(ea in 1:3) {
      r[ea, year] ~ dnorm(mean.r[ea], tau.proc)
      logN.est[ea, year+1] <- logN.est[ea, year] + r[ea, year]
    }
   }
  # Observation process
    for (year in 1:n_years) {
      for(ea in 1:3) {
        y[ea, year] ~ dnorm(logN.est[ea, year], tau.obs)
      }
   }
   
  # Population sizes on real scale
  for (year in 1:n_years) {
    for(ea in 1:3) {
     N.est[ea, year] <- exp(logN.est[ea, year])/100
    }
  }
  
  # Derived parameter: mean encounter rate
  for(year in 1:n_years) {
      mean_N[year] <- mean(N.est[, year])
  }
    
  }
  ",fill = TRUE)
  sink()
  
  #  definir y
  y <-  y
  
  # definir numero de anos
  n_years <- ncol(y)
  
  # juntar os dados
  jags.data <- list(y = log(y*100), n_years = n_years)
  
  # Initial values
  #inits <- function(){
  #  list(sigma.proc = runif(1, 0, 1), mean.r = rnorm(1),
  #       sigma.obs = runif(1, 0, 1),
  #       LogN.est = rbind(c(rnorm(1, -0.5, 0.1), rep(NA, (n.years-1))),
  #                        c(rnorm(1, -0.5, 0.1), rep(NA, (n.years-1))),
  #                        c(rnorm(1, -0.5, 0.1), rep(NA, (n.years-1))) )
  #} 
  #inits()
  
  # Parametros monitorados
  parameters <- c("mean_N", "r", "mean.r", "sigma2.obs", "sigma2.proc", "N.est")
  
  # definicoes MCMC
  ni <- 25000
  nt <- 3
  nb <- 10000
  nc <- 3
  
  # chamar o JAGS a partir do R
  ssm <- jags(jags.data, inits=NULL, parameters, here("analise-florestal-global", "ssm.jags"), n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb)
  
  # checar resultados
  print(ssm, digits = 2)
  
  # Probabiliade de N(2021) < N(2014)
  #mean(ssm$BUGSoutput$sims.list$N.est[,8] < ssm$BUGSoutput$mean$N.est[1])
  
  assign("N.est", ssm$BUGSoutput$sims.list$N.est, .GlobalEnv)
  assign("mean_N", ssm$BUGSoutput$sims.list$mean_N, .GlobalEnv) 
  assign("meanR", ssm$BUGSoutput$sims.list$mean.r, .GlobalEnv) 
  assign("ssm", ssm, .GlobalEnv)
  
}


# funcao para rodar modelo e gerar grafico para cada especie 
computar.plotar.taxa_encontro <- function(nome_especie, nome_uc) {
  
  # preparar dados
  y <- calcular.registros.por.trilha(nome_especie, nome_uc)
  n_years <- ncol(y)
  
  # rodar modelo bayesiano
  state.space.model(y)
  
  # computar quantis
  modelo_quantis <- as_tibble(mean_N) %>%
    pivot_longer(1:6, names_to = "year") %>%
    group_by(year) %>%
    summarize(quant025 = quantile(value, probs = 0.025),
              quant25 = quantile(value, probs = 0.25), 
              quant50 = quantile(value, probs = 0.5),
              quant75 = quantile(value, probs = 0.75),
              quant975 = quantile(value, probs = 0.975)) %>%
    mutate(year = c(2014:2019))
  modelo_quantis
  
  # plotar com ribbons
  plot_tendencia <- ggplot() + 
    geom_ribbon(data=modelo_quantis, aes(x=year, ymin=quant025, ymax=quant975),fill="coral", alpha=0.2) +
    geom_ribbon(data=modelo_quantis, aes(x=year, ymin=quant25, ymax=quant75),fill="coral3", alpha=0.3) +
    geom_line(data=modelo_quantis, aes(x=year, y=quant50), size=0.5, alpha=0.5) +
    geom_point(data=modelo_quantis, aes(x=year, y=quant50), size=2, alpha=0.5) +
    #stat_smooth(data=modelo_quantis, aes(x=year, y=quant50), method = "lm", formula = y ~ poly(x, 5), se = FALSE) +
    ylim(0, max(modelo_quantis$quant975)+0.05) +
    xlab("Ano") + 
    ylab("Taxa de encontro") +
    theme_bw() +
    theme(text = element_text(size=14)) #+
  #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  plot_tendencia
  
  # salvar jpeg
  #ggsave(here("encontro_saberes", paste(nome_especie, "2014 a 2021.jpeg", sep = " ")) )
  
}

computar.plotar.taxa_encontro("Dasyprocta fuliginosa", cazumba)
computar.plotar.taxa_encontro("Penelope")
computar.plotar.taxa_encontro("Sapajus")
computar.plotar.taxa_encontro("Urosciurus")