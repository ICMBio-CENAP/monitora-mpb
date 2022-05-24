
# Construct and fit HMSC models
# based on examples from book:
# Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press
# note: requires the 01_read_data script to be run first

# load libraries
library(here)
library(tidyverse)
library(Hmsc)

set.seed(1)

# set the working directory to the directory that has all the files for this study
# we recommend that you create always the subdirectories "data" and "models" to the main directory
#localDir = here("manejo-florestal-jamari")
#data.directory = file.path(localDir, "data")
#model.directory = file.path(localDir, "models")

# data was read in part 2, but let us check and manipulate some of it
head(S)
head(X)
head(Y)
head(Tr)


# study design and random levels
# note from the book (pg. 64):
# HMSC implements two kinds of random effects: those needed for hierarchical study
# designs, and those needed for spatial or temporal study designs. It
# further implements any combination of these, so that it is possible to
# include in a single HMSC model both a temporal and a spatial random
# effect; the latter can be further deﬁned either at the level of plots or
# sampling units, or even at both of these levels

xyz <- X %>%
  select(placename) %>%
  rownames_to_column() %>%
  left_join(S %>% rownames_to_column(), by="rowname") %>%
  #dplyr::select(rowname, longitude, latitude, sampling_event) %>%
  select(rowname, placename, longitude, latitude, sampling_event) %>%
  rename(sample_id = rowname,
         site_id = placename,
         year = sampling_event,
         lon = longitude,
         lat = latitude) %>%
  mutate(site_id = as.factor(site_id),
         sample_id = as.factor(sample_id),
         year = as.factor(year))
head(xyz)


studyDesign <- data.frame(sample = xyz$sample_id,
                          site = xyz$site_id,
                          year = xyz$year)
head(studyDesign)

# put coordinates into matrix form so it can be used as random levels
coords <- xyz %>%
  distinct(site_id, .keep_all = TRUE) %>%
  select(lon, lat)
xycoords <- as.matrix(coords)
dimnames(xycoords) <- list(levels(studyDesign$site), c("lon", "lat"))
head(xycoords)


# set random levels
#rL <- HmscRandomLevel(units = levels(studyDesign$site))
rL1 <- HmscRandomLevel(units = levels(studyDesign$year))
rL2 <- HmscRandomLevel(sData = xycoords, longlat = TRUE)
#rL2 <- HmscRandomLevel(sData = xycoords)# sMethod = "GPP") # spatial random level

# check
rL1
rL2


# XFormula and TrFormula
XFormula <- ~ intensity_500 + dist_water + effort
TrFormula <- ~ body_mass + herbivore + faunivore + omnivore


# rename X, Y and Tr for model building
XData <- X
TrData <- Tr
rownames(TrData) <- colnames(Y)


# define presence-absence model
model_pa <- Hmsc(Y = 1*(Y > 0), XData = XData, XFormula = XFormula,
                 TrData = TrData,
                 TrFormula = TrFormula, distr="probit",
                 studyDesign = studyDesign,
                 ranLevels = list("site" = rL2, "year" = rL1))

# define abundance model
model_abu <- Hmsc(Y = Y, YScale = TRUE,
                  XData = XData, XFormula = XFormula,
                  TrData = TrData,
                  TrFormula = TrFormula, distr="lognormal poisson",
                  studyDesign = studyDesign,
                  ranLevels = list("site" = rL2, "year" = rL1))


# save unfitted models
models = list("presence_absence" = model_pa, "abundance" = model_abu)
modelnames = c("presence_absence","abundance")
save(models, modelnames, file = file.path(here("manejo-florestal-jamari", "models"), "unfitted_models") )


# It is always a good idea to look at the model object
model_pa
model_abu

getCall(model_pa)
getCall(model_abu)

head(model_pa$X)
head(model_abu$X)


# run short MCMC to check
samples = 100
thin = 10
transient = samples/2
nChains = 2
nParallel = 2
model_pa <- sampleMcmc(model_pa, thin = thin, samples = samples, 
                       transient = transient, nChains = nChains, nParallel = nParallel)
model_pa
saveRDS(model_pa, file=file.path(model.directory, "model_pa_teste"))

# now run it for real:

model.directory = here("manejo-florestal-jamari", "models")
for (thin in c(1,10,100,1000)) {
  samples = 50*thin
  transient = samples/2
  model_pa = sampleMcmc(model_pa, thin = thin, samples = samples, 
                        transient = transient, nChains = nChains, nParallel = nParallel)
  filename = file.path(model.directory, paste0("model_pa_chains_",
                                               as.character(nChains),
                                               "_samples_",
                                               as.character(samples),
                                               "_thin_",
                                               as.character(thin),
                                               ".rds"))
  saveRDS(model_pa, file=filename)
  
  model_abu = sampleMcmc(model_abu, thin = thin, samples = samples, 
                         transient = transient, nChains = nChains, nParallel = nParallel)
  filename=file.path(model.directory, paste0("model_abu_chains_",
                                             as.character(nChains),
                                             "_samples_",
                                             as.character(samples),
                                             "_thin_",
                                             as.character(thin),
                                             ".rds"))
  saveRDS(model_abu, file=filename)
}



