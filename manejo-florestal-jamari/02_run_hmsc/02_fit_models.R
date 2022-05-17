
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
  dplyr::select(placename) %>%
  rownames_to_column() %>%
  left_join(S %>% rownames_to_column(), by="rowname") %>%
  dplyr::select(rowname, longitude, latitude, sampling_event) %>%
  #dplyr::select(placename, longitude, latitude, sampling_event) %>%
  rename(sample = rowname,
         #sample = placename,
         z = sampling_event,
         x = longitude,
         y = latitude) %>%
  mutate(sample = as.factor(sample),
         z = as.numeric(z))
head(xyz)

studyDesign <- data.frame(sample = as.factor(xyz$sample))
head(studyDesign)
str(studyDesign)

rL <- HmscRandomLevel(sData = xyz %>% dplyr::select(x, y, z))
rL

# template from Mirkka Jones:
#sample = studyDesign$sample
#rL.sample = HmscRandomLevel(units = levels(sample))

#lon = studyDesign$x
#rL.lon = HmscRandomLevel(units = levels(lon))

#lat = studyDesign$y
#rL.lat = HmscRandomLevel(units = levels(lat))

#year = studyDesign$z
#rL.year = HmscRandomLevel(units = levels(year))


Ypa <- 1*(Y>0)
Yabu <- Y
Yabu[Y == 0] <- NA
Yabu <- log(Yabu)

# XFormula and TrFormula
XFormula <- ~ dist_water + intensity_500 + effort
TrFormula <- ~ body_mass + herbivore + faunivore + omnivore

# rename X, Y and Tr for model building
XData <- X
TrData <- Tr
rownames(TrData) <- colnames(Ypa)


# define presence-absence model
model.pa <- Hmsc(Y = (Y > 0), XData = XData, XFormula = XFormula,
                 TrData = TrData,
                 TrFormula = TrFormula, distr="probit",
                 studyDesign = studyDesign,
                 ranLevels = list("sample" = rL))

# define abundance model
model.abu <- Hmsc(Y = Y, XData = XData, XFormula = XFormula,
              TrData = TrData,
              TrFormula = TrFormula, distr="lognormal poisson",
              studyDesign = studyDesign,
              ranLevels = list("sample" = rL))



# It is always a good idea to look at the model object.
model.pa
model.abu

getCall(model.pa)
getCall(model.abu)

head(model.pa$X)
head(model.abu$X)

thin = 1
samples = 250
nChains = 2
nParallel = 2

# run presence-absence model
model.pa <- sampleMcmc(model.pa, thin = thin, samples = samples, nChains = nChains, nParallel = nParallel)
save(model.pa, file=here("manejo-florestal-jamari", "models", "model_pa"))

# run abundance model
model.abu <- sampleMcmc(model.abu, thin = thin, samples = samples, nChains = nChains, nParallel = nParallel)
save(model.abu, file=here("manejo-florestal-jamari", "models", "model_abu"))

