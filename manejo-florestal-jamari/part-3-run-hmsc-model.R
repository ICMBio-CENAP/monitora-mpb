# THIS SCRIPT CONSTRUCTS AND FITS HMSC MODELS FOR THE PLANT EXAMPLE (SECTION 6.7) OF THE BOOK
# Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press.

# We first set the working directory to the directory that has all the
# files for this case study. We recommend that you create always the
# subdirectories "data" and "models" to the main directory. If running
# the script in another computer, all that needs to be changed in the
# script below is the main directory. Working directory below works
# only in our local computer system, and you need to modify this in
# your own system. For instance, in a MacOS system this could be
# "~/Data/HMSC_book/Section_6_7_plants".

## see your current working directory first
getwd()

# replace the lines below so that they correspond your working directory
wd = here() # using package here does this automatically
#setwd(wd)

localDir = "."
data.directory = file.path(localDir, "data")
model.directory = file.path(localDir, "models")

# We load the Hmsc package, and set the random seed, so that all
# results from this script are reproducible.

library(tidyverse)
library(Hmsc)
set.seed(1)

# data was read in part 2, but let us check and manipulate some of it
head(S)
head(X)
head(Y)
head(Tr)

# XFormula and TrFormula
XFormula <- ~ elevation + slope + dist_water + dummy_variable + effort
TrFormula <- ~ body_mass + herbivore + carnivore + insectivore + omnivore

# Study design and random levels
xyz <- X %>%
  select(placename) %>%
  rownames_to_column() %>%
  left_join(S %>% rownames_to_column(), by="rowname") %>%
  select(rowname, longitude, latitude, sampling_event) %>%
  rename(sample = rowname,
         z = sampling_event,
         x = longitude,
         y = latitude) %>%
  mutate(sample = as.factor(sample),
         z = as.numeric(z))
head(xyz)
#studyDesign <- data.frame(xyz)
studyDesign <- data.frame(sample = xyz$sample)
head(studyDesign)
str(studyDesign)
rL <- HmscRandomLevel(sData = xyz %>% select(x, y, z))
rL


# rename X, Y and Tr for model building
XData <- X
TrData <- Tr
rownames(TrData) <- colnames(Y)


# define presence-absence model
model.pa <- Hmsc(Y = (Y>0), XData = XData, XFormula = XFormula,
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

model.pa <- sampleMcmc(model.pa, thin = thin, samples = samples, nChains = nChains, nParallel = nParallel)

# run model pa
#model.pa <- sampleMcmc(model.pa, thin = thin, samples = samples, nChains = nChains, nParallel = nParallel)
#save(model.pa, here("models", "model.pa.rds"))
#filename = file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
#save(model.pa, file=filename)

# run model abu
#model.abu <- sampleMcmc(model.abu, thin = thin, samples = samples, nChains = nChains, nParallel = nParallel)
#save(model.abu, here("models", "model.abu.rds"))
#filename = file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
#save(model.pa, file=filename)


#for (thin in c(1,10,100,1000)){
for (thin in c(1,10)) {
  transient = 10*thin
  model.pa <- sampleMcmc(model.pa, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nParallel)
  filename = file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(model.pa, file=filename)
}

  model.abu <- sampleMcmc(model.abu, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nParallel)
  filename=file.path(model.directory, paste0("model_abu_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(model.abu, file=filename)
}

