
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
XFormula <- ~ intensity_500 + recovery_time + dist_water + effort
TrFormula <- ~ body_mass + herbivore + faunivore + omnivore

# rename X, Y and Tr for model building
XData <- X
TrData <- Tr
rownames(TrData) <- colnames(Ypa)


# define presence-absence model
model_pa <- Hmsc(Y = (Y > 0), XData = XData, XFormula = XFormula,
                 TrData = TrData,
                 TrFormula = TrFormula, distr="probit",
                 studyDesign = studyDesign,
                 ranLevels = list("sample" = rL))

# define abundance model
model_abu <- Hmsc(Y = Y, XData = XData, XFormula = XFormula,
              TrData = TrData,
              TrFormula = TrFormula, distr="lognormal poisson",
              studyDesign = studyDesign,
              ranLevels = list("sample" = rL))


# save unfitted models
models = list("presence_absence" = model_pa, "abundance" = model_abu)
modelnames = c("presence_absence","abundance")
save(models, modelnames, file = file.path(here("manejo-florestal-jamari", "models"), "unfitted_models") )


# It is always a good idea to look at the model object.
model.pa
model.abu

getCall(model.pa)
getCall(model.abu)

head(model.pa$X)
head(model.abu$X)

thin = 2
samples = 100
transient = samples/2
nChains = 2
nParallel = 2

# run presence-absence model
model.pa <- sampleMcmc(model.pa, thin = thin, samples = samples, 
                       transient = transient, nChains = nChains, nParallel = nParallel)
save(model.pa, file=here("manejo-florestal-jamari", "models", "model_pa"))

# run abundance model

#model.abu <- sampleMcmc(model.abu, thin = thin, samples = samples, 
#                        transient = transient, nChains = nChains, nParallel = nParallel)
#save(model.abu, file=here("manejo-florestal-jamari", "models", "model_abu"))
for (thin in c(1,10,100,1000)){
  transient <- 50*thin
  models <- sampleMcmc(model_abu, thin = thin, samples = samples, transient = transient,
                       nChains = nChains, nParallel = nParallel)
  filename <- here("manejo-florestal-jamari", "models", paste0("model_abu_chains_",
                                                               as.character(nChains),
                                                               "_samples_",
                                                               as.character(samples),
                                                               "_thin_",
                                                               as.character(thin)))
  save(models, file=filename)
}


#################################

# from fungi models template:

# in the scripts below, the object models will include all six models that were discussed in the book chapter, organised as a
# list of lists, so the models[[i]][[j]] is the model type i=1,2,3 for the choice of explanatory variables j=1,2.

# The model type i=1 is a lognormal Poisson model that is fitted to the sequence count data as they are.
# So this model will simultaneously account both for whether the species is present or not as well as how abundant
# it is when it is present. The model types i=2 and i=3 form together a hurdle model that separates presence-absence
# variation from abundance variation. Thus, model i=2 is a probit model that is fitted to sequences counts
# truncated to presence-absence, whereas model i=3 is a normal model that is fitted to log-transformed sequence
# counts conditional on presence. This means that for i=3, sampling units where the species is not present are shown in the
# Y matrix as missing data (NA) rather than as zeros.

# Concerning the choices on the explanatory variables, we always include log-transformed sequencing depth,
# as this variable measures the total observation effort. Sequencing depth is the only variable included with j=1,
# and thus in these models we will estimate raw co-occurrences. With j=2, we additionally include the categorical
# variable of the decay class, and thus in these models we will estimate residual co-occurrences.
# We note that there are also many other properties of the log than the decay class that are likely to
# influence fungal occurrences, such as the diameter of the log. However, we ignore the other variables,
# as the data was specifically sampled to include much variation in decay class but less variation in other properties such as diameter.

# In all models, we also a random effect at the sampling unit level. The random effect models associations among the species,
# which is what we are primarily interested about.

############################

# We next loop over both the model types as well as the selections of explanatory variables to fit all the six models.
# After fitting all models, we save the models object (including the six fitted model objects) to a file
# Loading the fitted models then serves as the starting point for exploring the results
# The script runs over a loop where thin is first 1, then 10, then 100, and so on
# Thin is the thinning parameter of the MCMC chain.
# The transient (also called burn-in) is set to 50*thin
# When thin = 1, there will be 50 burn-in and 100 actual iterations. All actual iterations are stored.
# When thin = 10, there will be 500 burn-in and 1000 actual iterations. The actual iterations are thinned by 10, so 100 are stored.
# When thin = 100, there will be 5000 burn-in and 10000 actual iterations. The actual iterations are thinned by 100, so 100 are stored.
# A long MCMC chain is needed to achieve convergence
# Thinning is applied to avoid storing model objects of very large size
# Even if in the end thin = 1000 is required to achieve converge, We recommend to run the loop thin = 1, 10, 100, 1000
# This is for several reasons.
# First of all, it will not be known beforehand how much thinning is needed to achieve satisfactory convergence
# Second, thin = 1 will run very fast, whereas thin = 1000 will take very long (1000 times longer)
# After thin = 1 is completed, it is already possible to develop all the remaining scripts that explore the fitted model
# When exploring the fitted model, often one realizes changes that need to be made, even if the fitting has not converged
# Third, running the model fitting for thin = 1, 10, 100, 1000 does not take much longer than running it just for thin = 1000 (it takes ca. 12% longer)
# Thus, in summary, running the model fitting for thin = 1, 10, 100, 1000 typically saves a lot of time,
# as it allows one to proceed fast in writing (and revising) all the scripts that are needed from defining the model to producing the result tables and figures
# The idea is not to run the entire loop in one go, as that would take a lot of time. Just run thin = 1, and then move to develop the next scripts. 
# You may then leave the remaining part of the loop (e.g. thin = 10, 100, 1000) to run e.g. overnight

for (thin in c(1,10,100,1000)){
  transient = 50*thin
  for (i in 1:3){
    for (j in 1:2){
      cat("model = ",i, ", modeltype = ",j,"\n",sep="")
      models[[i]][[j]] = sampleMcmc(models[[i]][[j]], thin = thin, samples = samples, transient = transient,
                                    nChains = nChains, nParallel = nChains, initPar = if(i==3) {NULL} else {"fixed effects"})
    }
  }
  filename=file.path(model.directory, paste0("models_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(models,file=filename)
}

# MCMC convergence can be difficult to achieve especially in those models that are not base on normal distribution
# For this reason, in the script above we initialize the "lognormal poisson" (i=1) and "probit" (i=2) models with
# initPar="fixed effects", with which option the MCMC chains are not started from locations randomized from the prior
# but from a maximum likelihood solution to the fixed-effects part of the model
