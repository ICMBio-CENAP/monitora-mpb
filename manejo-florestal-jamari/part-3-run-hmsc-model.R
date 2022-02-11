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

library(Hmsc)
set.seed(1)

# xy
xy <- S %>%
  select(longitude, latitude) %>%
  distinct(longitude, latitude)
head(xy)

head(Y)

XData <- X
head(XData)

TrData <- Tr
rownames(TrData) <- colnames(Y)
head(TrData)


XFormula <- ~ elevation + slope + dist_water + dummy_variable + effort

# We will use body mass as the only trait covariate, and assume a linear
# effect

TrFormula <- ~ body_mass + herbivore + carnivore + insectivore + omnivore

# We next define the presence-absence model.

# We convert the count data to presence-absences by Y=(Y>0)

# We include environmental covariates through XData and XFormula

# We include trait covariates by TrData and TrFormula

# We include the phylogenetic relationships by phyloTree

# We assume the probit distribution as appropriate for
# presence-absence data

# The data have a specific structure and includes random
# effects, thus we have to define studyDesign and ranLevels
studyDesign = data.frame(placename = X$placename)
rL = HmscRandomLevel(sData = xy)

model.abu <- Hmsc(Y = Y, XData = XData, XFormula = XFormula,
              TrData = TrData,
              TrFormula = TrFormula, distr="lognormal poisson",
              studyDesign = studyDesign,
              ranLevels = list(route=rL))

# it is not working with random levels, lets try without it
model.pa <- Hmsc(Y = (Y>0), XData = XData, XFormula = XFormula,
                 TrData = TrData,
                 TrFormula = TrFormula, distr="probit",
                 studyDesign = studyDesign)


model.abu <- Hmsc(Y = Y, XData = XData, XFormula = XFormula,
                 TrData = TrData,
                 TrFormula = TrFormula, distr="lognormal poisson",
                 studyDesign = studyDesign)

# It is always a good idea to look at the model object.
model.pa
model.abu

getCall(model.pa)
getCall(model.abu)

head(model.pa$X)
head(model.abu$X)

thin = 100
samples = 1000
nChains = 2
nParallel = 2

# for (thin in c(1,10,100,1000)){
for (thin in c(1,10)) {
  transient = 50*thin
  model.pa <- sampleMcmc(model.pa, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nParallel)
  filename = file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(model.pa, file=filename)
  model.abu <- sampleMcmc(model.abu, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nParallel)
  filename=file.path(model.directory, paste0("model_abu_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(model.abu, file=filename)
}

# PAREI AQUI
# NB! problemas a resolver acima, por exemplo quantas unidades amostrais afinal?
# sítios repetidos em diferentes anos estão sendo considerados amostras independentes?
#-------------------------------------------------

model.pa = Hmsc(Y=(Y>0),
                XData = X,  XFormula=XFormula,
                TrData = TP, TrFormula = TrFormula,
                distr="probit")

# It is always a good idea to look at the model object.

model.pa

# Evaluating the above line should give "Hmsc object with 52 sampling
# units, 75 species, 2 covariates, 2 traits and 0 random levels"

getCall(model.pa)

# Evaluating the above line shows the call by which the model object was constructed 

# It is alway a good idea to look at also the X-matrix that Hmsc has
# generated from XData and XFormula

# This will show e.g. if continuous variables are accidentally treated
# as factors or vice versa

head(model.pa$X)

# Evaluating the above line shows that there are two columns in the X
# matrix: the intercept and the linear effect of TMG.

# Note that the Hmsc object says that there are two covariates, thus
# here the intercept is counted as well.

# It is alway a good idea to look at also the Tr-matrix that Hmsc has
# generated from TrData and TrFormula

# This will show e.g. if continuous variables are accidentally treated
# as factors or vice versa

head(model.pa$Tr)

# Evaluating the above line shows that there are two columns in the Tr
# matrix: the intercept and the linear effect of CN.

# Note that the Hmsc object says that there are two traits, thus here
# the intercept is counted as well.


# The abundance model is defined exactly in the same way,

# except that now the data are not trunccated to presence-absence,

# and now we assume a lognormal Poisson model rather than the probit model.

model.abu = Hmsc(Y=Y,
                 XData = X,  XFormula=XFormula,
                 TrData = TP, TrFormula = TrFormula,
                 distr="lognormal poisson")

# Alternatively, we may define model.abu as an updated version of model.pa
model.abu = update(model.pa, Y = Y, distr="lognormal poisson")

# both will give the same model version
getCall(model.abu)

# We will fit both models so that we store 100 posterior samples for
# each of two chains
# We will set these below as nChains = 2, samples = 100

# We note that for more "final" results, one might wish to have
# e.g. 1000 samples for each of four chains

# In the script below, we fit both models (presence-absence and
# abundance) one after each other

# After fitting the model, we save the ftted model object to a file

# Loading the fitted model object then serves as the starting point
# for exploring the results

# The script runs over a loop where thin is first 1, then 10, then
# 100, and so on

# Thin is the thinning parameter of the MCMC chain.

# The transient (also called burn-in) is set to 50*thin

# When thin = 1, there will be 50 burn-in and 100 actual
# iterations. All actual iterations are stored.

# When thin = 10, there will be 500 burn-in and 1000 actual
# iterations. The actual iterations are thinned by 10, so 100 are
# stored.

# When thin = 100, there will be 5000 burn-in and 10000 actual
# iterations. The actual iterations are thinned by 100, so 100 are
# stored.

# A long MCMC chain is needed to achieve convergence

# Thinning is applied to avoid storing model objects of very large
# size

# Even if in the end thin = 1000 is required to achieve converge, We
# recommend to run the loop thin = 1, 10, 100, 1000

# This is for several reasons.

# First of all, it will not be known beforehand how much thinning is
# needed to achieve satisfactory convergence

# Second, thin = 1 will run very fast, whereas thin = 1000 will take
# very long (1000 times longer)

# After thin = 1 is completed, it is already possible to develop all
# the remaining scripts that explore the fitted model

# When exploring the fitted model, often one realizes changes that
# need to be made, even if the fitting has not converged

# Third, running the model fitting for thin = 1, 10, 100, 1000 does
# not take much longer than running it just for thin = 1000 (it takes
# ca. 12% longer)

# Thus, in summary, running the model fitting for thin = 1, 10, 100,
# 1000 typically saves a lot of time, as it allows one to proceed fast
# in writing (and revising) all the scripts that are needed from
# defining the model to producing the result tables and figures

# The idea is not to run the entire loop in one go, as that would take
# a lot of time. Just run thin = 1, and then move to develop the next
# scripts.

# Most modern computers have several CPUs and you can run chains in
# parallel to further save time. Below we set nParallel=2 so that we
# use one  CPU for each of the two chains.
# Setting nParallel > 1 has two consequences: tracing info
# vanishes, and random sequences will change from nParallel=1 and
# results are no longer reproducible compared to that choice

# You may then leave the remaining part of the loop (e.g. thin = 10,
# 100, 1000) to run e.g. overnight

## We set up basic model parameters 

thin = 10
samples = 100
nChains = 2
nParallel = 2

# for (thin in c(1,10,100,1000)){
for (thin in c(1,10)) {
  transient = 50*thin
  model.pa = sampleMcmc(model.pa, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nParallel)
  filename = file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(model.pa, file=filename)
  model.abu = sampleMcmc(model.abu, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nParallel)
  filename=file.path(model.directory, paste0("model_abu_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
  save(model.abu, file=filename)
}

# The script saves the finalized model after each thin. This means
# that you will not lose finished models if you interrupt the script,
# but they are in directory models.
