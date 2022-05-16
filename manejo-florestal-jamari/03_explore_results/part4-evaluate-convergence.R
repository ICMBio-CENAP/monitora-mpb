# Evaluate MCMC convergence of HMSC model

# The preliminaries are as in script S1

# Change the working directory if necessary
# wd = "C:/HMSC_book/R-scripts/Section_6_7_plants"
# setwd(wd)

library(here)

localDir = "."
data.directory = file.path(localDir, "data")
model.directory = file.path(localDir, "models")


#localDir = "."
#data.directory = file.path(localDir, "data")
#model.directory = file.path(localDir, "models")
library(Hmsc)
set.seed(1)

# The previous script saved the finished models saved the results in
# 'model.directory', and you can look at its content with
list.files(model.directory) #all models
list.files(model.directory, patt="model_pa_") #presence-absence models

# Here we explore the MCMC convergence of the presence-absence model only
# That of the abundance model could and should be explored similarly
# The main interest is in the "final" model that was run with the highest value of thin.
# However, we recommend running the script first with setting thin = 1
# Then rerun it with thin = 10, thin = 100, ... to examine how the
# convergence statistics improve

nChains = 2
samples = 250
thin = 1 # try with thin = 1, thin = 10, thin = 100, etc.
#filename=file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
#load(filename)
models <- load("/home/elildojr/Documents/r/monitora-mpb/models/model_abu_chains_2_samples_250_thin_1")
models

m <- model.abu

# extract posterior distribution and convert into a coda object
# Hmsc uses coda to examine convergence, other operations are conducted straight from the Hmsc object
mpost <- convertToCodaObject(m)

# explore MCMC convergence for beta-parameters (species niches),
# V-parameters (variation in species niches),
# and the rho-parameter (phylogenetic signal)
# These are just examples; you may use names(mpost) see what other parameters were estimated

# first examine the effective size of the posterior sample.
# As there are two chains with a sample of 100 from each, the actual sample size is 200.
# Thus, ideally the effective sample size would be 200 as well.
# But in presence of autocorrelation, the effective sample size will be lower than the actual sample size

ess.beta <- effectiveSize(mpost$Beta)

# Our model contains x*y beta-parameters (x for each of the y species)
# Thus we have x*y effective sample sizes to look at. This is best done with a histogram
hist(ess.beta, xlab = expression("Effective sample size" ~ beta ~ ""))

ess.V <- effectiveSize(mpost$V)
# As ess.V has only four entries, we just look at it directly instead of drawing a histogram
ess.V

# we did not used Phylo so there is no Rho
#ess.rho = effectiveSize(mpost$Rho)
#ess.rho

# We then examine the Gelman diagnostics, i.e. the Potential scale reduction factors
# This diagnostic compares if different chains (here we have 2 chains) give consistent results
# Ideally the value of this diagnostic would be close to one.
# As you increase thinning, you should observe the values getting closer to one.
# The Gelman diagnostic is often more informative diagnostic than the effective sample size, so if you will use only one of these, we recommend the Gelman diagnostic

psrf.beta <- gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
hist(psrf.beta, xlab = expression("Potential scale reduction factor" ~ beta ~ ""))
psrf.V <- gelman.diag(mpost$V,multivariate=FALSE)$psrf
psrf.V
#psrf.rho = gelman.diag(mpost$Rho,multivariate=FALSE)$psrf
#psrf.rho


partition <- createPartition(model.pa, nfolds = 2,
                            column = "sample")
partition


MF = list()
MFCV = list()
for (i in 1:3){
  preds = computePredictedValues(models[[i]])
  MF[[i]] = evaluateModelFit(hM = models[[i]], predY = preds)
  preds = computePredictedValues(models[[i]],
                                 partition = partition)
  MFCV[[i]] = evaluateModelFit(hM = models[[i]], predY = preds)
}
