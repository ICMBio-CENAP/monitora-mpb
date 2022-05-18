# Evaluate MCMC convergence of HMSC model

# The preliminaries are as in script S1

# load libraries
library(here)
library(Hmsc)
set.seed(1)

data.directory = here("manejo-florestal-jamari", "data")
model.directory = here("manejo-florestal-jamari", "models")

# the previous script saved the finished models saved the results in
# 'model.directory', and you can look at its content with
list.files(model.directory) #all models
list.files(model.directory, patt="model_pa") #presence-absence models

# here we explore the MCMC convergence of the presence-absence model only
# that of the abundance model could and should be explored similarly
# the main interest is in the "final" model that was run with the highest value of thin.
# however, we recommend running the script first with setting thin = 1
# then rerun it with thin = 10, thin = 100, ... to examine how the
# convergence statistics improve

nChains = 2
samples = 50000
thin = 250 # try with thin = 1, thin = 10, thin = 100, etc.
#models <- load(here("manejo-florestal-jamari", "models",
#                    "model_pa"))
models <- load(here("manejo-florestal-jamari", "models",
                    "model_pa_chains_2_samples_50000_thin_1"))
models

m <- model.pa
m

# extract posterior distribution and convert into a coda object
# Hmsc uses coda to examine convergence, other operations are conducted straight from the Hmsc object
mpost <- convertToCodaObject(m)

# see which parameters were estimated
names(mpost)

# explore MCMC convergence for beta-parameters (species niches) and
# V-parameters (variation in species niches)

# first examine the effective size of the posterior sample.
# As there are two chains with a sample of 100 from each, the actual sample size is 200
# Thus, ideally the effective sample size would be 200 as well
# But in presence of autocorrelation, the effective sample size will be lower than the actual sample size
ess.beta <- effectiveSize(mpost$Beta)
ess.beta
# our model contains x*y beta-parameters (x for each of the y species)
# Thus we have x*y effective sample sizes to look at. This is best done with a histogram
hist(ess.beta, xlab = expression("Effective sample size" ~ beta ~ ""))

# As ess.V has fewer entries, we just look at it directly instead of drawing a histogram
ess.V <- effectiveSize(mpost$V)
ess.V

# we did not used Phylo so there is no Rho
#ess.rho = effectiveSize(mpost$Rho)
#ess.rho

# we then examine the Gelman diagnostics, i.e. the Potential scale reduction factors
# this diagnostic compares if different chains (here we have 2 chains) give consistent results
# ideally the value of this diagnostic would be close to one
# as you increase thinning, you should observe the values getting closer to one
psrf.beta <- gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
hist(psrf.beta, xlab = expression("Potential scale reduction factor" ~ beta ~ ""))

psrf.V <- gelman.diag(mpost$V,multivariate=FALSE)$psrf
psrf.V

# elildo's note: not sure abount the meaning of the following, check in the manual:
# (furthermore it is not working anyway)
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
