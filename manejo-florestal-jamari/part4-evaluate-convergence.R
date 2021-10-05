# THIS SCRIPT EVALUATES MCMC CONVERGENCE OF HMSC MODELS FOR THE PLANT EXAMPLE (SECTION 6.7) OF THE BOOK
# Ovaskainen, O. and Abrego, N. 2020. Joint Species Distribution Modelling - With Applications in R. Cambridge University Press.

# The preliminaries are as in script S1

# Change the working directory if necessary
# wd = "C:/HMSC_book/R-scripts/Section_6_7_plants"
# setwd(wd)

localDir = "."
data.directory = file.path(localDir, "data")
model.directory = file.path(localDir, "models")
library(Hmsc)
set.seed(1)

# The previous script saved the finished models saved the results in
# 'model.directory', and you can look at its content with
list.files(model.directory) #all models
list.files(model.directory, patt="model_pa_") #presence-absence models

# Here we explore the MCMC convergence of the presence-absence model only
# That of the abundance model could and should be explored similarly
# The main interest is in the "final" model that was run with the highest value of thin.
# However, we recommed runing the script first with setting thin = 1
# Then rerun it with thin = 10, thin = 100, ... to examine how the
# convergence statistics improve

nChains = 2
samples = 100
thin = 1 # try with thin = 1, thin = 10, thin = 100, etc.
filename=file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
load(filename)

# When you use load(), the dataset name is set to the same as when 
# you used the data (and that was 'model.pa' in our script). You shall
# not assign (use = or <-) your load result, but after you use
# load(), you have 'model.pa' in your workspace.

# We first extract the posterior distribution from the model object and convert
# it into a coda object.
# Note that Hmsc uses coda only to examine convergence, whereas other operations
# (e.g. exploring of parameters or making predictions) are conducted straigt
# from the Hmsc object model.pa rather than the coda object.

mpost = convertToCodaObject(model.pa)

# We will explore the MCMC convergence for the beta-parameters (species niches),
# V-parameters (variation in species niches),
# and the rho-parameter (phylogenetic signal)
# These are just examples; you may use names(mpost) see what other parameters were estimated

# We first examine the effective size of the posterior sample.
# As there are two chains with a sample of 100 from each, the actual sample size is 200.
# Thus, ideally the effective sample size would be 200 as well.
# But in presense of autocorrelation, the effective sample size will be lower than the actual sample size

ess.beta = effectiveSize(mpost$Beta)
# Our model contains 150 beta-parameters (2 for each of the 75 species)
# Thus we have 150 effective sample sizes to look at. This is best done with a histogram
hist(ess.beta, xlab = expression("Effective sample size" ~ beta ~ ""))
ess.V = effectiveSize(mpost$V)
# As ess.V has only four entries, we just look at it directlt instead of
# drawing a histogram
ess.V
ess.rho = effectiveSize(mpost$Rho)
ess.rho

# We then examine the Gelman diagnostics, i.e. the Potential scale reduction factors
# This diagnostic compares if different chains (here we have 2 chains) give consistent results
# Ideally the value of this diagnostic would be close to one.
# As you increase thinning, you should observe the values getting closer to one.
# The Gelman diagnostic is often more informative diagnostic than the effective sample size, so if you will use only one of these, we recommend the Gelman diagnostic

psrf.beta = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
hist(psrf.beta, xlab = expression("Potential scale reduction factor" ~ beta ~ ""))
psrf.V = gelman.diag(mpost$V,multivariate=FALSE)$psrf
psrf.V
psrf.rho = gelman.diag(mpost$Rho,multivariate=FALSE)$psrf
psrf.rho
