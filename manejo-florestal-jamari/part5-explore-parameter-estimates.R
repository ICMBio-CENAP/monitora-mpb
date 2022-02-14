# THIS SCRIPT EXPLORES THE PARAMETER ESTIMATES OF HMSC MODELS FOR THE PLANT EXAMPLE (SECTION 6.7) OF THE BOOK
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

# We set the parameters of the MCMC run for which we wish to load the model object
# You may change the thin parameter to the highest thin for which you have fitted the model, 
# to get as reliable results as possible
# You may also choose whether you wish to examine the presence-absence or abundance model
# by commenting the loading of the model that you wish not to examine
# Note that the model is called "m", so that the same code applies whether 
# you read originally the model.pa or the model.abu

nChains <- 2
samples <- 100
thin <- 1
#filename <- file.path(model.directory, paste0("model_pa_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
#load(filename)
m <- model.pa
#filename=file.path(model.directory, paste0("model_abu_chains_",as.character(nChains),"_samples_",as.character(samples),"_thin_",as.character(thin)))
#load(filename)
#m = model.abu

# We first examine the beta-parameters, i.e. the species niches
# The parameter estimates of species niches show that many species respond 
# negatively to the topographic moisture gradient.
# This means that they are more likely to be present (model.pa) and more 
# abundant (model.abu) in sites with low value of TMG,
# i.e. in sites located in mesic, north-facing slopes.

postBeta <- getPostEstimate(m, parName="Beta")
?plotBeta
par(mar=c(6,10,1,1))
plotBeta(m, post=postBeta, param="Support", supportLevel = 0.95, spNamesNumbers = c(T,T), covNamesNumbers = c(T,F))
dev.off()

m$

# We next examine the gamma-parameters, i.e. the influence of environmental
# covariates to expected species niches
# These parameter estimates addresses the main question that we were
# interested in this case study:
# is there an association between environmental conditions and species traits?
# For both models, you should find a negative relationship between the "trait intercept" and TMG,
# and a positive relationship between C:N ratio and TMG.
# As the species traits are scaled to zero mean, the result related to the intercept
# means a typical species (i.e., that with zero C:N ratio) responds negatively to TMG.
# This reflects the results of species niches, which shows that many species respond negatively to TMG.
# The positive relationship between C:N ratio and TMG means that those species with
# a low C:N ratio respond especially negatively to TMG, whereas species with high C:N ratio
# may respond even positively to it.
# The results further show a positive relationship between C:N ratio and the
# "environmental covariate intercept". As the environmental covariates have been
# scaled to have a zero mean, also this result can be interpreted.
# It means that species with high C:N ratio are on average more common
# (in terms of both occurrence and abundance) than species with low C:N ratio.

postGamma = getPostEstimate(m, parName="Gamma")
plotGamma(m, post=postGamma, param="Support", supportLevel = 0.95, covNamesNumbers = c(T,F), trNamesNumbers = c(T,F), colorLevels = 3)

# We note that in line with these analyses, Miller et al. (2018) found a strong negative
# main effect of TMG on abundance, and a strong positive main effect of C:N ratio on abundance.
# Regarding the relationship between C:N ratio and TMG, Miller et al. (2018) found support
# with some methods but no support with some other methods. Based on the Hmsc analyses,
# there is good evidence for such a signal both in the presence-absence pattern, as well as
# in the abundance pattern. Thus, our analyses give support for the original hypothesis
# that species occurring on drier and warmer sites have on average higher C:N ratio
# than those occurring in more moist and cooler sites.

# We next construct gradient plots to illustrate how the species community varies along a given environmental gradient
# These results show that species richness (generated with measure="S") decreases with TMG,
# and that community-weighted mean of C:N ratio (generated with measure="T") is on average positive and increases with TMG.
# Note that with measure="T", we have set index=2, to select the second trait, which is that of CN (the first one is the intercept)

Gradient = constructGradient(m,focalVariable = "elevation")
predY = predict(m,Gradient = Gradient, expected = TRUE)
prob = c(0.25,0.5,0.75)
plotGradient(m, Gradient, pred=predY, measure="S", showData = TRUE, q = prob) # prob should be q
plotGradient(m, Gradient, pred=predY, measure="T", index=2, showData = TRUE,  q = prob) # prob should be q

Gradient = constructGradient(m,focalVariable = "dist_water")
predY = predict(m,Gradient = Gradient, expected = TRUE)
prob = c(0.25,0.5,0.75)
plotGradient(m, Gradient, pred=predY, measure="S", showData = TRUE, q = prob) # prob should be q
plotGradient(m, Gradient, pred=predY, measure="T", index=2, showData = TRUE,  q = prob) # prob should be q


# To ask how much C:N ratio explains out of the variation in species niches and occurrences,
# we next compute the variance partitioning.
# We observe that the C:N ratio explains not only how species respond to TMG,
# but also a substantial variation in the species intercepts, reflecting the discussion above.

VP = computeVariancePartitioning(m, group=c(1,1),groupnames = "TMG")
VP$R2T

# We finally ask if there is evidence phylogenetic signal in the residual variation in species niches,
# on top of what can be explained by the trait C:N ratio.
# There is not specific Hmsc functionality for doing so, but we may use the summary function of the coda package
# We do not find such evidence, as zero is not excluded from the main part
# of the posterior distribution. Hence we conclude that related species do not respond more
# similarly to the TMG than unrelated species, beyond to what can be expected based on their C:N ratio.

mpost = convertToCodaObject(m)
summary(mpost$Rho)$quant

# All of the above results are based on the fitted model.
# But what we did not address yet is how good is the model.
# One way of examining this is to evaluate the model fit. This is done with the script below
# We evaluate model fit here in terms of how well the model is able to discriminate presences and absences
# This can be done by computing the model's explanatory power in terms of e.g. TjurR2 or AUC
# Below we make a plot that compares these two to each other.
# Note that the AUC values are much higher than the TjurR2 values, even if they measure the fit of exactly the same model.
# Thus what is high or low value of model fit depends very much of the units used to measure it.

predY = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=predY)
plot(MF$TjurR2,MF$AUC)

# Above we measured the explanatory power of the model.
# If adding more and more covariates (or random effects), the model can eventually explain all the data
# Thus it is important to evaluate more critically also the predictive power of the model
# Predictive power asks how well the model is able to explain data that was not used for model fitting
# Below we evaluate predictive power by cross-validation
# As cross-validation requires re-fitting the model many times, this can take a lot of time
# Thus it is best to examine predictive power first with thin=1, and leave longer runs later e.g. overnight
# In models that take much longer to fit, it is better to save
# the cross-validation results so that you can return back to them
# without running cross-validation again (see e.g. the bird example of Section 11_1)

partition = createPartition(m, nfolds = 2)
predY = computePredictedValues(m, partition = partition, nParallel = 2)

# Note that in computePredicted values it is also possible to use the nParallel option
# Below we construct a plot that compares explanatory power (MF) to predictive power (MFCV)
# As expected, the explanatory power is higher than the predictive power

MFCV = evaluateModelFit(hM=m, predY=predY)
plot(MF$AUC, MFCV$AUC)
abline(0,1)

