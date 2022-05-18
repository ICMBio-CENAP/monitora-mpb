# Explore parameter estimates of models for Jamari National Forest


library(here)
library(tidyverse)
library(Hmsc)
library(corrplot)
set.seed(1)

data.directory = here("manejo-florestal-jamari", "data")
model.directory = here("manejo-florestal-jamari", "models")


# We set the parameters of the MCMC run for which we wish to load the model object
# You may change the thin parameter to the highest thin for which you have fitted the model, 
# to get as reliable results as possible
# You may also choose whether you wish to examine the presence-absence or abundance model
# by commenting the loading of the model that you wish not to examine
# Note that the model is called "m", so that the same code applies whether 
# you read originally the model.pa or the model.abu

nChains <- 2
samples <- 50000
thin <- 250

models <- load(here("manejo-florestal-jamari", "models",
                    "model_pa_chains_2_samples_50000_thin_1"))
models

m <- model.pa


#----- Beta parameters (species niches, i.e. responses to covariates)

postBeta <- getPostEstimate(m, parName="Beta")
postBeta

# the command above provides just summaries but we want the quantiles
mpost <- convertToCodaObject(m)
print(summary(mpost$Beta))
summary(mpost$Beta)[["quantiles"]]


# convert to tibble to make it friendlier and highlight negative values
betas <- tibble(parameter = dimnames(summary(mpost$Beta)[["quantiles"]])[[1]],
                lower = summary(mpost$Beta)[["quantiles"]][,1],
                mean = summary(mpost$Beta)[["quantiles"]][,3], # mean or median?
                upper = summary(mpost$Beta)[["quantiles"]][,5] ) %>%
  separate(parameter, c("predictor", "genus"), sep=",") %>%
  mutate(predictor = str_extract(predictor, m$covNames),
         genus = str_trim(genus),
         genus = word(genus, 1) ) %>%
  select(genus, predictor, mean, lower, upper) %>%
  print(n = Inf)


# significant betas for logging intensity:
betas %>%
  filter(predictor == "intensity_500") %>%
  filter(lower < 0 & upper < 0 | lower > 0 & upper > 0) %>%
  print(n = Inf)

# significant betas for distance to water:
betas %>%
  filter(predictor == "dist_water") %>%
  filter(lower < 0 & upper < 0 | lower > 0 & upper > 0) %>%
  print(n = Inf)

# significant betas for sampling effort:
betas %>%
  filter(predictor == "effort") %>%
  filter(lower < 0 & upper < 0 | lower > 0 & upper > 0) %>%
  print(n = Inf)


# do this to check individual species responses:
# (e.g., for Dasyprocta)
betas %>%
  filter(genus == "Dasyprocta")


#----- Plot Beta parameters

# plot overall species vs predictor betas
par(mar=c(6,10,1,1))
plotBeta(m, post=postBeta, param="Support", supportLevel = 0.95, spNamesNumbers = c(T,T), covNamesNumbers = c(T,F))
dev.off()


#----- Gradient plots

# illustrate how the species community varies along a given environmental gradient
# can be done for species richness, individual species, traits etc (see help) 
# ideally do only for species with significant beta effects

# plot prediction for species richness given by index
Gradient <- constructGradient(m, focalVariable = "intensity_500")
predY <- predict(m, Gradient = Gradient, expected = TRUE)
prob <- c(0.25,0.5,0.75)

plotGradient(m, Gradient, pred=predY, measure="S", showData = TRUE, q = prob) # prob should be q


# plot prediction for individual species given by index
# to find The index for each species check m$spNames
# logging intensity significantly affected Pecari, Tayassu and Dasypus

# Pecari (index = 4)
plotGradient(m, Gradient, pred=predY, measure="Y", index=4, las=1,
             showData = TRUE, main='occurrence (measure="Y")',
             xlab = "Logging intensity")
# version without margin text
plotGradient(m, Gradient, pred=predY, measure="Y", index=4, las=1,
             showData = TRUE, main='occurrence (measure="Y")',
             xlab = "Logging intensity", ylab = "Pecari (occurrence)",
             showPosteriorSupport = FALSE)


# Tayassu  (index = 10)
plotGradient(m, Gradient, pred=predY, measure="Y", index=10, las=1,
             showData = TRUE, main='occurrence (measure="Y")',
             xlab = "Logging intensity", ylab = "Tayassu (occurrence)",
             showPosteriorSupport = FALSE)

# Dasypus  (index = 15)
plotGradient(m, Gradient, pred=predY, measure="Y", index=15, las=1,
             showData = TRUE, main='occurrence (measure="Y")',
             xlab = "Logging intensity", ylab = "Dasypus (occurrence)",
             showPosteriorSupport = FALSE)


# effect of sampling effort
# signigicant for Pecari, Leopardus, Nasua, Tamandua, Didelphis and Eira
# do a multipanel plot

Gradient <- constructGradient(m, focalVariable = "effort")
predY <- predict(m, Gradient = Gradient, expected = TRUE)
prob <- c(0.25,0.5,0.75)

par(mfrow = c(3,2))
# Pecari
plotGradient(m, Gradient, pred=predY, measure="Y", index=4, las=1,
             showData = TRUE, main='',
             xlab = "Sampling effort (days)", ylab = "Dasypus (occurrence)",
             showPosteriorSupport = FALSE)
# Leopardus
plotGradient(m, Gradient, pred=predY, measure="Y", index=7, las=1,
             showData = TRUE, main='',
             xlab = "Sampling effort (days)", ylab = "Leopardus (occurrence)",
             showPosteriorSupport = FALSE)
# Nasua
plotGradient(m, Gradient, pred=predY, measure="Y", index=9, las=1,
             showData = TRUE, main='',
             xlab = "Sampling effort (days)", ylab = "Nasua (occurrence)",
             showPosteriorSupport = FALSE)
# Tamandua
plotGradient(m, Gradient, pred=predY, measure="Y", index=12, las=1,
             showData = TRUE, main='',
             xlab = "Sampling effort (days)", ylab = "Tamandua (occurrence)",
             showPosteriorSupport = FALSE)
# Didelphis
plotGradient(m, Gradient, pred=predY, measure="Y", index=13, las=1,
             showData = TRUE, main='',
             xlab = "Sampling effort (days)", ylab = "Didelphis (occurrence)",
             showPosteriorSupport = FALSE)
# Eira
plotGradient(m, Gradient, pred=predY, measure="Y", index=14, las=1,
             showData = TRUE, main='',
             xlab = "Sampling effort (days)", ylab = "Eira (occurrence)",
             showPosteriorSupport = FALSE)


# save last plot as jpeg
# ggsave only works for ggplot...
#ggsave('nasua_vs_intensity.jpeg', here("manejo-florestal-jamari", "results"))


# make a plot to see if species commonness or rarity affects beta for effor
test_x <- 1*m$Y %>%
  colMeans()/dim(m$Y)[1]
test_y <- betas %>%
  filter(predictor == "effort") %>%
  pull(mean)
dev.off()
plot(test_x, test_y, xlab="Prevalence", ylab="beta for effort", pch=16, cex=1.5)



#----- Gamma parameters (trait effects on species’ environmental responses)

postGamma <- getPostEstimate(m, parName="Gamma")
postGamma
print(summary(mpost$Gamma))
summary(mpost$Gamma)[["quantiles"]]

# plot
plotGamma(m, post=postGamma, param="Support", supportLevel = 0.95, covNamesNumbers = c(T,F), trNamesNumbers = c(T,F), colorLevels = 3)

# convert to tibble to make it friendlier and highlight negative values
gammas <- tibble(parameter = dimnames(summary(mpost$Gamma)[["quantiles"]])[[1]],
                lower = summary(mpost$Gamma)[["quantiles"]][,1],
                mean = summary(mpost$Gamma)[["quantiles"]][,3], # mean or median?
                upper = summary(mpost$Gamma)[["quantiles"]][,5] ) %>%
  separate(parameter, c("predictor", "trait"), sep=",") %>%
  mutate(predictor = str_extract(predictor, m$covNames),
         trait = str_trim(trait),
         trait = word(trait, 1) ) %>%
  select(trait, predictor, mean, lower, upper) %>%
  print(n = Inf)



# plot community-weighed mean values of traits given by index
# e.g. body mass (index = 2, see m$trNames)
Gradient = constructGradient(m, focalVariable = "intensity_500")
predY = predict(m, Gradient = Gradient, expected = TRUE)
prob = c(0.25,0.5,0.75)
plotGradient(m, Gradient, pred=predY, measure="T", index=2, las=1,
             showData = TRUE, main='Mean body mass (measure="T")')



#----- species associations

OmegaCor <- computeAssociations(m)
OmegaCor
supportLevel <- 0.95
for (r in 1:m$nr){
  plotOrder = corrMatOrder(OmegaCor[[r]]$mean,order="AOE")
  toPlot = ((OmegaCor[[r]]$support>supportLevel) +
              (OmegaCor[[r]]$support<(1-supportLevel))>0)*OmegaCor[[r]]$mean
  par(xpd=T)
  colnames(toPlot)=rownames(toPlot)=gsub("_"," ",x=colnames(toPlot))
  corrplot(toPlot[plotOrder,plotOrder], method = "color",
           col=colorRampPalette(c("blue","white","red"))(200),
           title="",type="lower",tl.col="black",tl.cex=.7, mar=c(0,0,6,0))
}



#----- variance partitioning

# compute variance partitioning
VP <- computeVariancePartitioning(m) # we can group predictors for computing/plotting, see help
VP$vals # variance proportion for each group and species
VP$R2T # variance among species explained by traits
VP$R2T$Beta
VP$R2T$Y

# plot variance partitioning
plotVariancePartitioning(m, VP) 
# (the plot can be edited with additional parameters passed to the barplot function)



#----- Model fit

# how well the model is able to discriminate presences and absences
# This can be done by computing the model's explanatory power in terms of e.g. TjurR2 or AUC
# Below we make a plot that compares these two to each other.
# Note that the AUC values are much higher than the TjurR2 values, even if they measure the fit of exactly the same model.
# Thus what is high or low value of model fit depends very much of the units used to measure it.

predY = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=predY)
plot(MF$TjurR2,MF$AUC)

# Above we measured the explanatory power of the model.
# now evaluate predictive power of the model
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

