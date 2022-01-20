setwd("P:/h572/hmsc_course/carvalhojr_elildo")
# OR ON A MAC:
# setwd("/Volumes/group/h572/hmsc_course/carvalhojr_elildo")
localDir = "."
ModelDir = file.path(localDir, "models")
DataDir = file.path(localDir, "data")
library(Hmsc)

load(file=file.path(DataDir,"allData.R")) #S,X,Y,Tr

S$unique_sample = paste(S$Camera.Trap.Name, S$Sampling.Event, sep = "_")
length(unique(S$unique_sample))

# Y is a  matrix of 1171 camera-trap records of 25 mammal genera (> 60 min apart) sampled annually
# from 2015-2019. Of these 1169 are unique (two are accidentally replicated), so are removed.

replicateID_N = which(as.vector(by(rep(1, nrow(S)), S$unique_sample, sum)==2))
replicateIDs = unique(S$unique_sample)[replicateID_N]
#The repicated samples are: "CT-EEM-2-29_2018" "CT-RBG-2-50_2017"
# These are removed, leaving 1169 samples in the full dataset.

S = S[-match(replicateIDs, S$unique_sample),]
X = X[-match(replicateIDs, S$unique_sample),]
Y = Y[-match(replicateIDs, S$unique_sample),]

# FURTHER DATA REDUCTION FOR FITTING A SIMPLE PILOT MODEL: random selection of 200 of the total original
# samples for relatively fast model fitting.

set.seed(1)
selrows = sample(nrow(S), 200)
S = droplevels(S[selrows,])
X = droplevels(X[selrows,])
Y = Y[selrows,]

# Check for absent (0) or ubiquitous species (1).
range(colMeans(Y>0))

min(colSums(Y>0))
# =1: check how many species are rare and exclude these. 
# Rare defined as < 5 plot occurrences in the pilot model, to limit processing time.

rarespecies = which(colSums(Y>0)<5)
length(rarespecies)
# =49 out of the original 25 species are rare in the pilot dataset. 
# Excluding these leaves 21 species in the dataset.
Y = Y[,-rarespecies]

hist(colMeans(Y>0),main="prevalence")
# Most species are rare nonetheless.
hist(as.matrix(log(Y[Y>0])),main="log abundance conditional on presence")

# Species are absent in many plots - if abundance is modelled, need a zero-inflated model. 
# Choice for the pilot model is a hurdle model: species presence-absence and log(abundance)
# separately.

hist(rowSums(Y>0))
# species richness distribution across samples.

plot(X)
# Proposed three X variables for pilot model are: elevation, dist.water, forest
# Sampling effort is also included, so that the effect of varying sampling effort
# (N days camera was in use) on the results can be directly estimated.

plot(X[, c("elevation", "dist.water", "forest", "effort")])
cor(X[, c("elevation", "dist.water", "forest", "effort")])

XFormula = ~elevation + dist.water + forest + effort

Tr = droplevels(Tr[-rarespecies,])
# Suggested pilot trait = log.BodyMass.Value
Tr$log.BodyMass.Value = log(Tr$BodyMass.Value)
summary(Tr)

TrFormula = ~log.BodyMass.Value

head(S)
plot(S$Longitude, S$Latitude)
# Note (Mirkka): Samples are highly aggregated within 7 areas (S$Project.Name)
# so I will not set the model up as spatially explicit, but I will
# include project name, year and unique sample as factor random effects.

studyDesign = data.frame(project = as.factor(S$Project.Name), year = as.factor(S$Sampling.Event), sample = as.factor(S$unique_sample))

Project = studyDesign$project
rL.project = HmscRandomLevel(units = levels(Project))

Year = studyDesign$year
rL.year = HmscRandomLevel(units = levels(Year))

Sample = studyDesign$sample
rL.sample = HmscRandomLevel(units = levels(Sample))

Ypa = 1*(Y>0)
Yabu = Y
Yabu[Y==0] = NA
Yabu=log(Yabu)

m1 = Hmsc(Y=Ypa, XData = X,  XFormula = XFormula,
          TrData = Tr,TrFormula = TrFormula,
          distr="probit",
          studyDesign=studyDesign,
          ranLevels={list("project" = rL.project, "year" = rL.year, "sample" = rL.sample)})

m2 = Hmsc(Y=Yabu, YScale = TRUE,
          XData = X,  XFormula = XFormula,
          TrData = Tr,TrFormula = TrFormula,
          distr="normal",
          studyDesign=studyDesign,
          ranLevels={list("project" = rL.project, "year" = rL.year, "sample" = rL.sample)})

models = list(m1,m2)
modelnames = c("presence_absence","abundance_COP")

save(models,modelnames,file = file.path(ModelDir, "unfitted_models"))

