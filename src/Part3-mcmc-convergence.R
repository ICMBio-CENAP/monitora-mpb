
# Part 3

## Examine MCMC convergence

# Compute mixing statistics
thin = 100
samples = 1000
nChains = 4
comp.time = matrix(nrow=2, ncol=3)
for (modeltype in 1:2){
  for (model in 1:3){
    filename = file.path(ModelDir, paste("model_",as.character(model),"_",
                                         c("pa","abundance")[modeltype],
                                         "_chains_",as.character(nChains),
                                         "_thin_", as.character(thin),"_samples_",
                                         as.character(samples),
                                         ".Rdata",sep = ""))
    load(filename)
    comp.time[modeltype,model] = computational.time[1]
    mpost = convertToCodaObject(m)
    es.beta = effectiveSize(mpost$Beta)
    ge.beta = gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
    es.gamma = effectiveSize(mpost$Gamma)
    ge.gamma = gelman.diag(mpost$Gamma,multivariate=FALSE)$psrf
    es.rho
ge.rho
es.V =
  ge.V =
  = effectiveSize(mpost$Rho)
= gelman.diag(mpost$Rho,multivariate=FALSE)$psrf
effectiveSize(mpost$V)
gelman.diag(mpost$V,multivariate=FALSE)$psrf
if (model==2){
  es.omega = NA
  ge.omega = NA
} else {
  es.omega = effectiveSize(mpost$Omega[[1]])
  ge.omega = gelman.diag(mpost$Omega[[1]],multivariate=FALSE)$psrf
}
mixing = list(es.beta=es.beta, ge.beta=ge.beta,
              es.gamma=es.gamma, ge.gamma=ge.gamma,
              es.rho=es.rho, ge.rho=ge.rho,
              es.V=es.V, ge.V=ge.V,
              es.omega=es.omega, ge.omega=ge.omega)
filename = file.path(MixingDir, paste("mixing_",as.character(model),"_",
                                      c("pa","abundance")[modeltype],
                                      "_chains_",as.character(nChains),
                                      "_thin_", as.character(thin),"_samples_",
                                      as.character(samples),
                                      ".Rdata",sep = ""))
save(file=filename, mixing)
  }}


# Model fit and explanatory power

thin = 100
samples = 1000
nChains = 4
for(modeltype in c(1,2)){
  for (model in c(1,2,3)){
    filename = file.path(ModelDir, paste("model_",as.character(model),
                                         c("_pa","_abundance")[modeltype],
                                         "_chains_",as.character(nChains),
                                         "_thin_", as.character(thin),
                                         "_samples_", as.character(samples),
                                         ".Rdata",sep = ""))
    load(filename)
    set.seed(1)
    predY = computePredictedValues(m, expected=FALSE)
    MF = evaluateModelFit(hM=m, predY=predY)
    filename = file.path(MFDir, paste("model_",as.character(model),
                                      c("_pa","_abundance")[modeltype],
                                      "_chains_",as.character(nChains),
                                      "_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      ".Rdata",sep = ""))
    save(file=filename, MF)
  }
}


# Predictive power

for(modeltype in c(1,2)){
  for (model in c(1,2,3)){
    filename = file.path(ModelDir, paste("model_",as.character(model),
                                         c("_pa","_abundance")[modeltype],
                                         "_chains_",as.character(nChains),
                                         "_thin_", as.character(thin),
                                         "_samples_", as.character(samples),
                                         ".Rdata",sep = ""))
    load(filename)
    set.seed(1)
    partition=createPartition(hM=m, nfolds=4, column="Route")
    predY = computePredictedValues(m, expected=FALSE, partition=partition,
                                   nCores = length(m$postList))
    MFCV = evaluateModelFit(hM=m, predY=predY)
    filename = file.path(MFDir, paste("model_CV_",as.character(model),
                                      c("_pa","_abundance")[modeltype],
                                      "_chains_",as.character(nChains),
                                      "_thin_", as.character(thin),
                                      "_samples_", as.character(samples),
                                      ".Rdata",sep = ""))
    save(file=filename, MFCV)
  }
}


