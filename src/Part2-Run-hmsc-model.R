#
## Amazon mammal community structure
## Model with Y, X XY and T (no Pi and C matrix)
## Elildo Carvalho Jr @ ICMBio/CENAP 2020-07-12
#

## ---- The Model --------
# Y matrix: only species recorded in more than 10% of sites (n=7 sites)
# X matrix: distance to edge, proportion of burned trees, logging bouts and time since, tree density
# Pi matrix: no
# xy matrix: geo coordinates 
# T matrix: yes


## ---- load libraries --------
library(Hmsc)
library(here)


## set seed to ensure reproducibility of the results
rm(list = ls())
set.seed(1)


## ---- Load data --------

# Community matrix Y
Y <- apply(read.csv(here("data", "Y.csv")),2,as.numeric) # photo records per site corrected for effort
#Y <- Y[,2:ncol(Y)] # remove 1st column with Camera.Trap.Names which became NAs in the previous command
#Y[Y > 0] <- 1 # turn Y into a presence-absence matrix by replacing non-zero values with one

  # Optional: changing column order in Y (may be useful for variance partitioning figure)
  #(mns <- colMeans(Y, na.rm=TRUE)) # calculate column means, NAs omitted:
  #order(mns)
  #(Y <- Y[,order(mns)]) # reorder columns


# Covariate matrix X
X <-read.csv(here("data", "x1.csv"))
#X <-apply(read.csv(here("data", "x1.csv")),2,as.numeric)
#X <- X[,3:ncol(X)] # remove 1st column with Camera.Trap.Names
colnames(X) # checking
#X <- X[,-2] # removing tree density


# Random effects
#Pi <- read.csv("Pi_matrix.csv", header=T)
#Pi <- Pi[,c(2,1)] # just change the order, block first and camera second
#Pi <- Pi$block # only camera block

# xy matrix (spatial coordinates)
xy <- read.csv(here("data", "xy.csv"), header=T)
xy <- xy[,c(2:4)] # remove meaningless first column
xy$x <- as.numeric((xy$x)) # from integer to numeric
xy$y <- as.numeric((xy$y))


# Tr matrix
Tr <- read.csv(here("data", "Tr.csv"), header=T)
Tr <- Tr[,-1]
Tr$body.mass <- log(Tr$body.mass)
#Tr <- Tr[,2:3]

  ## Transposing Tr matrix:
  # first remember the names
#  n <- Tr$species
  # transpose all but the first column (species)
#  Tr <- as.data.frame(t(Tr[,2:ncol(Tr)])) # as.data.frame(t(Tr[,3:ncol(Tr)])) in the previous version
#  colnames(Tr) <- n


## ---- Create Hmsc object --------
m <- Hmsc(Y=Y, XData=X, XScale=TRUE, XFormula=~distance.to.river+forest.area+tree.density,
                   TrData=Tr, TrFormula=~body.mass+Fr.In+Sp.Fr, distr="lognormal poisson")

## ---- Run model and save results --------

# mcmc settings
thin <- 100
samples <- 500
nChains <- 3
set.seed(1)
ptm <- proc.time()

# run model
m <- sampleMcmc(m, samples = samples, thin = thin,
               adaptNf = rep(ceiling(0.4*samples*thin),1),
               transient = ceiling(0.5*samples*thin),
               nChains = nChains, nParallel = nChains,
               initPar = "fixed effects")
computational.time <- proc.time() - ptm

# save results
filename = file.path(here("results", paste("model_", as.character(m), "_",
                                     c("pa","abundance")[modeltype], "_thin_", ... = as.character(thin),
                                     "_samples_", as.character(samples), ".Rdata", sep = "")))
save(m, here("results", "m.Rdata"), computational.time)
save(m, file=filename, computational.time)

