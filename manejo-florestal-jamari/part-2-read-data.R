# Read and prepare data for HMSC analysis

library(here)

# You need to provide an SXY file.
# The files TP and P are optional, so indicate with TRUE/FALSE if they are included or not
is.TP <- TRUE
is.P <- FALSE

# READING IN SXY: study design (S) and/or covariates (X) and species data (Y) 
SXY <- read.csv(here("data", "SXY.csv"), stringsAsFactors=TRUE)
# Modify the next three lines to split your SXY file to components that relate to
# S: study design, including units of study and their possible coordinates (named as Route_x and Route_y to indicate that they relate to the units of Route)
# X: covariates to be used as predictors
# Y: species data
# If you don't have variables that define the study design, indicate this by S=NULL
# If you don't have covariate data, indicate this by X=NULL
names(SXY) # check names
S <- SXY[,1:3]
X <- SXY[,4:10]
Y <- SXY[,11:27]

head(S)
head(X)
head(Y)

# Check for absent (0) or ubiquitous species (1).
range(colMeans(Y>0))
min(colSums(Y>0))

# =1: check how many species are rare and exclude these. 
# Rare defined as < 5 plot occurrences in the pilot model, to limit processing time.

rarespecies = which(colSums(Y>0)<5)
length(rarespecies)
# =49 out of the original 25 species are rare in the pilot dataset. 
# Excluding these leaves 21 species in the dataset.
#Y = Y[,-rarespecies]


hist(colMeans(Y>0),main="prevalence")
# Most species are rare nonetheless
hist(as.matrix(log(Y[Y>0])),main="log abundance conditional on presence")


# Species are absent in many plots - if abundance is modeled, need a zero-inflated model. 
# Choice for the pilot model is a hurdle model: species presence-absence and log(abundance)
# separately.

hist(rowSums(Y>0))
# species richness distribution across samples.

plot(X)

# Proposed three X variables for model are: elevation, dist_water, logging_intensity etc
# sampling effort is also included

plot(X[, c("dummy_variable1", "dummy_variable2", "intensity_500", "effort")])
cor(X[, c("dummy_variable1", "dummy_variable2", "intensity_500", "effort")])


#Tr = droplevels(Tr[-rarespecies,])
# Suggested pilot trait = log.BodyMass.Value
#Tr$log.BodyMass.Value = log(Tr$BodyMass.Value)
#summary(Tr)

# What is not always easy is to decide what goes to S and what to X.
# As a general rule, include in S those variables that you think should be modelled as random effect,
# and in X those that you think should be modelled as fixed effects.
# Don't worry if you are not sure which one is the "right choice", we will discuss this with you.


# check that community data are numeric and have finite numbers. If the script
# writes "Y looks OK", you are ok.
if (is.numeric(as.matrix(Y)) || is.logical(as.matrix(Y)) && is.finite(sum(Y, na.rm=TRUE))) {
    print("Y looks OK")
} else {
	print("Y should be numeric and have finite values")	}

# Check that the stydy design data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(S))) {
  print("S has NA values - not allowed for")
} else {
  print("S looks ok")	}

# Check that the covariate data do not have missing values (they are allowed for Y but not S, X, P or Tr)
if (any(is.na(X))) {
  print("X has NA values - not allowed for")
} else {
  print("X looks ok")	}


# READING IN TP: traits (T) and/or phylogenetic information in table format (P)
if(is.TP){
  # Read in the species names as rownames, not as a column of the matrix
  #TP = read.csv("TP.csv", stringsAsFactors=TRUE,row.names = 1)
  TP = read.csv(here("data", "TP.csv"), stringsAsFactors=TRUE,row.names = 1)
  # The script below checks if the species names in TP are identical and in the same order as in Y
  # If the script prints "species names in TP and SXY match", you are ok.
  # If it says that they do not match, you need to modify the files so that they match 
  if(all(rownames(TP)==colnames(Y))) {
    print("species names in TP and SXY match")
  } else{
    print("species names in TP and SXY do not match")
  }
  # Modify the next two lines to split your TP file to components that relate to
  # Tr: species traits (note that T is a reserved word in R and that's why we use Tr)
  # P: phylogenetic information given by taxonomical levels, e.g. order, family, genus, species
  # If you don't have trait data, indicate this by Tr=NULL. 
  # If TP does not have phylogenetic data (because you don't have such data at all, or because
  # it is given in tree-format, like is the case in this example), indicate this with P=NULL 
  Tr = TP[,1:5]
  P = NULL
  # Check that the data looks as it should!
  #View(Tr)
  #View(P)
  # Check that the Tr data do not have missing values (they are allowed for Y but not S, X, P or Tr)
  if (any(is.na(Tr))) {
    print("Tr has NA values - not allowed for")
  } else {
    print("Tr looks ok")	}
  # Check that the phylogenetic/taxonomic data do not have missing values (they are allowed for Y but not S, X, P or Tr)
  if (any(is.na(P))) {
    print("P has NA values - not allowed for")
  } else {
    print("P looks ok")	}
}

# READING IN P: phylogenetic information in tree format (P)
# we use ape package for trees, and P.tre must be in a format that ape understands
if(is.P){
  # Read in the phylogenetic tree using read.tree from ape
  library(ape)
  P = read.tree("P.tre")
  # When you look at P (e.g. write P and press enter),
  # you should see that it is a phylogenetic tree which
  # is rooted and includes branch lengths and tip labels
  # The script below checks if the species names in P are identical (but not necessarily in the same order) as in Y
  # If the script prints "species names in P and SXY match", you are ok.
  # If it says that they do not match, you need to modify the files so that they match 
  if(all(sort(P$tip.label) == sort(colnames(Y)))){
    print("species names in P and SXY match")
  } else{
    print("species names in P and SXY do not match")
  }
  # Check that the data looks as it should!
  plot(P, cex=0.5)
}

