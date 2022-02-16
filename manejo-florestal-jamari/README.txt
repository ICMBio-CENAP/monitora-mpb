This README.text file includes the basic information about the dataset coded as
carvalhojr_elildo

# MAIN QUESTION/AIM OF THE STUDY IN ONE SENTENCE:
Effects of selective logging on vertebrate assemblage

# SPECIES DATA (Y)
The species data consists of camera-trap records of bird and mammal genera (species in same genera were lumped).
Values represent the number of independent records (> 60 min).

# STUDY DESIGN (S)
The study design contains annual visits at camera-trap sites (approximately 30 trap-days per year).
The site id is in the column "placename" and its coordinates are in columns "longitude" and "latitude". Coordinates are in decimal degrees.
The year of visit is in the column "sampling_event".

# COVARIATES (X)
NB! to be added, for now we are using a mix of real and dummy covariates for testing
elevation (continuous): camera-trap site elevation in m.a.s.l.
slope (continuous): slope at camera-trap site
dist.water (continuous): distance to nearest water body in metres
hfi (continuous): human footprint index at camera-trap site
effort (continuous): number of days each camera was active in each visit 


# HOW TO SET UP A REASONABLE PILOT MODEL FOR XFormula:
The three most important predictors are: elevation, dist.water, forest
However, effort also should be included to control for variation in effort among sites
No transformations are suggested

# TRAITS (Tr)
herbivore (binary): herbivore or not herbivore
carnivore (binary): carnivore or not carnivore
insectivore (binary): insectivore or not insectivore
omnivore (binary): omnivore or not omnivore
body_mass (continuous): body mass

# HOW TO SET UP A REASONABLE PILOT MODEL FOR TrFormula:
The most important trait is: body_mass
Transformations suggested for continuous variables:
-log for body_mass

# PHYLOGENY (P)
The data does not include a phylogenetic tree

# HOW COVARIATES WERE PREPARED
merge_tree_shapefiles-alternative-take.R
jamari_get_covariates.R
part1-prep-data.R