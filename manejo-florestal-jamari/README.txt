This README.text file includes the basic information about the dataset coded as
carvalhojr_elildo

# MAIN QUESTION/AIM OF THE STUDY IN ONE SENTENCE:
How mammal abundance depends on environmental and spatial predictors, species traits and species interactions.

# SPECIES DATA (Y)
The species data consists of camera-trap records of 25 mammal genera (species in same genera were lumped for analysis).
Values represent the number of independent records (> 60 min).

# STUDY DESIGN (S)
The study design contains annual visits at camera-trap sites (approximately 30 trap-days per year).
The site id is in the column "Camera.Trap.Name" and its coordinates are in columns "Longitude" and "Latitude". Coordinates are in decimal degrees.
The year of visit is in the column "Sampling.Event".
Camera-trap sites are clustered by project, so we included "Project.Name" in S in case we want to use it as a random factor

# COVARIATES (X)
elevation (continuous): camera-trap site elevation in m.a.s.l.
slope (continuous): slope at camera-trap site
dist.water (continuous): distance to nearest water body in metres
hfi (continuous): human footprint index at camera-trap site
forest (continuous): forest cover in 250 m buffer around camera-trap site
grassland (continuous): grassland cover in 250 m buffer around camera-trap site
pasture (continuous): pasture cover in 250 m buffer around camera-trap site
water (continuous): open water cover in 250 m buffer around camera-trap site
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
BodyMass.Value (continuous): body mass

# HOW TO SET UP A REASONABLE PILOT MODEL FOR TrFormula:
The most important trait is: BodyMass.Value
Transformations suggested for continuous variables:
-log for BodyMass.Value

# PHYLOGENY (P)
The data does not include a phylogenetic tree

# CAN WE USE YOUR DATA AS EXAMPLE IN THE COURSE: YES

# DO YOU POTENTIALLY WISH TO COLLABORATE WITH YOUR DATA ANALYSES AT THE LEVEL OF CO-AUTHORSHIP: MAYBE
