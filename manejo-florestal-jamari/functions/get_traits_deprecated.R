#----- TP: Traits data -----

# feeding guilds: another version using EltonTraits (more recent than PanTheria)
#traits <- read.csv("/home/elildojr/Documents/r/databases/MamFuncDat.txt", sep="\t") # read elton traits table
#traits$genus <- gsub(" .*$", "", traits$Scientific) # Create genus column in traits
#traits <- filter(traits, genus %in% generaToUse) # keeping only species in species.list
#traits <- distinct(traits, genus, .keep_all=TRUE) # remove duplicates in Genus
#write.csv(traits, here("data", "traits.csv"), row.names = FALSE)
traits <- as_tibble(gsheet2tbl("https://docs.google.com/spreadsheets/d/1HYNHvnZAQ3NPbR58KJZRWocaA46PPVT-AtfIIkVj4uc/edit?usp=sharing"))
traits

# Assign each species to a feeding guild
# e.g. primary consumer, secondary consumer, omnivore
# e.g. Oberosler et al. 2020 used:
# carnivore (>50% of diet based on vertebrates)
# herbivore (include grazers, browsers, granivores and frugivores, with >50% plant material)
# insectivore (>50% invertebrates),
# omnivore (generally both plant and animal material;
traits <- traits %>%
  mutate(herbivore = Diet.Fruit+Diet.Seed+Diet.PlantO,
         herbivore = ifelse(herbivore > 50, herbivore <- 1, herbivore <- 0),
         carnivore = Diet.Inv+Diet.Vend+Diet.Vunk,
         carnivore = ifelse(carnivore > 50, carnivore <- 1, carnivore <- 0),
         insectivore = Diet.Inv,
         insectivore = ifelse(insectivore > 50, insectivore <- 1, insectivore <- 0),
         omnivore = ifelse(carnivore+herbivore+insectivore > 0, omnivore <- 0, omnivore <- 1),
         omnivore = ifelse(carnivore+herbivore > 0, omnivore <- 0, omnivore <- 1)) %>%
  dplyr::select(genus, herbivore, carnivore, insectivore, omnivore, BodyMass.Value) %>%
  rename(body_mass = BodyMass.Value)

genus_names <- colnames(Y)
genus_names <- word(genus_names, 1, sep = "\\ ")
traits <- traits[match(genus_names, traits$genus),] # put rows in same order as in Y columns
TP <- traits
TP
# !NB bird traits missing from Elton Traits

