# function to fidata species names from Jamari dataset
# Elildo Carvalho Jr @ ICMBio/CENAP, 2020-04-07

f.fix.species.names <- function(data)  {
  data <- data[!(data$bin==" "),]
  levels(data$bin)[levels(data$bin)=="Aramides unknown"] <- "Aramides cajaneus"     # rename by name
  data <- data[!(data$bin=="Aramides cajaneus"),]
  levels(data$bin)[levels(data$bin)=="Atelocynus unknown"] <- "Atelocynus microtis" 
  data <- data[!(data$bin=="Baryphthengus martii"),]
  data <- data[!(data$bin=="Buteogallus urubitinga"),]
  data <- data[!(data$bin=="Campephilus rubricollis"),]    
  data <- data[!(data$bin=="Cebus apella"),]                                          # delete
  levels(data$bin)[levels(data$bin)=="Cerdocyon unknown"] <- "Cerdocyon thous" 
  data <- data[!(data$bin=="Choloepus hoffmanni"),]
  levels(data$bin)[levels(data$bin)=="Crypturellus cinereus"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus soui"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus strigulosus"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus tataupa"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus undulatus"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus unknown"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus NA"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Crypturellus variegatus"] <- "Crypturellus spp"
  levels(data$bin)[levels(data$bin)=="Dasyprocta fuliginosa"] <- "Dasyprocta punctata"
  levels(data$bin)[levels(data$bin)=="Dasyprocta prymnolopha"] <- "Dasyprocta punctata"
  levels(data$bin)[levels(data$bin)=="Dasyprocta unknown"] <- "Dasyprocta punctata"
  levels(data$bin)[levels(data$bin)=="Dasypus kappleri"] <- "Dasypus spp"
  levels(data$bin)[levels(data$bin)=="Dasypus NA"] <- "Dasypus spp"
  levels(data$bin)[levels(data$bin)=="Dasypus novemcinctus"] <- "Dasypus spp"
  levels(data$bin)[levels(data$bin)=="Dasypus unknown"] <- "Dasypus spp"
  data <- data[!(data$bin=="Dendrocolaptes NA"),]
  levels(data$bin)[levels(data$bin)=="Didelphis albiventris"] <- "Didelphis marsupialis"
  levels(data$bin)[levels(data$bin)=="Didelphis NA"] <- "Didelphis marsupialis"
  data <- data[!(data$bin=="Geotrygon montana"),]
  data <- data[!(data$bin=="Homo sapiens"),]
  levels(data$bin)[levels(data$bin)=="Leopardus NA"] <- "Leopardus spp"
  levels(data$bin)[levels(data$bin)=="Leopardus pardalis"] <- "Leopardus spp"
  levels(data$bin)[levels(data$bin)=="Leopardus unknown"] <- "Leopardus spp"
  levels(data$bin)[levels(data$bin)=="Leopardus wiedii"] <- "Leopardus spp"
  data <- data[!(data$bin=="Leontocebus fuscicollis"),]
  data <- data[!(data$bin=="Leptotila rufaxilla"),]
  data <- data[!(data$bin=="Leptotila unknown"),]
  data <- data[!(data$bin=="Leptotila NA"),]
  levels(data$bin)[levels(data$bin)=="Mazama americana"] <- "Mazama spp"
  levels(data$bin)[levels(data$bin)=="Mazama nemorivaga"] <- "Mazama spp"
  levels(data$bin)[levels(data$bin)=="Mazama unknown"] <- "Mazama spp"
  levels(data$bin)[levels(data$bin)=="Mazama NA"] <- "Mazama spp"
  #data <- data[!(data$bin=="Mazama unknown"),]
  data <- data[!(data$bin=="Mellivora capensis"),]
  data <- data[!(data$bin=="Mico rondoni"),]
  data <- data[!(data$bin=="Mico unknown"),]
  levels(data$bin)[levels(data$bin)=="Mitu tuberosum"] <- "Pauxi tuberosa" 
  data <- data[!(data$bin=="Momotus momota"),]
  data <- data[!(data$bin=="Monasa morphoeus"),]
  data <- data[!(data$bin=="Monasa nigrifrons"),]
  data <- data[!(data$bin=="Myrmothera campanisona"),]
  data <- data[!(data$bin=="Neomorphus geoffroyi"),]
  data <- data[!(data$bin=="Nothocrax urumutum"),]
  levels(data$bin)[levels(data$bin)=="Odontophorus gujanensis"] <- "Odontophorus spp"
  levels(data$bin)[levels(data$bin)=="Odontophorus stellatus"] <- "Odontophorus spp"
  levels(data$bin)[levels(data$bin)=="Odontophorus unknown"] <- "Odontophorus spp"
  levels(data$bin)[levels(data$bin)=="Odontophorus NA"] <- "Odontophorus spp"
  levels(data$bin)[levels(data$bin)=="Panthera pardus"] <- "Panthera onca" 
  data <- data[!(data$bin=="Patagioenas plumbea"),]
  levels(data$bin)[levels(data$bin)=="Pauxi pauxi"] <- "Pauxi tuberosa" 
  levels(data$bin)[levels(data$bin)=="Pauxi unicornis"] <- "Pauxi tuberosa" 
  levels(data$bin)[levels(data$bin)=="Pauxi unknown"] <- "Pauxi tuberosa" 
  levels(data$bin)[levels(data$bin)=="Penelope jacquacu"] <- "Penelope spp" 
  levels(data$bin)[levels(data$bin)=="Penelope pileata"] <- "Penelope spp" 
  levels(data$bin)[levels(data$bin)=="Penelope superciliaris"] <- "Penelope spp" 
  levels(data$bin)[levels(data$bin)=="Penelope unknown"] <- "Penelope spp"
  levels(data$bin)[levels(data$bin)=="Penelope NA"] <- "Penelope spp"
  data <- data[!(data$bin=="Pilherodius pileatus"),]
  data <- data[!(data$bin=="Plecturocebus brunneus"),]
  levels(data$bin)[levels(data$bin)=="Psophia unknown"] <- "Psophia viridis" 
  data <- data[!(data$bin=="Sapajus apella"),]
  data <- data[!(data$bin=="Saguinus fuscicollis"),]
  data <- data[!(data$bin=="Saguinus unknown"),]
  levels(data$bin)[levels(data$bin)=="Sciurus spadiceus"] <- "Sciurus sp"
  levels(data$bin)[levels(data$bin)=="Sciurus NA"] <- "Sciurus sp"
  levels(data$bin)[levels(data$bin)=="Sciurus unknown"] <- "Sciurus sp" 
  levels(data$bin)[levels(data$bin)=="Sciurus griseus"] <- "Sciurus sp" 
  levels(data$bin)[levels(data$bin)=="Sylvilagus unknown"] <- "Sylvilagus brasiliensis" 
  data <- data[!(data$bin=="Sus scrofa"),] # remove Sus, but check later if ID is right
  levels(data$bin)[levels(data$bin)=="Tapirus unknown"] <- "Tapirus terrestris"
  levels(data$bin)[levels(data$bin)=="Tapirus NA"] <- "Tapirus terrestris"
  levels(data$bin)[levels(data$bin)=="Tinamus guttatus"] <- "Tinamus spp" 
  levels(data$bin)[levels(data$bin)=="Tinamus major"] <- "Tinamus spp" 
  levels(data$bin)[levels(data$bin)=="Tinamus solitarius"] <- "Tinamus spp"
  levels(data$bin)[levels(data$bin)=="Tinamus tao"] <- "Tinamus spp" 
  levels(data$bin)[levels(data$bin)=="Tinamus unknown"] <- "Tinamus spp" 
  levels(data$bin)[levels(data$bin)=="Tinamus NA"] <- "Tinamus spp" 
  data <- data[!(data$bin==" "),] # remove empty " " rows
  data <- data[!(data$bin=="No CV Result No CV Result"),] # remove empty " " rows
  data <- data[!(data$bin=="NA NA"),] # remove empty " " rows
  data <- data[!is.na(data$bin),]
  data <- data[!(data$bin=="Unknown Unknown"),]
  data$bin <- factor(data$bin)
  assign("dataTemp", data, envir=.GlobalEnv)
  }
  
