# **3. Policy relevance**
#   
# _Input_: Criterea (e.g. IUCN threatened / Priority species / ...); Location
# 
# _Output_: Species list
# 
# _Metadata_: Criterea used

library(readxl)
library(tidyverse)

# read in file
list.files('data/taxonomy/')
pol <- read_xlsx('Data/taxonomy/butterfly_moth_orthop_conservation_designation.xlsx')
head(pol)

clean_pol <- read.csv('Data/taxonomy/species_policy_cleaned.csv')


### start of function

## currently doesn't consider location...?
criteria = 'Biodiversity Action Plan'

filter_policy_relevance <- function(criteria = c("Biodiversity Action Plan", "IUCN", "Habitats Directive", 
                                                 "Wildlife and Countryside Act 1981", "Northern Ireland Wildlife Order 1985"), # one or multiple of unique(clean_pol$rep_cat_clean)
                                    location = c(51.602436, -1.110557)){
  
  # get the species listed under the relevant criteria
  policy_relevance_spp <- clean_pol[clean_pol$rep_cat_clean %in% criteria,] %>% arrange(species)
  
  # create data frame
  policy_relevance <- data.frame(species = policy_relevance_spp$species,
                                 criteria = policy_relevance_spp$rep_cat_clean,
                                 status = policy_relevance_spp$desig_clean)
  
  # policy_relevance <- data.frame(species = 'Polyommatus bellargus',
  #                                status = 'LC')
  
  # set attributes
  attr(policy_relevance, which = 'criteria') <- criteria
  
  return(policy_relevance)
  
}



filter_policy_relevance <- function(criterea = 'iucn',
                                    location = c(51.602436, -1.110557)){
  
  
  policy_relevance <- data.frame(species = 'Polyommatus bellargus',
                                 status = 'LC')
  
  attr(policy_relevance, which = 'criterea') <- 'iucn'
  
  return(policy_relevance)
  
}

