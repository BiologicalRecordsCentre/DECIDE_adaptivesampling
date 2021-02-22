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
pol <- read_xlsx('data/taxonomy/butterfly_moth_orthop_conservation_designation.xlsx')
head(pol)


filter_policy_relevance <- function(criterea = 'iucn',
                                    location = c(51.602436, -1.110557)){
  
  
  policy_relevance <- data.frame(species = 'Polyommatus bellargus',
                                 status = 'LC')
  
  attr(policy_relevance, which = 'criterea') <- 'iucn'
  
  return(policy_relevance)
  
}

