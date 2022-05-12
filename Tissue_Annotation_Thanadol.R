## Script name: Vennpieagram_GO_reliability
##
## Purpose of script: Make overlaying pie-diagrams that will show the different GO terms present
##                    on each gene as well as what NeXtProt evidence and which 
##
## Author: Rutger Schutten
##
## Date Created: 2021-12-13
##
## Copyright (c) Rutger Schutten, 2022
## Email: rutger.schutten@igp.uu.se
##
## --------------------------- ##
##
## Notes:
##   
##
## --------------------------- ##

####Data preparation####
library('tidyverse')
library('data.table')
library(dplyr)
#library(forcats)
library(tidyr)
# setwd("/Users/thasu980/Documents/Rutger_upset")
#setwd("//argos.rudbeck.uu.se/MyGroups$/Gold/HPA/HPA-Research/2. Fallopian tube proteome/Figures")
#dir()

#Load dataset
input_anno <- read.csv("220503 Annotation of Cilia TMA For R.csv", sep=",", header = TRUE) 

cleaned_input <- input_anno %>% 
  select(-FT_Ext_Intensity, -FT_Ext_Structure, -FT_Ext_Quantity, -Intensity, -Structure, -Quantity) %>%
  # rename(Cervix = Cervix_Intensity, 
  #        Endometrium_Proliferative = Endometrium_Prol_Intensity, 
  #        Endometrium_Secretory = Endometrium_Secr_Intensity,
  #        Caudate = Caudate_Intensity, 
  #        Choroid_Plexus = Choroid_Plexus_Intensity, 
  #        Epididymis = Epididymis_Intensity, 
  #        Fallopian_Tube = FT_Intensity,
  #        Nasopharynx = Nasopharynx_Intensity, 
  #        Bronchus = Bronchus_Intensity) %>%
  gather(Factors, Values, -Gene) %>%  
  mutate(Factors = recode(Factors, 
                          "Endometrium_Prol_Intensity" = "EndometriumProl_Intensity",
                          "Endometrium_Prol_Structure" = "EndometriumProl_Structure", 
                          "Endometrium_Prol_Quantity" = "EndometriumProl_Quantity",
                          "Endometrium_Secr_Intensity" = "EndometriumSecr_Intensity",
                          "Endometrium_Secr_Structure" = "EndometriumSecr_Structure",
                          "Endometrium_Secr_Quantity" = "EndometriumSecr_Quantity",
                          "Choroid_Plexus_Intensity"  = "ChoroidPlexus_Intensity"  ,
                          "Choroid_Plexus_Structure" = "ChoroidPlexus_Structure" ,
                          "Choroid_Plexus_Quantity" = "ChoroidPlexus_Quantity"  )) %>%
  mutate(Factors = str_replace(Factors, "_", ".")) %>%
  separate(Factors, c("Tissue", "Element")) %>% 
  as.data.frame() %>% 
  group_by(Gene, Tissue) %>% spread(Element, Values) %>% 
  ungroup() 


### intensity staining QC
cleaned_input %>% 
  group_by(Tissue) %>% 
  count(Intensity) %>% 
  ggplot(aes(x= Tissue, y= n, fill = Intensity)) + 
  geom_bar(stat = "identity", position="stack") + 
  theme_classic() + coord_flip()
  
