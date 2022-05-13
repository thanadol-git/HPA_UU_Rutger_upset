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

setwd("//argos.rudbeck.uu.se/MyGroups$/Gold/HPA/HPA-Research/2. Fallopian tube proteome/Figures")
dir()

#Load dataset
vd <- read.delim("220503 Annotation of Cilia TMA For R.csv", sep=",", header = TRUE)
vd <- vd[,-c(23, 24, 25, 32, 33, 34)] #Removing anything non-intensity deep annotation combined with Gene
vd <- data.table(vd)

#Turning important column names into something less wordy
vd = rename(vd, Cervix = Cervix_Intensity, Endometrium_Proliferative = Endometrium_Prol_Intensity, Endometrium_Secretory = Endometrium_Secr_Intensity,
            Caudate = Caudate_Intensity, Choroid_Plexus = Choroid_Plexus_Intensity, Epididymis = Epididymis_Intensity, Fallopian_Tube = FT_Intensity,
            Nasopharynx = Nasopharynx_Intensity, Bronchus = Bronchus_Intensity)

#Create a list of column names containing the column names of the columns that should be plotted
vd_sub = colnames(vd)[c(2,5,8,11,14,17,20,23,26)]

#####Plot a Upset Plot#####
library('ComplexUpset')

Plot_Upset <- upset(
  vd, vd_sub, 
  name = "Upset with Upsetting data", 
  width_ratio = 0.1,
  set_sizes = (
    upset_set_size(
      geom = geom_bar(
        aes(fill=FT_Structure),
        width = 0.8
        ),
    position="left"
    )
      ),
guides="over"
)

#### Generate a stacked barplot that I want inside the Upsetplot? ####
library(ggplot2)

##### Remodel the dataframe to make plotting of the tissue bar plot possible #####
Cervix        <- vd[,c(2,3)] #Selecting columns with annotation information.
Cervix        <- Cervix[!(Cervix == "0")] #Dropping the ones that have too weak staining for the upset plot, to make the data the same for both.
Cervix$Cervix <- as.character(Cervix$Cervix) #Make sure it is possible to add text to the column
Cervix$Cervix <- "Cervix" #Use tissue to be able to identify which location came from which tissue.
Cervix        <- rename(Cervix, Tissue = Cervix, Structure = Cervix_Structure) #Change the column names to shared names to make merging possible.
  
Endo_Prol                           <- vd[,c(5,6)]
Endo_Prol                           <- Endo_Prol[!(Endometrium_Proliferative == "0")]
Endo_Prol$Endometrium_Proliferative <- as.character(Endo_Prol$Endometrium_Proliferative)
Endo_Prol$Endometrium_Proliferative <- "Endometrium Proliferative"
Endo_Prol                           <- rename(Endo_Prol, Tissue = Endometrium_Proliferative, Structure = Endometrium_Prol_Structure)

Endo_Secr                           <- vd[,c(8,9)]
Endo_Secr                           <- Endo_Secr[!(Endometrium_Secretory == "0")]
Endo_Secr$Endometrium_Secretory     <- as.character(Endo_Secr$Endometrium_Secretory)
Endo_Secr$Endometrium_Secretory     <- "Endometrium Secretory"
Endo_Secr                           <- rename(Endo_Secr, Tissue = Endometrium_Secretory, Structure = Endometrium_Secr_Structure)

Caudate                             <- vd[,c(11,12)]
Caudate                             <- Caudate[!(Caudate == "0")]
Caudate$Caudate                     <- as.character(Caudate$Caudate)
Caudate$Caudate                     <- "Caudate"
Caudate                             <- rename(Caudate, Tissue = Caudate, Structure = Caudate_Structure)

Choroid                             <- vd[,c(14:15)]
Choroid                             <- Choroid[!(Choroid_Plexus == "0")]
Choroid$Choroid_Plexus              <- as.character(Choroid$Choroid_Plexus)
Choroid$Choroid_Plexus              <- "Choroid Plexus"
Choroid                             <- rename(Choroid, Tissue = Choroid_Plexus, Structure = Choroid_Plexus_Structure)

Epididymis                          <- vd[,c(17,18)]
Epididymis                          <- Epididymis[!(Epididymis == "0")]
Epididymis$Epididymis               <- as.character(Epididymis$Epididymis)
Epididymis$Epididymis               <- "Epididymis"
Epididymis                          <- rename(Epididymis, Tissue = Epididymis, Structure = Epididymis_Structure)

Fallopian                           <- vd[,c(20,21)]
Fallopian                           <- Fallopian[!(Fallopian_Tube == "0")]
Fallopian$Fallopian_Tube            <- as.character(Fallopian$Fallopian_Tube)
Fallopian$Fallopian_Tube            <- "Fallopian Tube"
Fallopian                           <- rename(Fallopian, Tissue = Fallopian_Tube, Structure = FT_Structure)

Naso                                <- vd[,c(23,24)]
Naso                                <- Naso[!(Nasopharynx == "0")]
Naso$Nasopharynx                    <- as.character(Naso$Nasopharynx)
Naso$Nasopharynx                    <- "Nasopharynx"
Naso                                <- rename(Naso, Tissue = Nasopharynx, Structure = Nasopharynx_Structure)

Bronchus                            <- vd[,c(26,27)]
Bronchus                            <- Bronchus[!(Bronchus == "0")]
Bronchus$Bronchus                   <- as.character(Bronchus$Bronchus)
Bronchus$Bronchus                   <- "Bronchus"
Bronchus                            <- rename(Bronchus, Tissue = Bronchus, Structure = Bronchus_Structure)

Bar_data <- rbind(Cervix, Endo_Prol, Endo_Secr, Caudate, Choroid, Epididymis, Fallopian, Naso, Bronchus) #put all the data in one dataset
Bar_data <- setDT(Bar_data)[,list(Count=.N),names(Bar_data)] #Merge duplicates and generate a column showing the counts for those.

Bar_size <- plyr::ddply(Bar_data, c("Tissue"), summarise, Value= sum(Count)) #Get the total size of the bar for each tissue

Test_data <- Bar_data %>% #Add the total value in a separate column. Note that if none match that 999 will be the applied value which is easy to sort out.
  mutate(Value = if_else(Tissue == "Cervix", 76, 
                         if_else(Tissue == "Bronchus", 76,
                                 if_else(Tissue == "Caudate", 62,
                                         if_else(Tissue == "Choroid Plexus", 15,
                                                 if_else(Tissue == "Endometrium Proliferative", 71,
                                                         if_else(Tissue == "Endometrium Secretory", 46,
                                                                 if_else(Tissue == "Epididymis", 63,
                                                                         if_else(Tissue == "Fallopian Tube", 80,
                                                                                 if_else(Tissue == "Nasopharynx", 66, 999)))))))))) 
##### Make the barplot #####
plot_bar <- ggplot(Test_data, 
       aes(fill=Structure, y=Count, x=Tissue)) +
  aes(x = reorder(Tissue, Value)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#f8766d", "#c49a00", "#53b400", "#00c094", "#00b6eb", "#a58aff", "#fb61d7")) +
  coord_flip()

##### Trying to make an upsetplot with bar plot beside it #####
library(multipanelfigure)


figure2 <- multi_panel_figure(columns = 3, rows = 3, panel_label_type = "upper-roman")
figure2 %<>%
  fill_panel(Plot_Upset, column = 1:2, row = 1:3) %<>%
  fill_panel(plot_bar, column = 3, row = 3) 
figure2

library(patchwork)
(Plot_Upset | plot_bar) + plot_layout(guides='collect')

plot_bar + Plot_Upset
