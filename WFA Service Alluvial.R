library(alluvial)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sqldf)
library(ggpubr)
library(RColorBrewer)
library(ggthemes)
library(ggpmisc)

wfa <- read_csv("C:/Users/KnudseQ/OneDrive - BASF/Desktop/WFA Alluvial 2021.csv")
wfa
#data <- as.data.frame(Titanic)
#test <- data[,-5]
wfa <- wfa %>%
  select(Entity, Nature, Scope, Chargeable) %>% 
  group_by(Entity, Nature, Scope, Chargeable) %>% 
  summarise(count = n()) %>% 
  ungroup()


alluvial(wfa[,1:4], freq=wfa$count,
         col = ifelse(wfa$Chargeable == "Yes", "blue", "lightgrey"),
         border = ifelse(wfa$Nature == "Consulting", "black", "orange"),
         hide = wfa$count == 0,
         cex = .9
)
mtext("WFA 2021 Summary", 3, line=3, font=2)
mtext("Partner", 2, line=3, font=2)




















# create alluvial plot

alluvial(
  pkmn_wt_ht[, 1:3],  # exclude the count column from the plot
  freq = pkmn_wt_ht$count,  # column containing frequencies
  col = ifelse(
    test = pkmn_wt_ht$weight_bin == "extra_small",
    yes = "blue",  # colour where test is met
    no = "grey"
  ),  
  layer = pkmn_wt_ht$weight_bin %in% c("extra_large", "normal")  # order of layers
)









# get data

download.file(
  url = "https://raw.githubusercontent.com/mwdray/datasets/master/pokemon_go_captures.csv",
  destfile = "~/Desktop/pokemon_go_captures.csv"  # where to save it to
)

# read data

library(readr)
pokemon <- read_csv("C:/Users/KnudseQ/OneDrive - BASF/Desktop/pokemon.csv")
pkmn <- pokemon
# select, filter and summarise

pkmn_wt_ht <- pkmn %>%
  filter(species %in% c("pidgey", "rattata", "drowzee")) %>% 
  select(species, weight_bin, height_bin) %>% 
  group_by(species, weight_bin, height_bin) %>% 
  summarise(count = n()) %>% 
  ungroup()

# create alluvial plot

alluvial(
  pkmn_wt_ht[, 1:3],  # exclude the count column from the plot
  freq = pkmn_wt_ht$count,  # column containing frequencies
  col = ifelse(
    test = pkmn_wt_ht$weight_bin == "extra_small",
    yes = "blue",  # colour where test is met
    no = "grey"
  ),  
  layer = pkmn_wt_ht$weight_bin %in% c("extra_large", "normal")  # order of layers
)
