library(readr)
library(ggthemes)
library(repr)
library(rworldmap)
library(dplyr)
library(ggplot2)
library(plotly)

library(usmap)
library(ggplot2)


cost <- read_csv("C:/Users/KnudseQ/Desktop/R Data/cost of living.csv")


plot_usmap(data = cost, values = "costIndex", color = "darkgreen") + 
  scale_fill_continuous(
    low = "cornsilk", high = "red", name = "Cost Index", label = scales::comma
  ) + theme(legend.position = "right")+ 
  labs(title = "Cost of Living by State", subtitle = "South of the Mason Dixon Line has cheaper cost of living.")+
  theme(legend.position = "right")


plot_usmap(data = cost, values = "costIndex", color = "darkgreen") + 
  scale_fill_gradientn(colours = c("darkgreen", "yellow", "orange", "orange red", "red", "firebrick", "darkred"),
                       breaks = c(0,90,110,130,150,170,190))+ theme(legend.position = "right")+ 
  labs(title = "Cost of Living by State",
       subtitle = "South of the Mason Dixon Line has cheaper cost of living.",fill="Cost of Living")+
  theme(legend.position = "right")

  





library(binr)
library(OneR)
cost$IndexBin <- bin(cost$costIndex, nbins = 3, labels = c("low", "medium", "high"), method = c("length", "content",
                                               "clusters"), na.omit = TRUE)
cost$CostGroup <- cut(cost$costIndex, 3)

library(classInt)
cost$group <- classIntervals(cost$costIndex, 3, style = 'equal')
