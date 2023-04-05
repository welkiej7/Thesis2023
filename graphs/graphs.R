readr::read_csv("~/Desktop/Sampled.csv") -> tdata
library(igraph)
library(ggraph)
library(tidyverse)
library(ggplot2)
tdata[sample(1:nrow(tdata)),] -> tdata

ggplot(tdata, mapping = aes(x = 1:nrow(tdata), y = Homophily, size = 7, color = PS, shape = Type)) + geom_point() + ylim(c(-0.2,0.5)) + 
ggtitle(label = "Homophily Structure Across Random Sampled Networks") + xlab("Graph Number") + scale_color_brewer(palette = "Dark2" )

ggplot(tdata, mapping = aes(x = 1:nrow(tdata), y = Number_of_Components, size = 7, color = PS, shape = Type)) + geom_point() +  
ggtitle(label = "Number of Components Across Random Sampled Networks") + xlab("Graph Number") + scale_color_brewer(palette = "Dark2" )

ggplot(tdata, mapping = aes(x = 1:nrow(tdata), y = Mean_Ego_Size, size = 7, color = PS, shape = Type)) + geom_point() +  
ggtitle(label = "Mean Ego Size Across Random Sampled Networks") + xlab("Graph Number") + scale_color_brewer(palette = "Dark2" )

ggplot(tdata, mapping = aes(x = 1:nrow(tdata), y = Farthest_Nodes, size = 7, color = PS, shape = Type)) + geom_point() +  
ggtitle(label = "Farthest Nodes Across Random Sampled Networks") + xlab("Graph Number") + scale_color_brewer(palette = "Dark2" )


