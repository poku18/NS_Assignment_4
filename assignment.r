library(igraph)
nodes <- read.csv("vertex911.csv", header = TRUE) 
adj <- read.csv("adj911.csv", header=TRUE) 
head(adj) # to see how the data look like 