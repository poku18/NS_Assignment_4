library(igraph)
nodes <- read.csv("vertex911.csv", header = TRUE) 
adj <- read.csv("adj911-1.csv", header=TRUE) 
head(adj) # to see how the data look like 

adj <- as.matrix(adj) # convert to R matrix object 

g <- graph_from_adjacency_matrix(adj, mode = "undirected")

# Q1
V(g)$color <- "lightblue" 
V(g)$color[1:19] <- "salmon" 
plot(g, vertex.label=NA,vertex.size=8, layout=layout_with_gem(g))

# Q2
eigen_cent <- eigen_centrality(g)
nodes[,5] <- eigen_cent$vector
colnames(nodes)[5] <-c("EigenValue")

print(nodes[,1][which(nodes$EigenValue > .6)])

# Q3
betweenness_cent <- betweenness(g) 
closeness_cent <- closeness(g)

nodes[,6] <- betweenness_cent
nodes[,7] <- closeness_cent
colnames(nodes)[6:7] <- c("Betweenness","Closeness")

print(nodes[,1][which(nodes$Betweenness > 300)])
print(nodes[,1][which(nodes$Closeness > 0.006)])

# Q4(a)
clique_size_counts(g)
table(sapply(cliques(g),length)) # Maximum size is 6

cliq<-cliques(g) [sapply(cliques(g), length) == 6]


# Q5 (a)
pilots <- data.frame(
  id = c(1:4), 
  name = c("Hani.Hanjour", "Mohamed.Atta", "Marwan.Al.Shehhi", "Ziad.Jarrah"))

g2 <- graph_from_adjacency_matrix(adj[1:19,1:19], mode = "undirected") 

i<-1
for(v in V(g2)$name){
  if(any(pilots$name==v)){
    print(v)
    V(g2)$shape[i]<-"square" 
  }
  else{
    V(g2)$shape[i]<-"circle" 
    
  }
  i<-i+1
}
V(g2)$color <-  nodes[1:19,3]
plot(g2, vertex.label=ifelse(V(g2)$shape=="square",V(g2)$name, NA),vertex.size=8, layout=layout_with_dh(g2))

# Q5 (b)
table(sapply(cliques(g2),length)) # Maximum size is 4

cliques(g2) [sapply(cliques(g2), length) == 4]
