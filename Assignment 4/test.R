#import
library(igraph)
#-----------------------------------

#read graph + plot 
g<-read_graph("graph4.gml",format=c("gml"))
plot(g)
#PER ORA MI TENGO QUESTO LAYOUT
coords = layout_with_lgl(g) #fixes the graph nodes layout using a specific algorithm
plot(g, layout=coords)
#------------------------------

# ------ ANALISI RETE --------------------------------------

# 1 DENSITà

# 2 DISTRIBUZIONE DEL GRADO

#è UNA RETE REALE VEDIAMO COME SI CREANO LEGAMI

# 3 ASSORTATIVITà DELLA RETE
#sull'età -> ALTA (0.7)
assortativity(g, V(g)$age, directed = FALSE )
#sul genere -> BASSA (-0.00081)
assortativity_nominal(g
                      ,as.numeric
                      (as.factor(V(g)$gender)),
                      directed = FALSE)

density(g)
help("density")

edge_density(g, loops = FALSE)

# 4 PRESENZA DI HUB
#GUARDARE CODICE PROGETTO SOMENI
#penso di poter usare il 90esimo percentile

#----------------------------------------------------

# ------ COMMUNITY DETECTION -------------------------------

#detect communities using betweenness
#list of removed edges ath each step
#betweenees computed on the complete graph, betweenees for all the edges
eb <- cluster_edge_betweenness(g)


#modularity
modularity(eb)
#maximal modularity in the vector

#number of clusters according to max modularity
length(eb) 
#optimal number of clusters for max mod

#check that modularity is maximised if we choose 5 clusters
eb$modularity

#membership to the clusters/communities
eb$membership

#plot the communities with shaded regions around
plot(eb,g,layout=coords)
# plot communities without shaded regions
plot(g, vertex.color=membership(eb), layout=coords)

#plot dendrogram
plot_dendrogram(eb)

#plot hierarchical clustering
ebd<-as.hclust(eb)
plot(ebd)
# draw blue borders around clusters
clusters.list = rect.hclust(ebd, k = 5, border="blue")
# 2 main groups + 1 isolated node
#modularity è uno strumento usabile per definire il best split
#ma se aiuta l'interpretazione possiamo anche usare un different split


#----------------------------------------------

#MISURE SULLE COMMUNITIES

# creare sottografo per ogni community


#s1 <- induced_subgraph(g, 1:20)
#vertex_attr(s1)
#plot.igraph(s1)

# assign communities to graph
g$community <- eb$membership

g$community

# see how many communities there are
unique(g$community)

g$community

#sottografi communities

s1 <- induced_subgraph(g, g$community == 1)
vertex_attr(s1)
plot.igraph(s1)

s2 <- induced_subgraph(g, g$community == 2)
vertex_attr(s2)
plot.igraph(s2)

s3 <- induced_subgraph(g, g$community == 3)
vertex_attr(s3)
plot.igraph(s3)

s4 <- induced_subgraph(g, g$community == 4)
vertex_attr(s4)
plot.igraph(s4)

s5 <- induced_subgraph(g, g$community == 5)
vertex_attr(s5)
plot.igraph(s5)

s6 <- induced_subgraph(g, g$community == 6)
vertex_attr(s6)
plot.igraph(s6)

# ASSORTATIVITà COMMUNITIES

# 1
#sull'età ->  (0.367)
assortativity(s1, V(s1)$age, directed = FALSE )
#sul genere -> BASSA (0.026)
assortativity_nominal(s1
                      ,as.numeric
                      (as.factor(V(s1)$gender)),
                      directed = FALSE)


# 2
#sull'età ->  0.0300273
assortativity(s2, V(s2)$age, directed = FALSE )
#sul genere -> 0.1574074
assortativity_nominal(s2
                      ,as.numeric
                      (as.factor(V(s2)$gender)),
                      directed = FALSE)

# 3
#sull'età -> -0.0830141
assortativity(s3, V(s3)$age, directed = FALSE )
#sul genere ->  -0.2857143
assortativity_nominal(s3
                      ,as.numeric
                      (as.factor(V(s3)$gender)),
                      directed = FALSE)
V(s3)$gender

# 4
#sull'età -> -1
assortativity(s4, V(s4)$age, directed = FALSE )
#sul genere -> BASSA (-0.00081)
assortativity_nominal(s4
                      ,as.numeric
                      (as.factor(V(s4)$gender)),
                      directed = FALSE)

V(s4)$gender #viene nan perchè ho solo due nodi

# 5
#sull'età -> ALTA (0.7)
assortativity(s5, V(s5)$age, directed = FALSE )
#sul genere -> BASSA (-0.00081)
assortativity_nominal(s5
                      ,as.numeric
                      (as.factor(V(s5)$gender)),
                      directed = FALSE)

# 6
#sull'età -> ALTA (0.7)
assortativity(s6, V(s6)$age, directed = FALSE )
#sul genere -> BASSA (-0.00081)
assortativity_nominal(s6
                      ,as.numeric
                      (as.factor(V(s6)$gender)),
                      directed = FALSE)





#prova
communities <- data.frame()

unique(g$community)

for (i in unique(g$community)) {
  # create subgraphs for each community
  subgraph <- induced_subgraph(g, v = which(g$community == i))
  # get size of each subgraph
  size <- igraph::gorder(subgraph)
  # get betweenness centrality
  btwn <-  igraph::betweenness(subgraph)
  communities <- communities %>% 
    dplyr::bind_rows(
      data.frame(community = i,
                 n_characters = size,
                 most_important = names(which(btwn == max(btwn)))
      )
    )
}

knitr::kable(communities %>% 
               dplyr::select(community, n_characters, most_important))

#-----

# give our nodes some properties, incl scaling them by degree and coloring them by community

V(g)$name

V(g)$size <- 10
V(g)$frame.color <- "black"
V(g)$color <- g$community
V(g)$label <- V(g)$name
V(g)$label.cex <- 10

# also color edges according to their starting node
edge.start <- ends(g, es = E(g), names = F)[,1]
E(g)$color <- V(g)$color[edge.start]
E(g)$arrow.mode <- 0

# only label central characters

v_labels <- which(V(g)$name %in% names(g))

for (i in 1:length(V(g))) {
  if (!(i %in% v_labels)) {
    V(g)$label[i] <- ""
  }
}

l2 <- layout_with_mds(g)
coords1 = layout_with_fr(g)

plot(g, rescale = T, layout = coords1, main = "chissà")
#-----







# SIMULATION


generalized_degroot <- function(input_network){
  
  output_network <- input_network
  n_agents <- dim(output_network[])[1]
  informed_index <- !is.na(V(output_network)$information)
  
  for(i in 1:n_agents){
    external_info <- sum(input_network[i, informed_index] * (V(input_network)$information[informed_index])/sum(input_network[i, informed_index]))
    V(output_network)$information[i] <- external_info
  }
  
  return(output_network)
}


# Initialize the undirected graph
net <- graph_from_literal(A-B-C-D,C-F-E-H-I-F) 
# Add self memory to nodes
mat <- as.matrix(net[]) + diag(dim(net[])[1]) 
# Re-initialize the network
net <- graph_from_adjacency_matrix(mat) 

plot(net, layout = layout.star(net))

# Distribute information
V(net)$information <- c(3,NA,NA,NA,NA,NA,1,NA)  

plot(net, vertex.label = V(net)$information, layout = layout.star(net))

timeline <- list()
timeline[[1]] <- net

for (i in 2:100) { 
  timeline[[i]] <- generalized_degroot(timeline[[i-1]])
}

t=0 
for (i in timeline) { 
  
  plot(i, vertex.label = V(i)$information, layout = layout.star(net))
  title(paste("t: ",t))
  t = t + 1
}

