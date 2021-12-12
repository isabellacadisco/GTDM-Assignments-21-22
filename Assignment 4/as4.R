rm(list = ls(all.names = TRUE))
library(igraph)
x<-read.graph("~/Downloads/graph4.gml", format=c("gml"))
gsize(x)
gorder(x)
plot(x) #plots the graph
plot(x, layout=layout_in_circle)
#detect communities using betweenness
y <- cluster_edge_betweenness(x)

#modularity
modularity(y)

#number of clusters according to max modularity
length(y)

#check that modularity is maximised if we choose 5 clusters
y$modularity

#membership to the clusters/communities
y$membership

#there is a cluster composed only by obs 39,25
#fixes the graph nodes layout using a specific algorithm
z = layout_with_fr(x)
#plot the communities with shaded regions around
plot(y,x,layout=z)

# plot communities without shaded regions
plot(x, vertex.color=membership(y), layout=z)

#plot dendrogram
plot_dendrogram(y)

#plot hierarchical clustering
y1<-as.hclust(y)
plot(y1)
# draw blue borders around clusters
clusters.list = rect.hclust(y1, k = 5, border="pink")
#2.2
x1<-induced_subgraph(x, 1:70)
#explore the names of vertices
vertex_attr(x1)
plot.igraph(x1)
#compute the adjacency matrix
adjmat1<-as_adj(x1)
adjmat1
#find the nodes hub through degree
d<- degree(x1)
d
table(d)
time_bins(x, middle = TRUE)
