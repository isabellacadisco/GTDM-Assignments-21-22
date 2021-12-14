install.packages('igraph')
library("igraph")
g= read.graph(file = "C:/Users/khate/Downloads/graph4.gml", format = "gml")
summary(g)

#printing the number of nodes and edges:
vcount(g)
ecount(g)
#printing nodes
V(g)

#visualizing the graph
nodes= layout.fruchterman.reingold(g)
plot(g, layout= nodes, vertex.label=NA, vertex.size=5)

V(g)$name

#computing the degree of nodes (the number of relations between persons)
d=degree(g)
summary(d)

 #plotting the histogram of number of relations:
hist(d, xlab = "Number of Relations", main = "Relations", breaks = 0:max(d))

#Top 10 most associated persons:
names(d)= V(g)$name
sort(d, decreasing=TRUE)[1:10]

#present subgraphs using edge betweenness 
x <- edge_betweenness(g, e= E(g), directed = FALSE, weights = NULL)
c <- cluster_edge_betweenness(g)
plot(c,g)

#dendrogram clustering
plot_dendrogram(c)

s <- induced_subgraph(g, 1:70)
vertex_attr(s)
plot.igraph(s)