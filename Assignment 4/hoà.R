#rm(list = ls(all.names = TRUE))
library(igraph)
x<-read.graph("graph4.gml", format=c("gml"))
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
# draw pink borders around clusters
clusters.list = rect.hclust(y1, k = 5, border="pink")
#2.2
x1<-induced_subgraph(x, 1:70)
#explore the names of vertices
vertex_attr(x1)
plot.igraph(x1)



#compute the adjacency matrix
adjmat1<-as_adj(x1)
adjmat1

#heatmap adj matrix
#libraries needed
library(Matrix)
library(pracma)
library(pheatmap)
library(RColorBrewer)

purplecols <- brewer.pal(6, "Purples")
pyg <- brewer.pal(9, "PiYG")
set3 <- brewer.pal(6, "Set3")


pheatmap (adjmat1, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:70), display_numbers =FALSE , legend = TRUE, 
          col = colorRampPalette(c("darkolivegreen3", "yellow", 
                                   "darkorange", "darkorange1", "orangered","firebrick1","firebrick2", "firebrick3"))(50))

pheatmap (adjmat1, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:70), display_numbers =FALSE , legend = FALSE, col=purplecols)

matrix_a = matrix(V(g)$age, nrow=70, ncol=1)
pheatmap (matrix_a, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:70), display_numbers =TRUE , legend = TRUE, 
          col = pyg)


pheatmap (y$membership, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:70), display_numbers =TRUE , legend = TRUE, 
          col = set3)



#provadata = data.frame(node=1:70, age = V(g)$age,
           #member = y$membership)
#provadata
#matrix_prova =  data.matrix(provadata)
#matrix_prova
#pheatmap (matrix_prova, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
         # labels_row = c(1:70), display_numbers =TRUE , legend = TRUE, 
          #col = pyg6)

#find the nodes hub through degree
d<- degree(x1)
d
table(d)

#Histogram of node degree 
V(x1)$label <- V(x1)$name
V(x1)$degree <- degree(x1)
hist(V(x1)$degree,
     col = 'green',
     main = 'Histogram of Node Degree',
     ylab = 'Frequency',
     xlab = 'Degree of Vertices')

hs <- hub_score(x1)$vector
set.seed(123)
plot(x1,
     vertex.size=hs*30,
     main = 'Hubs',
     vertex.color = rainbow(52),
     edge.arrow.size=0.1,
     layout = layout.kamada.kawai)

#2nd part
#Find the most closness
closeness.cent <- closeness(x1, mode="all")
closeness.cent
#node 55

#Find the mean time (x: the 1st one who got infected by the fake news)
set.seed(12345)
I <- matrix(rep(0,times = 70),ncol = 1)

I

#x number of starting node
I[55] <- 1

I

v= rep(0,times = 100)





for (i in 1:100){
  NI <- I
  
  NI
  
  t <-0
  
  sum(NI)
  
  while (sum(NI) < 70){
    
    Matrix1 <- as.matrix(adjmat1)
    
    Matrix1
    
    Num_Inf <- Matrix1 %*% NI
    P_inf <- Num_Inf*0.2 + NI
    P_inf[P_inf > 1] <- 1
    R_vector <- runif(70, min = 0, max = 1)
    NI <- P_inf > R_vector
    sum(NI)
    t <- t+1
    
  }
  v[i] = t
  
}

mean(v)

