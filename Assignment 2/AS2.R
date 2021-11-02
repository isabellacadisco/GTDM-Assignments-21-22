library(Matrix) 
library(pheatmap)
library(pracma)
#Q1
# Read dataset
M <- read.csv("~/Downloads/data4.csv", header=TRUE)
# Remove first column (index)
M<-data.matrix(M[,-1])
# Plot heatmap of dataset
pheatmap(M, how_colnames = TRUE, show_colnames = TRUE, cluster_rows = FALSE,
         cluster_cols = FALSE, display_numbers = TRUE, legend = FALSE)

# Compute basic SVD
svd_result<-svd(M)
U<-svd_result$u 
V<-(svd_result$v) 
Vt<-t(V) 
D<-diag(svd_result$d)

# Plot heatmap of SVD
pheatmap(U, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)
pheatmap(D, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)
pheatmap(Vt, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)

# Compute energy keeped from the first 2 singular values
energy_keeped<-sum(svd_result$d[1:2]^2)/sum(svd_result$d^2)
energy_keeped
# Compute SVD (nu=2, nv=2) 
svd_result2<-svd(M,nu=2,nv=2)
U2<-svd_result2$u 
V2<-(svd_result2$v) 
Vt2<-t(V2) 
D<-diag(svd_result2$d)

# Plot heatmap of SVD matrices (nu=2, nv=2)
pheatmap(U2, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)
pheatmap(V2, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)
pheatmap(Vt2, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)

#Compute and plot the diagonal matrix with first 2 singular values
V2<-diag(svd_result$d[1:2],nrow=2,ncol=2)
pheatmap(V2, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE, fontsize = 20)
#Compute and plot the simplified matrix Mbest
Mbest<-U2%*%V2%*%Vt2
pheatmap(Mbest, how_colnames=TRUE, show_colnames=TRUE, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers=TRUE, legend=FALSE)
# Rank of M and Mbest
rankMatrix(M) 
rankMatrix(Mbest)

#Q2
#look for the similarity of the customers:
S<-M%*%svd_result$v[,1:2]
S
#compute cos(theta) between all
 #let's compute his scores with respect to the "concepts"
  scoreb<-b%*%svd_result$v[,1:2]
scoreb
#let's compute his scores in the "aspects"
score_va<-b%*%svd_result$v[,1:2]%*%Vt[1:2,]
score_va
#let's look for the similarity of the other customers:
Scores_customers<-M%*%svd_result$v[,1:2]
Scores_customers
similarity<-c()
for (i in c(1:dim(Scores_customers)[1])){
  similarity[i]<-dot(scoreb,Scores_customers[i,])/(sqrt(sum(scoreb^2)*sum((Scores_customers[i,])^2)))
}
similarity

pheatmap(similarity, how_colnames = TRUE, show_colnames = TRUE, cluster_rows = FALSE,cluster_cols = FALSE, display_numbers = TRUE, legend = FALSE)
pheatmap(similarity, how_colnames = TRUE, show_colnames = TRUE, cluster_rows = TRUE,cluster_cols = FALSE, display_numbers = TRUE, legend = FALSE)
#OR
plot(similarity)
