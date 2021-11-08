#libraries needed
library(Matrix)
library(pracma)
library(pheatmap)

#DATASET 5
#import dataset and remove index column
dat<-read.csv('data4.csv')
dat <- dat[,-1]
#convert in matrix form
M<-as.matrix(dat)
M

# Plot heatmap of data matrix
pheatmap (M, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:15), display_numbers =TRUE , legend = TRUE, 
          col = colorRampPalette(c("darkolivegreen3", "yellow", 
                                   "darkorange", "darkorange1", "orangered","firebrick1","firebrick2", "firebrick3"))(50))


#calculate rank of the data matrix
rankMatrix(M)[1]

#SVD (with 5 singular values)
svd_result<-svd(M,nu=5,nv=5)

U<-svd_result$u
V <-( svd_result $v)
Vt <-t(V)
D<-diag(svd_result$d)


#verifica 
Mprove<-(U%*%D%*%Vt)
Mprove
M
rankMatrix(Mprove)[1]
pheatmap (Mprove, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:15), labels_col= c("m1", "m2", "m3", "m4", "m5"),
          display_numbers =TRUE , legend = TRUE, 
          col = colorRampPalette(c("darkolivegreen3", "yellow", 
                                   "darkorange", "darkorange1", "orangered","firebrick1","firebrick2", "firebrick3"))(50))

#-------------------



#relationship consumers-concepts
U

#relationship feature-concepts
Vt

#strength of the concepts
D

#strength of the concepts visualization
pheatmap (D, 
          cluster_rows = FALSE, cluster_cols = FALSE,
          display_numbers =TRUE , legend = TRUE)


# Compute energy keeped from the first 2 singular values
x <- svd_result$d[1:2]
energy_keeped <- sum(x^2) / sum( svd_result$d^2)
energy_keeped

#RETAIN 2 SINGULAR VALUES
svd_result2<-svd(M,nu=2, nv=2)

U2<-svd_result2$u
V2<-svd_result2$v
Vt2<-t(V2)
D2<-diag(svd_result2$d,nrow=2, ncol=2)

#relationship consumers-2 concepts
U2

#D, central matrix with 2 concepts, strength of the concepts
D2

#relationship aspects to improve-concepts
Vt2



#visualization consumers-concept
pheatmap (U2, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:15), labels_col = c("concept 1", "concept 2"), display_numbers =TRUE,legend = TRUE)
pheatmap (U2, cluster_rows = TRUE, cluster_cols = TRUE, 
          labels_row = c(1:15), labels_col = c("concept 1", "concept 2"), display_numbers =TRUE,legend = TRUE,
          treeheight_row =0,
          treeheight_col =0)


pheatmap (U2, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:15), labels_col = c("concept 1", "concept 2"), display_numbers =TRUE,legend = FALSE,
          col = colorRampPalette(c("white"))(50))


#relationship aspects to improve-concepts visualization
pheatmap (Vt2, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c("concept 1", "concept 2"), labels_col = c("m1", "m2", "m3", "m4", "m5"), display_numbers =TRUE , legend = TRUE)



#create M with the best rank approximation
Mbest<-(U2%*%D2%*%Vt2)

# Plot heatmap of best rank approximation of the dataset
pheatmap (Mbest, show_colnames =TRUE, cluster_rows = FALSE, cluster_cols = FALSE, 
          labels_row = c(1:15), labels_col = c("m1", "m2", "m3", "m4", "m5"), display_numbers =TRUE , legend = TRUE, 
          col = colorRampPalette(c("darkolivegreen3", "yellow", 
                                   "darkorange", "darkorange1", "orangered","firebrick1","firebrick2", "firebrick3"))(50))


#compute score of the customers in the concepts' space
S<-M%*%V2
S


similarity<-matrix(nrow=15, ncol=15)
for (i in c(1:15)){
  for (j in c(1:15)){
    similarity[i, j]<-dot(S[i,],S[j,])/(sqrt(sum(S[i,]^2)*sum((S[j,])^2)))
  }
}

#no clustering
pheatmap(S,display_numbers = TRUE, cluster_rows = FALSE, cluster_cols = FALSE)

#clustering
pheatmap(S,display_numbers = TRUE, cluster_rows = TRUE, cluster_cols = TRUE)

#no clustering
pheatmap(similarity,display_numbers = TRUE, cluster_rows = FALSE, cluster_cols = FALSE,
         labels_row = c(1:15), labels_col = c(1:15))

#clustering
pheatmap(similarity,display_numbers = TRUE, cluster_rows = TRUE, cluster_cols = TRUE,
         labels_row = c(1:15), labels_col = c(1:15), treeheight_row =0,
         treeheight_col =0)



