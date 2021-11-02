library(Matrix)
library(pracma)
library(pheatmap)
dat<-read.csv('data4.csv')
dat
M<-as.matrix(dat)
M
M<-data.matrix(M[,-1])
M
rankMatrix(M)
svd_result<-svd(M)
U<-svd_result$u
Vt<-t(svd_result$v)
D<-diag(svd_result$d)
U
D
Vt
svd_result2<-svd(M,nu=2, nv=2)
U2<-svd_result2$u
Vt2<-t(svd_result2$v)
U2
Vt2
svd_result2
D2<-diag(svd_result2$d,nrow=2, ncol=2)
D2
Mbest<-(U2%*%D2%*%Vt2)
Mbest
S<-M%*%svd_result$v[,1:2]
S
similarity<-matrix(nrow=15, ncol=15)
for (i in c(1:15)){
  for (j in c(1:15)){
    similarity[i, j]<-dot(S[i,],S[j,])/(sqrt(sum(S[i,]^2)*sum((S[j,])^2)))
  }
}
similarity
pheatmap(M)
pheatmap(U,display_numbers = TRUE)
pheatmap(Vt,display_numbers = TRUE)
pheatmap(D,display_numbers = TRUE)

#no clustering
pheatmap(similarity,display_numbers = TRUE, cluster_rows = FALSE, cluster_cols = FALSE)
pheatmap(similarity,display_numbers = TRUE, cluster_rows = TRUE, cluster_cols = TRUE)





