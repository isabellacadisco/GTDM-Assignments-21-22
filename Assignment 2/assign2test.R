library(Matrix)

# Read dataset
dat <- read.csv("data4.csv", header=TRUE)
View(dat)

# Remove first column (index)
dat <- dat[,-1]
View(dat)

#trasofrmo in matrice
M<-as.matrix(dat)
M

rankMatrix(M)[1]


#we can compute singular value decomposition
#i compute as many rows and volumn as the total rank of M
#compute SVD
svd_result<-svd(M,nu=5,nv=5)

U<-svd_result$u
Vt<-t(svd_result$v)

svd_result$d

D <- diag(svd_result$d)



#SVD RESULTS
U
D
Vt

rankMatrix(M)[1] #rank = 5


# last 3 values are relatively small so we neglect them
# the concepts we are dealing with are facilities and variety of films

#compute the best rank 2 approximation of M
Mbest<-U[,1:2]%*%D[1:2,1:2]%*%Vt[1:2,]
Mbest

rankMatrix(Mbest)[1] #check that its rank is 2

# Compute energy keeped from the first 2 singular values
energy_keeped<-sum(svd_result$d[1:2]^2)/sum(svd_result$d^2)
energy_keeped
# è 0.96 quindi it's ok!


#

Ubest <- U[,1:2]
Dbest <- D[1:2,1:2]
Vtbest <- Vt[1:2,]

Ubest 
Dbest
Vtbest 

#però non ha molto senso così


#non sono certa si faccia così -----------------------------------
# Compute SVD (nu=2, nv=2) 
svd_result2<-svd(M,nu=2,nv=2)
U2<-svd_result2$u 
V2<-(svd_result2$v) 
Vt2<-t(V2) 
D2<-diag(svd_result2$d)


U2
D2
Vt2
#----------------------------------------------------------


