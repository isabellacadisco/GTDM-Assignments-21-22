library(Matrix)
library(ggplot2)

### Upload Data and convert to Matrix format
data<-read.csv('/Users/andrewcosta/Desktop/Milan/Maths/data4_.csv')
rownames(data) = data$X
data=data[, -1]
M <- as.matrix(data)

#### Heat Map experimentation ####
df=data.frame(customer= LETTERS[1:15], 
              var= rep(0, 5*15), 
              val= rep(0, 5*15))

df$customer=as.character(df$customer)

colnames(M) <- paste0('col', 1:5)
rownames(M) <- paste0('row', 1:15)

x = 1
for (row in 1:nrow(data)){
  for (col in 1:ncol(data)){
    df[x, 3]=data[row, col]
    df[x, 2]=colnames(data)[col]
    x = x + 1
  }
}
df
colnames(M) <- c("Number of Projection Rooms", "Cleanliness", "Comfort of Chairs", "Variety of Prime Vision Movies", "Variety of 3D Movies")
ggplot(df, aes(var, customer, fill=val)) +
  geom_tile()

View(M)
ggplot(M, aes(X, y))
heatmap(M)

### Compute rank
rankMatrix(M)[1]

### compute SVD
svd_result<-svd(M,nu=6,nv=6) # set to 6 and 6 as rank=6

### Compute U, V.T, D
U <- svd_result$u
Vt <- t(svd_result$v)
D <- diag(svd_result$d) # last 3 values are relatively small so we neglect them

### calculate best rank 3 matrix of M
Mbest <- U[,1:3]%*%D[1:3,1:3]%*%Vt[1:3,]
Mbest

#check rank
rankMatrix(Mbest)[1]

# enter new review
nr <- c(16, 4, 9, 2, 6, 0)

#compute score for new review
score_nr <- nr%*%svd_result$v[,1:3]
score_nr

# the new review score relative to others
score_rev <- nr%*%svd_result$v[,1:3]%*%Vt[1:3,]
score_rev # cleanliness and variety of 3D movies rank poorly relative to others, so perhaps a freak experience

#let's look for the similarity of the other users with respect to 'concept' space:
Scores_users <- M%*%svd_result$v[,1:3]
Scores_users 


#compute cosine similarity
similarity<-c()

library(pracma) #needed for dot product
for (i in c(1:dim(Scores_users)[1])){
  similarity[i] <- dot(score_nr,Scores_users[i,])/(sqrt(sum(score_nr^2)*sum((Scores_users[i,])^2)))
}

similarity

# new review is closest to 13th and 15th review
