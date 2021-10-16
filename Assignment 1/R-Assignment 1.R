#use the following 2 lines just with MacOS
options(rgl.useNULL=TRUE)
library(rgl)


library(matlib)

#PART 1
A <- matrix(c(2, 5, 8, 
              5, 2, 3,
              1,  3, 2), 3, 3, byrow=TRUE)


b <- c(335, 170, 115)


echelon(A, b, reduced=FALSE, verbose=TRUE, fractions=TRUE) # row-echelon form
echelon(A, b, reduced=TRUE, verbose=TRUE, fractions=TRUE) # reduced row-echelon form

#PART 2
A <- matrix(c(2, 5, 8, -1, 0, 0, 5, 2, 3, 0, -1, 0, 1, 3, 2, 0, 0, -1, 0, 0, 0, 
              1, 1, 1), 4, 6, byrow=TRUE)


b <- c(0, 0, 0, 620)


echelon(A, b, reduced=FALSE, verbose=TRUE, fractions=TRUE) # row-echelon form
echelon(A, b, reduced=TRUE, verbose=TRUE, fractions=TRUE) # reduced row-echelon form

