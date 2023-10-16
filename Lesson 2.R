#MATRICES AND THIER OPERATIONS

#Additinal and Subtraction
set.seed(0001)
A <- matrix(round(rnorm(25,7,2),0),nrow=5,byrow=T)
A
B <- matrix(round(rnorm(25,3,5),0),nrow=5,byrow=T)
B
C <- matrix(c(2,3,5,6,5,4,5,6,8,4),nrow=5,byrow=T)
C
A+B
A+C

A-B
A-C

#Scalar multiple
#3A
j <- 3*A
j

#Linear combination
#2A+3B-4A
K <- 2*A+3*B-4*A
K

#Transpose
A_T <- t(A)
A_T

#Inverse
solve(B)

#Identity matrix
I <- diag(5)
I
A
X <- diag(diag(A))
X

#Determinant
det(A)

#Kronecker product
kronecker(A,B)
kronecker(C,B)

#Matrix Multiplication
AB <- A%*%B
AB
A%%B
A*B
A%in%B

#Extraction
# Element : A[i,j]
# Row : A[i,]
# Column : A[,j]
A[3,1]
A[3,]
A[,4]

#Element Replacement
B[5,1] <- 3
B
B[3,] <- c(rep(3,5))
B
B[,5] <- c(2,5,6,4,5)
B

ni <- NULL
myfun <- function(n){
  n <- as.integer(readline(prompt = "How many values: "))
  nr <- as.integer(readline(prompt = "How many rows of matrix: "))
  for (i in 1:n) {
    ni[i] <- as.integer(readline(prompt = "Enter an element : "))
  }
  Mymat <- matrix(ni,nrow = nr, byrow = T )
  return(Mymat)
}
myfun()

#Means of row and columns
A;B
rowMeans(A)
colMeans(B)

#Logarithms
log10(8.2)
log(8.2,10)
log(8.2)
log(8.2,exp(1))


#Exponential
exp(8.2)
exp(1)
