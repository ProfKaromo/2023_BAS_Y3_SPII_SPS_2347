#Eigenvalues and Eigenvectors
A <- matrix(c(1,0,0.6,0,4,0,0.6,0,9),nrow = 3,byrow = T)
A

eigen_vals_vecs <- eigen(A)
eigenvals <- eigen_vals_vecs$values
eigenvals

eigenvecs <- eigen_vals_vecs$vectors
eigenvecs

sum(eigenvecs[3,]^2)

#properties
#Sum ofeigen values = trace of A

sum(diag(A)) == sum(round(eigenvals,4))

#prod of eigen values = the det of A

round(det(A),4) == round(prod(eigenvals),4)

#Dimensin reduction
desire <- 0.50
sumvar <- 0
for (i in 1:length(eigenvals)) {
  sumvar <- sumvar + eigenvals[i]
  if((sumvar/sum(eigenvals))>=desire)
    break
}
cat("The first",i," variables will explain",((sumvar/sum(eigenvals))*100),"% of the variablity.")

A <- cov(mtcars)
A
eigen_vals_vecs <- eigen(A)
eigenvalues <- eigen_vals_vecs$values
eigenvalues

eigenvecs <- eigen_vals_vecs$vectors
eigenvecs

sumlambda <- 0
desire <- 0.99999

for (i in 1:length(eigenvalues)) {
  sumlambda <- sumlambda + eigenvalues[i]
  #print(sumlambda)
  if (sumlambda/sum(eigenvalues) > desire) {
    cat("The first", i ,"principle components explains", (sumlambda/sum(eigenvalues)*100) , "% of the total variance.","\n")
    break()
  }
}

