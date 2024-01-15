#Differentiation 
#y=x^2e^x+7x+10

f <- expression((x^2)*(exp(x))+7*x+10)
D(f,"x")

Deriv(f,"x")

D(D(f,"x"),"x")

D(D(D(f,"x"),"x"),"x")

h <- expression((x^2+5*x-1)/(sqrt(x^2+2)))

D(h,"x")

#Integration
inte <- function(x){
  1/((x+1)*sqrt(x))
}

myval <- integrate(inte, lower = 0, upper = Inf)
myval
myval$value

library(cubature)
f <- function(x){
  (3/4)*(2*x[1]+x[2]+4*x[3])
}
adaptIntegrate(f,lowerLimit = c(0,0,0),upperLimit = c(1,1/2,1/4))
