#Systems of linear Equations 
A <- matrix(c(2,3,-1,4,-5,5,-2,2,3,6,-1,4,-3,2,1,3,1,-2,-5,4,4,2,1,-6,7),
            nrow = 5,byrow = T)
A
b <- c(10,5,-3,12,0)
Mysol <- solve(A)%*%b
Mysol

4*Mysol[1]+2*Mysol[2]+Mysol[3]-6*Mysol[4]+7*Mysol[5]

k1 <- c(-1,4,-3,2,1)
1
k1%*%Mysol


#Linear Programming 
library(lpSolve)
#help(lp)
#lp (direction = "min", objective.in, const.mat, 
#    const.dir, const.rhs)

obj.in <- c(3,2,5,-1,2,4)
const.mat <- matrix(c(2,3,-1,2,2,1,4,1,2,1,-3,2,3,-2,3,4,1,2),nrow = 3, byrow = T)
const.dir <- c("<=",">=","=")
const.rhd <- c(10,8,5)

sol <- lp("max",obj.in,const.mat,const.dir,const.rhd)
sol <- lp("max",const.mat =  const.mat,
          const.dir =  const.dir,
          objective.in =  obj.in,
          const.rhs = const.rhd)
sol$solution
sol
