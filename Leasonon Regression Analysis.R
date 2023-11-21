#regression Analysis
View(women)
#weight = b0 + b1*heigh + e
model1 <- lm(women$weight~women$height)
summary(model1)

#fitted model
#weight = -87.51667 + 3.45*heigh + e

w_hat <- -87.51667 + 3.45*women$height
w_hat1 <- -87.51667 + 3.45*70

women$weight
e <- women$weight - w_hat

mydata <- cbind(women$height,women$weight,w_hat,e)
mydata
colnames(mydata) <- c("Height","Weight","Fitted Weight","Error")
mydata

model1$coefficients
coef(model1)[1]
coef(model1)[2]

model1$residuals
e

model1$fitted.values
w_hat

anova(model1)

confint(model1)

#Multiple lenear regression
View(mtcars)
attach(mtcars)
model2 <- lm(disp~mpg+cyl+hp)
model2 <- lm(disp~.,data = mtcars)
summary(model2)
confint(model2)
coef(model2)[1:5]

#Using the matrix method
#Using women data
Y <- women$weight
X <- as.matrix(cbind(rep(1,length(women$height)),
                     women$height))
Y
X

xtx <- t(X)%*%X
xty <- t(X)%*%Y

betas <- solve(xtx)%*%xty
betas
coef(model1)
