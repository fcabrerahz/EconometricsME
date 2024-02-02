
#Actividad 1

###################################################################################
#PREGUNTA 3
###inciso a.

#Clear the environment
rm(list = ls())

#Packages and library
install.packages("ggpubr") #to create kernel distributions (histograms but with lines)
install.packages("ggplot2") #for nice graphs!


library("ggpubr")
library("ggplot2")

help(rnorm)
help(runif)

#creamos variables aleatorias usando las funciones “rnorm” y “runif”
set.seed(123) #esto sirve para que siempre cree los mismos números aleatorios y sea reproducible el ejercicio.
x <- runif(100, min=0, max=20)
error <- rnorm(100, mean=0, sd=10)
y <-15+(7*x) + error 

### inciso b.
plot(x,y, col="blue")

#inciso c
#let’s make a regression using the random variables we created.
reg <- lm(y ~ x)
summary(reg)
anova(reg)

#find sse
sse <- sum((fitted(reg) - y)^2)
sse

#find ssr
ssr <- sum((fitted(reg) - mean(y))^2)
ssr

#find sst
sst <- ssr + sse
sst

###inciso g.
summary(fitted(reg))
summary(y)

###################################################################################
#PREGUNTA 5
#biased estimator
repet <- 1000
n <- 20
beta <- NULL

for (i in 1:repet){
  x <- rnorm(n)
  u <- rnorm(n,0,1)
  y=2+2*x+u
  beta[i] <- (10/(n-1))+lm(y~x)$coef[2] 
}
hist(beta, main="Biased n=20") 
abline(v = mean(beta), col="red", lwd=3, lty=2)
abline(v = 2, col="blue", lwd=3, lty=2)


#biased but consistent estimator
n <- 1000
for (i in 1:repet){
  x <- rnorm(n)
  u <- rnorm(n,0,1)
  y=2+2*x+u
  beta[i] <- (10/(n-1))+lm(y~x)$coef[2] 
}
hist(beta, main="Biased but consistent, n=1000") 
abline(v = mean(beta), col="red", lwd=3, lty=2)
abline(v = 2, col="blue", lwd=3, lty=2)

#e.g. la media muestral, si es calculada con muestras aleatorias, aun sesgadas por chance, son consistentes si n->infinito.


### Omitted Variable Bias: Biased and Inconsistent (en econometría, si el estimador B está sesgado, no es consistente ni aunque n->infinito)
#REMEMBER: x, y , and u are the true population parameters we artificially define, line 99, estimates these with OLS using different random 1000 samples.

#biased
n <- 20
beta <- NULL

set.seed(1234567)

for (i in 1:repet){
  x <- runif(n, min=0, max=30) #n i-values for x between 0 and 30
  u <- (rnorm(n,0,1)+.1*x) #correlate u to x, this biases and makes x inconsistent (the higher the correlation, the bigger the bias).
  y=2+2*x+u # we define y, so that beta is 2 by definition.
  beta[i] <- lm(y~x)$coef[2] #we collect all Betas 1 from our 1000 estimations in one vector.
}
hist(beta, main="Biased, n=20", xlim = c(1.95,2.2) ) 
abline(v = mean(beta), col="red", lwd=3, lty=2 )
abline(v = 2, col="blue", lwd=3, lty=2)


#biased and inconsistent.
n <- 1000
beta <- NULL

set.seed(1234567)

for (i in 1:repet){
  x <- runif(n, min=0, max=30) #n i-values for x between 0 and 30
  u <- (rnorm(n,0,1)+.1*x) #correlate u to x 
  y=2+2*x+u # we define y, so that beta is 2 by definition.
  beta[i] <- lm(y~x)$coef[2] #we collect all B1 from our 1000 estimations in one vector.
}
hist(beta, main="Biased and inconsistent, n=1000", xlim = c(1.95,2.2) ) 
abline(v = mean(beta), col="red", lwd=3, lty=2 )
abline(v = 2, col="blue", lwd=3, lty=2)

#inciso d.
#unbiased and consistent
n <- 1000
beta <- NULL

set.seed(1234567)

for (i in 1:repet){
  x <- runif(n, min=0, max=30) #n i-values for x between 0 and 30
  u <- rnorm(n,0,1) #DO NOT correlate u to x 
  y=2+2*x+u # we define y, so that beta is 2 by definition.
  beta[i] <- lm(y~x)$coef[2] #we collect all B1 from our 1000 estimations in one vector.
}
hist(beta, main="Unbiased and consistent, n=1000", xlim = c(1.95,2.15) ) 
abline(v = mean(beta), col="red", lwd=3, lty=2 )
abline(v = 2, col="blue", lwd=3, lty=2)


###################################################################################
#PREGUNTA 6
###inciso a.
#Clear the environment
rm(list = ls())

#creamos variables aleatorias usando las funciones “rnorm” y “runif”
set.seed(123) #esto sirve para que siempre cree los mismos números aleatorios
x <- runif(100, min=0, max=1)
error <- rnorm(100, mean=0, sd=5)
y <-15+(7*x) + error 
summary (lm(y ~ x))


#lets make a regression using the random variables we created.
regresion <- lm(y ~ x)
summary(regresion)

#scatterplot (gráfico) de nuestra regresión.
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE) +
  abline(lm(y ~ x, data = mtcars), col = "blue")

###inciso c.
#Otras x, y, error:
set.seed(123) #esto sirve para que siempre cree los mismos números aleatorios
x <- runif(100, min=0, max=1)
error <- rnorm(100, mean=0, sd=15)
y <-15+(7*x) + error

#scatterplot de nuestra segunda regresión.
plot(x, y, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title",
     pch = 19, frame = FALSE) +
  abline(lm(y ~ x), col = "blue")
summary (lm(y ~ x))

##inciso e. 
#no es peor, su R-sq es más pequeña, su bondad de ajuste es menor, pero ambas regresiones están cerca de beta_1=15, 
#el valor problacional. Esto muestra que la dispersión de los datos afectan la R2, la relación entre x-y  es menos fuerte, 
#pero la beta por construcción es consistente e insesgada.


