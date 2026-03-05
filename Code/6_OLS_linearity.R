set.seed(123654)

# Simulate data
n <- 20000
X <- runif(n, -2, 2)
gX <- X + 1
Y <- X^2 + rnorm(n, 0, 0.5)   # nonlinear CEF on purpose

# Create bins to approximate conditional expectations
bins <- cut(X, breaks = 80)

# Conditional expectations
EY_X  <- tapply(Y, bins, mean)
EgXY_X <- tapply(gX * Y, bins, mean)
EgX_X <- tapply(gX, bins, mean)

# Compute g(X)E[Y|X]
gX_EY <- EgX_X * EY_X

# Compare numerically
cor(EgXY_X, gX_EY)

