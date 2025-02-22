# Variance decomposition demonstration
set.seed(123)
n <- 1000
X1 <- rnorm(n)
X2 <- rnorm(n)
Y <- 2 * X1 + 3 * X2 + rnorm(n)

Var_Y <- var(Y)
Var_Y_given_X1 <- var(Y - predict(lm(Y ~ X1)))
Var_Y_given_X1_X2 <- var(Y - predict(lm(Y ~ X1 + X2)))

cat("Var[Y]:", Var_Y, "\n")
cat("Var[Y - E[Y|X1]]:", Var_Y_given_X1, "\n")
cat("Var[Y - E[Y|X1, X2]]:", Var_Y_given_X1_X2, "\n")
