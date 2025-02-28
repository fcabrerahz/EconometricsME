set.seed(123654)

# Generate many values of X
X <- runif(10000, -2, 2)  # X values drawn from U[-2,2]
g_X <- X + 1  # Define a function g(X) that is linear in X

# Linear case: E[Y | X] = 2X
Y_linear <- 2 * X + rnorm(10000, 0, 0.5)  # Generate Y values
E_Y_given_X_linear <- tapply(Y_linear, X, mean)  # Conditional expectation
E_gX_Y_linear <- tapply(g_X * Y_linear, X, mean)  # E[g(X)Y | X]
gX_E_Y_linear <- g_X * E_Y_given_X_linear  # g(X) * E[Y | X]

# Nonlinear case: E[Y | X] = X^2
Y_nonlinear <- X^2 + rnorm(10000, 0, 0.5)  # Generate Y values
E_Y_given_X_nonlinear <- tapply(Y_nonlinear, X, mean)
E_gX_Y_nonlinear <- tapply(g_X * Y_nonlinear, X, mean)
gX_E_Y_nonlinear <- g_X * E_Y_given_X_nonlinear

# Compare the results
cat("LINEAR CASE:\n")
print(cor(E_gX_Y_linear, gX_E_Y_linear))  # Should be ~1 (almost perfect)

cat("NONLINEAR CASE:\n")
print(cor(E_gX_Y_nonlinear, gX_E_Y_nonlinear))  # Should be < 1

plot(E_gX_Y_linear, gX_E_Y_linear, 
     main = "Scatter Plot of E[g(X)Y | X] vs. g(X)E[Y | X]", 
     xlab = "E[g(X)Y | X]", 
     ylab = "g(X)E[Y | X]", 
     col = "blue", pch = 19)

plot(E_gX_Y_nonlinear, gX_E_Y_nonlinear, 
     main = "Scatter Plot of E[g(X)Y | X] vs. g(X)E[Y | X]", 
     xlab = "E[g(X)Y | X]", 
     ylab = "g(X)E[Y | X]", 
     col = "blue", pch = 19)

