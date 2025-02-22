# Simulated data
set.seed(123)
n <- 100
x <- rnorm(n, mean = 5, sd = 2)
y <- 3 + 2*x + rnorm(n)  # True beta_0 = 3, beta_1 = 2

# OLS estimation with intercept
model1 <- lm(y ~ x)
beta1_hat <- coef(model1)[2]  # Extract slope

# Demeaning the data
x_demeaned <- x - mean(x)
y_demeaned <- y - mean(y)

# OLS estimation with demeaned data and no intercept
model2 <- lm(y_demeaned ~ x_demeaned - 1)  # Removing intercept
beta1_demeaned <- coef(model2)[1]  # Extract slope

# Print results
cat("Slope coefficient from standard OLS:", beta1_hat, "\n")
cat("Slope coefficient from demeaned regression:", beta1_demeaned, "\n")
