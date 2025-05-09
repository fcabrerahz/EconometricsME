# Load libraries
library(quantreg)

# Simulate heteroskedastic data
set.seed(123)
n <- 1000
x <- runif(n, 0, 5)
sigma <- 0.5 + x  # heteroskedastic errors
eps <- rnorm(n, 0, sigma)
y <- 1 + 0.5 * x + eps

# Quantile regression at tau = 0.5
tau <- 0.5
fit <- rq(y ~ x, tau = tau)
beta_hat <- coef(fit)
X <- model.matrix(fit)
residuals_hat <- resid(fit)

# Compute psi values
psi <- tau - as.numeric(residuals_hat < 0)

# Define a function to compute Omega_hat
compute_Omega <- function(h, X, psi) {
  keep <- abs(residuals_hat) < h
  weighted_grads <- lapply(which(keep), function(i) {
    tcrossprod(X[i, ]) * psi[i]^2
  })
  Omega_hat <- Reduce("+", weighted_grads) / h
  return(Omega_hat)
}

# Loop over bandwidths
h_vals <- seq(0.1, 2, length.out = 20)
slope_var <- sapply(h_vals, function(h) compute_Omega(h, X, psi)[2, 2])

# Plot
plot(h_vals, slope_var, type = "b", pch = 19, col = "orange",
     main = expression("Sensitivity of " * hat(Omega)[tau] * " to Bandwidth " * h),
     xlab = "Bandwidth h",
     ylab = expression(hat(Omega)[tau] * "[2,2] (slope variance component)"))
grid()
