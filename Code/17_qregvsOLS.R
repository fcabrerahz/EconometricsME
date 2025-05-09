# Load required package
library(quantreg)

# Set seed
set.seed(123)

# Simulate asymmetric data (e.g. skewed with outliers)
n <- 100
x <- runif(n, 0, 10)
y <- 2 + 0.5 * x + rt(n, df = 2)  # heavy-tailed residuals (t-distribution)

# Introduce asymmetry (more positive outliers)
y[x > 8] <- y[x > 8] + 10

# Fit OLS (targets the mean)
ols_fit <- lm(y ~ x)

# Fit quantile regression for τ = 0.5 and τ = 0.2
qr_05 <- rq(y ~ x, tau = 0.5)
qr_02 <- rq(y ~ x, tau = 0.2)

# Plot data
plot(x, y, pch = 20, col = "gray", main = "Mean vs Quantile Regression")
abline(ols_fit, col = "blue", lwd = 2)
abline(qr_05, col = "red", lwd = 2, lty = 2)
abline(qr_02, col = "darkgreen", lwd = 2, lty = 3)

# Add legend
legend("topleft",
       legend = c("OLS (mean)", "QR τ = 0.5 (median)", "QR τ = 0.2"),
       col = c("blue", "red", "darkgreen"),
       lwd = 2, lty = c(1, 2, 3))

###NOW ADDING Variance diffrences (see slide 23-28):

# Load quantreg
library(quantreg)

# Simulate heteroskedastic data
set.seed(123)
n <- 1000
x <- runif(n, 0, 5)
sigma <- 0.5 + x  # heteroskedasticity: variance increases with x
eps <- rnorm(n, mean = 0, sd = sigma)
y <- 1 + 0.5 * x + eps  # true model

# Estimate quantile regression
tau <- 0.5
fit <- rq(y ~ x, tau = tau)

# Classical (homoskedastic) SE
summary(fit, se = "nid")

# Robust (heteroskedasticity-consistent) SE
summary(fit, se = "ker")