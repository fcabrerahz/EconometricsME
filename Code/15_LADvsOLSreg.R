# Load necessary library
library(quantreg)

# Set seed for reproducibility
set.seed(42)

# Generate predictor
n <- 100
x <- runif(n, 0, 10)

# True relationship
beta_0 <- 2
beta_1 <- 0.5
y_true <- beta_0 + beta_1 * x

# Add noise
noise <- rnorm(n, mean = 0, sd = 1)

# Add outliers to 10% of the data
outliers <- sample(c(0, 20), size = n, replace = TRUE, prob = c(0.9, 0.1))
y <- y_true + noise + outliers

# Create data frame
data <- data.frame(x = x, y = y)

# --- Scatterplot to visualize outliers ---
plot(data$x, data$y,
     pch = 20, col = "darkgray",
     main = "Simulated Data with Outliers",
     xlab = "x", ylab = "y")
abline(beta_0, beta_1, col = "green", lty = 3, lwd = 2)

# --- OLS regression ---
ols_fit <- lm(y ~ x, data = data)

# --- LAD (median) regression ---
lad_fit <- rq(y ~ x, data = data, tau = 0.5)

# --- Print estimates ---
cat("OLS Estimate:\n")
print(coef(ols_fit))

cat("\nLAD Estimate (Median Regression):\n")
print(coef(lad_fit))

# --- Plot comparison ---
plot(data$x, data$y, pch = 20, col = "gray",
     main = "OLS vs Median Regression (LAD)",
     xlab = "x", ylab = "y")
abline(ols_fit, col = "blue", lwd = 2)
abline(lad_fit, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("OLS", "LAD (Median)"), col = c("blue", "red"),
       lty = c(1, 2), lwd = 2)


###NOW WITH OUTLIERS FOR LARGE X ONLY AND USING OPTIMIZATION:

# Set seed and simulate data
set.seed(42)
n <- 100
x <- runif(n, 0, 10)
beta_0 <- 2
beta_1 <- 0.5
y_true <- beta_0 + beta_1 * x
noise <- rnorm(n, 0, 1)

# Modify: Add outliers only when x > 8
outliers <- ifelse(x > 8, sample(c(0, 20), size = n, replace = TRUE, prob = c(0.8, 0.2)), 0)

# Construct y
y <- y_true + noise + outliers
data <- data.frame(x = x, y = y)

# --- Visualize the data ---
plot(x, y, pch = 20, col = "gray", main = "Simulated Data with Localized Outliers")
abline(beta_0, beta_1, col = "green", lwd = 2, lty = 3)

# --- OLS estimate for comparison ---
ols_fit <- lm(y ~ x)
abline(ols_fit, col = "blue", lwd = 2)

# --- Define LAD loss function ---
LAD_objective <- function(par, x, y) {
  intercept <- par[1]
  slope <- par[2]
  y_hat <- intercept + slope * x
  sum(abs(y - y_hat))  # LAD: sum of absolute residuals
}

# --- Run optimization using optim() ---
initial_guess <- c(0, 0)
lad_fit <- optim(par = initial_guess,
                 fn = LAD_objective,
                 x = x, y = y,
                 method = "BFGS")

# --- Extract coefficients ---
lad_coef <- lad_fit$par
cat("Manual LAD Estimate (via optim):\n")
print(lad_coef)

# --- Plot LAD fit ---
abline(a = lad_coef[1], b = lad_coef[2], col = "red", lwd = 2, lty = 2)

# --- Add legend ---
legend("topleft",
       legend = c("True line", "OLS", "Manual LAD"),
       col = c("green", "blue", "red"),
       lty = c(3, 1, 2),
       lwd = 2)


