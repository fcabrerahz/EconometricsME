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

