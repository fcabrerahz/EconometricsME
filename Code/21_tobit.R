# Load required libraries
library(AER)       # for tobit()
library(ggplot2)
library(dplyr)
library(gridExtra)

# Function to simulate Tobit data
simulate_tobit_data <- function(n = 1000, beta = 1, sigma = 1, shift = 0) {
  set.seed(123)
  X <- rnorm(n)
  Y_star <- shift + beta * X + rnorm(n, mean = 0, sd = sigma)
  Y <- pmax(Y_star, 0)  # Censor at zero
  censoring_rate <- mean(Y == 0)
  data <- data.frame(X = X, Y_star = Y_star, Y = Y)
  list(data = data, censoring = censoring_rate)
}

# Generate high and low censoring datasets
high <- simulate_tobit_data(shift = -1.5)
low  <- simulate_tobit_data(shift = 2.0)

# Plot histograms
p1 <- ggplot(high$data, aes(x = Y)) +
  geom_histogram(bins = 30, fill = "tomato", alpha = 0.7) +
  ggtitle(paste0("High Censoring (", round(high$censoring, 2), ")")) +
  xlab("Observed Y")

p2 <- ggplot(low$data, aes(x = Y)) +
  geom_histogram(bins = 30, fill = "seagreen", alpha = 0.7) +
  ggtitle(paste0("Low Censoring (", round(low$censoring, 2), ")")) +
  xlab("Observed Y")

# Display plots side by side
grid.arrange(p1, p2, ncol = 2)

# Fit Tobit models
model_high_tobit <- tobit(Y ~ X, left = 0, data = high$data)
model_low_tobit  <- tobit(Y ~ X, left = 0, data = low$data)

# Fit OLS models
model_high_ols <- lm(Y ~ X, data = high$data)
model_low_ols  <- lm(Y ~ X, data = low$data)

# Create comparison table
summary_table <- data.frame(
  Model = c(
    "Tobit (High Censoring)",
    "OLS (High Censoring)",
    "Tobit (Low Censoring)",
    "OLS (Low Censoring)"
  ),
  Censoring_Proportion = c(
    high$censoring,
    high$censoring,
    low$censoring,
    low$censoring
  ),
  Intercept = c(
    coef(model_high_tobit)[1],
    coef(model_high_ols)[1],
    coef(model_low_tobit)[1],
    coef(model_low_ols)[1]
  ),
  X_Coefficient = c(
    coef(model_high_tobit)[2],
    coef(model_high_ols)[2],
    coef(model_low_tobit)[2],
    coef(model_low_ols)[2]
  ),
  Sigma = c(
    model_high_tobit$scale,
    NA,
    model_low_tobit$scale,
    NA
  )
)

print(summary_table)

#sigma It tells you how spread out the latent (uncensored) outcome 
#You don’t observe Y* for Y less than zero
#The Tobit model infers σ as part of the maximum likelihood estimation.
