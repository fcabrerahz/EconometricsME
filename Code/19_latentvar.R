# Load packages
library(ggplot2)
library(dplyr)

# 1. Simulate data ----------------------------------------------
set.seed(123)
n <- 1000
X1 <- runif(n, -2, 2)
X2 <- runif(n, 0, 10)
beta <- c(0.5, 1.2)  # Coefficients for X1 and X2
intercept <- -1

# Linear index
XB <- intercept + beta[1] * X1 + beta[2] * X2

# Latent variable Y* = Xᵗβ + e, where e ~ N(0, 1)
e <- rnorm(n, mean = 0, sd = 1)
Y_star <- XB + e

# Observed outcome: Y = 1 if Y* > 0
Y <- as.integer(Y_star > 0)

# Combine into data frame
df <- data.frame(Y_star = Y_star, XB = XB, Y = factor(Y))

# 2. Plot latent variable vs linear index -----------------------
ggplot(df, aes(x = XB, y = Y_star, color = Y)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Latent Variable Model: Y* = Xᵗβ + e",
    subtitle = "Dashed line shows threshold at Y* = 0",
    x = "Linear Index (Xᵗβ)",
    y = "Latent Variable Y*",
    color = "Observed Y"
  ) +
  theme_minimal()
