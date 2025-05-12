# Load packages
library(quantreg)
install.packages("ald")
library(ald)
library(ggplot2)
library(dplyr)
install.packages("rmutil")   # if not yet installed
library(rmutil)
install.packages("cbq")  # Install if you haven't
library(cbq)             # Load the package

# Set seed and parameters
set.seed(123)
n <- 1000
X <- cbind(1, runif(n, 0, 10))
beta <- c(2, 0.5)
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)

# Store results
results <- data.frame()

# Loop over tau values
for (tau in taus) {
  # Simulate error from ALD (Asymetric laplace distribution) with quantile tau centered at 0
  e <- rald(n, mu = 0, sigma = 1, p = tau)
  
  # Generate Y
  Y <- X %*% beta + e
  
  # Fit quantile regression
  fit <- rq(Y ~ X[,2], tau = tau)
  residuals_hat <- resid(fit)
  
  # Store quantile of residuals
  q_resid <- quantile(residuals_hat, probs = tau)
  
  # Collect data for plotting
  temp <- data.frame(
    residual = residuals_hat,
    tau = paste0("τ = ", tau),
    q_at_tau = as.numeric(q_resid)
  )
  results <- rbind(results, temp)
}

# Create summary data for vertical lines in each facet
vline_data <- results %>%
  group_by(tau) %>%
  summarise(q_at_tau = unique(q_at_tau), .groups = "drop")

# Plot residual histograms and overlay τ-th quantile lines
ggplot(results, aes(x = residual)) +
  geom_histogram(bins = 60, fill = "skyblue", alpha = 0.7) +
  geom_vline(data = vline_data, aes(xintercept = q_at_tau), 
             color = "red", linetype = "dashed", linewidth = 1) +
  facet_wrap(~ tau, scales = "free") +
  labs(
    title = "Residuals from Quantile Regression for Various τ",
    subtitle = "Red dashed line = τ-th quantile of residuals (should be ≈ 0)",
    x = "Residual",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 14)


