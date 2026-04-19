# Load library
library(ggplot2)

set.seed(123)

# ------------------------------------------------------------
# Goal:
# Show convergence in distribution:
# sqrt(n)(Xbar_n - mu)  ->d  N(0, sigma^2)
#
# We simulate from a non-normal population so we can see
# that the limit is normal even when the underlying data are not.
# ------------------------------------------------------------

# Population distribution: exponential
# Mean = 1, Variance = 1
mu <- 1
sigma <- 1

# Sample sizes
n_values <- c(2, 5, 10, 30, 100)

# Number of Monte Carlo replications
R <- 5000

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

for (n in n_values) {
  
  # ----------------------------------------------------------
  # Generate R samples of size n from Exp(1)
  # Each row is one sample
  # ----------------------------------------------------------
  X <- matrix(rexp(R * n, rate = 1), nrow = R, ncol = n)
  
  # Sample means
  Xbar <- rowMeans(X)
  
  # ----------------------------------------------------------
  # CLT object:
  # sqrt(n)(Xbar_n - mu)
  # ----------------------------------------------------------
  Zn <- sqrt(n) * (Xbar - mu)
  
  # Histogram of simulated distribution
  hist(Zn,
       probability = TRUE,
       breaks = 40,
       col = "lightgray",
       border = "white",
       main = paste("n =", n),
       xlab = expression(sqrt(n) * (bar(X)[n] - mu)),
       xlim = c(-4, 4),
       ylim = c(0, 0.45))
  
  # Overlay the limiting density N(0, sigma^2)
  curve(dnorm(x, mean = 0, sd = sigma),
        col = "red",
        lwd = 2,
        add = TRUE)
  
  # Add a legend
  legend("topright",
         legend = c("Simulated distribution", "Limiting N(0,1)"),
         col = c("lightgray", "red"),
         lwd = c(10, 2),
         bty = "n",
         cex = 0.8)
}

