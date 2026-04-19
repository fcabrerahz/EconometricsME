# Load libraries
library(MASS)     # For mvrnorm
library(rgl)      # For 3D plot
library(mvtnorm)  # For true multivariate normal CDF

# ------------------------------------------------------------
# Define fixed vector u in R^3
# u defines the region Z ≤ u. It is NOT a probability.
# It is the point where we evaluate the CDF:
# F(u) = P(Z1 ≤ u1, Z2 ≤ u2, Z3 ≤ u3)

# ------------------------------------------------------------
u <- c(1, 1, 1)

# ------------------------------------------------------------
# Distribution parameters
# Simulated vectors are jointly normal (NOT independent across coordinates)
# Positive covariance across dimensions
# ------------------------------------------------------------
mu <- c(0, 0, 0)

Sigma <- matrix(c(1, 0.4, 0.3,
                  0.4, 1, 0.5,
                  0.3, 0.5, 1), nrow = 3)

# ------------------------------------------------------------
# Sample sizes to visualize convergence
# As n increases, we get more draws → better approximation
# Note we are fixing the distribution and drawing random vectors.
# ------------------------------------------------------------
n_values <- c(100, 1000, 10000)

set.seed(123)

# ------------------------------------------------------------
# True CDF
# This is the actual population probability:
# F(u) = P(Z ≤ u)
# ------------------------------------------------------------
true_cdf <- pmvnorm(upper = u, mean = mu, sigma = Sigma)[1]

for (n in n_values) {
  
  # ----------------------------------------------------------
  # Generate Sample
  # This gives an n x 3 matrix: each row is one draw Zi
  # ----------------------------------------------------------
  Z_n <- mvrnorm(n, mu = mu, Sigma = Sigma)
  
  # ----------------------------------------------------------
  # Indicator function: 1{Zi ≤ u}
  # TRUE if the draw lies inside the region defined by u
  # ----------------------------------------------------------
  inside <- (Z_n[,1] <= u[1]) & 
    (Z_n[,2] <= u[2]) & 
    (Z_n[,3] <= u[3])
  
  # ----------------------------------------------------------
  # Empirical CDF:
  # Fraction of sample points satisfying Zi ≤ u
  # NOT the number of blue points, but the SHARE
  # ----------------------------------------------------------
  F_hat <- mean(inside)
  
  open3d()
  
  # ----------------------------------------------------------
  # Each point = one draw Zi
  # Blue = Zi ≤ u → contributes to empirical CDF
  # Gray = outside region
  # ----------------------------------------------------------
  plot3d(Z_n,
         col = ifelse(inside, "blue", "gray"),
         size = 4,
         xlab = "Z1", ylab = "Z2", zlab = "Z3")
  
  # ----------------------------------------------------------
  # KEY VISUAL: REGION Z ≤ u
  # The red transparent box represents all points satisfying Zi ≤ u
  # This is the region whose probability is F(u)
  # ----------------------------------------------------------
  
  bounds <- apply(Z_n, 2, min)
  
  shade3d(
    cube3d(center = (bounds + u)/2,
           scale  = (u - bounds)),
    color = "red",
    alpha = 0.15
  )
  
  # ----------------------------------------------------------
  # Cutoff planes: Z1 = u1, Z2 = u2, Z3 = u3
  # Everything "below" these planes is counted in the CDF
  # ----------------------------------------------------------
  planes3d(1, 0, 0, -u[1], col = "red", alpha = 0.3)
  planes3d(0, 1, 0, -u[2], col = "red", alpha = 0.3)
  planes3d(0, 0, 1, -u[3], col = "red", alpha = 0.3)
  
  # ----------------------------------------------------------
  # Display empirical vs true CDF
  # As n increases:
  # - more points overall
  # - more blue points
  # BUT what matters is:
  # → the FRACTION stabilizes
  # → F_hat → F(u)
  # ----------------------------------------------------------
  title3d(
    main = paste0(
      "n = ", n,
      " | Empirical CDF = ", round(F_hat,4),
      " | True CDF = ", round(true_cdf,4)
    ),
    line = 2
  )
  
  readline(prompt = "Press [Enter]...")
}

# ------------------------------------------------------------
# Interpretation:
#
# Each blue dot is one draw of Z that satisfies Z ≤ u.
# The empirical CDF is the fraction of such draws.
#
# Asymptotically:
# Taking more draws from a fixed multivariate distribution,
# the empirical fraction below u approaches the true probability F(u).
#
# This is convergence in probability:
# F_hat_n(u) →p F(u)
## ------------------------------------------------------------    







#################################################################
# In 2D for simplicity



# ------------------------------------------------------------
# Convergence in Probability (Consistency) – 2D Visualization
#
# Goal:
# Show that the empirical CDF at a fixed point u converges:
#
#   F_hat_n(u) ->p F(u)
#
# where:
#   F_hat_n(u) = (1/n) sum 1{Zi ≤ u}
# ------------------------------------------------------------

library(MASS)      # For multivariate normal simulation
library(mvtnorm)   # For true CDF

set.seed(123)

# ------------------------------------------------------------
# Fixed point u in R^2
# u defines the region Z ≤ u
# ------------------------------------------------------------
u <- c(1, 1)

# ------------------------------------------------------------
# Distribution parameters
# Jointly normal with correlation
# ------------------------------------------------------------
mu <- c(0, 0)

Sigma <- matrix(c(1, 0.6,
                  0.6, 1), nrow = 2)

# ------------------------------------------------------------
# Sample sizes
# As n increases → better approximation
# ------------------------------------------------------------
n_values <- c(100, 1000, 10000)

# ------------------------------------------------------------
# True CDF
# F(u) = P(Z1 ≤ u1, Z2 ≤ u2)
# ------------------------------------------------------------
true_cdf <- pmvnorm(upper = u, mean = mu, sigma = Sigma)[1]

for (n in n_values) {
  
  # ----------------------------------------------------------
  # Generate sample: n draws of Z = (Z1, Z2)
  # Each row is one realization Zi
  # ----------------------------------------------------------
  Z_n <- mvrnorm(n, mu = mu, Sigma = Sigma)
  
  # ----------------------------------------------------------
  # Indicator: 1{Zi ≤ u}
  # TRUE if the point lies inside the region
  # ----------------------------------------------------------
  inside <- (Z_n[,1] <= u[1]) & (Z_n[,2] <= u[2])
  
  # ----------------------------------------------------------
  # Empirical CDF:
  # Fraction of points inside the region
  # ----------------------------------------------------------
  F_hat <- mean(inside)
  
  # ----------------------------------------------------------
  # Plot
  # Each point = one draw Zi
  # Blue = contributes to empirical CDF
  # Gray = outside region
  # ----------------------------------------------------------
  plot(Z_n,
       col = ifelse(inside, "blue", "gray70"),
       pch = 19,
       xlab = "Z1", ylab = "Z2",
       main = paste0(
         "n = ", n,
         " | Empirical CDF = ", round(F_hat,4),
         " | True CDF = ", round(true_cdf,4)
       ))
  
  # ----------------------------------------------------------
  # Draw region Z ≤ u (the CDF region)
  # This rectangle shows the probability being computed
  # ----------------------------------------------------------
  rect(xleft = min(Z_n[,1]), ybottom = min(Z_n[,2]),
       xright = u[1], ytop = u[2],
       border = "red",
       col = adjustcolor("red", alpha.f = 0.15),
       lwd = 2)
  
  # ----------------------------------------------------------
  # Draw cutoff lines Z1 = u1 and Z2 = u2
  # Everything below these lines is included
  # ----------------------------------------------------------
  abline(v = u[1], col = "red", lty = 2, lwd = 2)
  abline(h = u[2], col = "red", lty = 2, lwd = 2)
  
  # Mark the point u
  points(u[1], u[2], col = "darkred", pch = 19, cex = 1.5)
  text(u[1], u[2], labels = " u", pos = 4, col = "darkred")
  
  readline(prompt = "Press [Enter] to continue...")
}

# ------------------------------------------------------------
# Interpretation:
#
# Each blue point is one realization Zi that satisfies Zi ≤ u.
#
# The empirical CDF is:
#   F_hat_n(u) = fraction of blue points
#
# As n increases:
#   → the proportion of blue points stabilizes
#   → F_hat_n(u) gets closer to F(u)
#
# This is convergence in probability (consistency):
#
#   F_hat_n(u) ->p F(u)
#
# ------------------------------------------------------------
