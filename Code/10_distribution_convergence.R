# Load libraries
library(MASS)  # For mvrnorm
library(rgl)   # For 3D plot

# Define fixed vector u in R^3
u <- c(1, 1, 1)

# Distribution parameters
mu <- c(0, 0, 0)
Sigma <- matrix(c(1, 0.4, 0.3,
                  0.4, 1, 0.5,
                  0.3, 0.5, 1), nrow = 3)

# Sample sizes to visualize
n_values <- c(100, 1000, 10000)

#true CDF
true_cdf <- pmvnorm(upper = u, mean = mu, sigma = Sigma)[1]


for (n in n_values) {
  set.seed(123)
  Z_n <- mvrnorm(n, mu = mu, Sigma = Sigma)
  inside <- (Z_n[, 1] <= u[1]) & (Z_n[, 2] <= u[2]) & (Z_n[, 3] <= u[3])
  F_hat <- mean(inside)
  
  # 3D plot
  open3d()
  plot3d(Z_n, col = ifelse(inside, "blue", "gray"), size = 3,
         xlab = "Z1", ylab = "Z2", zlab = "Z3", main = paste("n =", n))
  
  # Draw bounding box from min to u
  mins <- apply(Z_n, 2, min)
  cube_edges <- expand.grid(x = c(mins[1], u[1]),
                            y = c(mins[2], u[2]),
                            z = c(mins[3], u[3]))
  cube <- cube_edges[c(1,2,4,3,1), ]
  lines3d(cube$x, cube$y, cube$z, col = "red", lwd = 2)
  
  title3d(
    main = paste("n =", n, "| Empirical CDF =", round(F_hat, 4)),
    line = 2
  )
  readline(prompt = "Press [Enter] to view next n...")
}




         