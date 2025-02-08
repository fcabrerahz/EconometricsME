library(MASS)  # For mvrnorm
library(ggplot2)
library(reshape2)
library(lattice)  # For 3D plotting

install.packages("reshape2")  # Install the package
library(reshape2)  # Load it

# Set seed for reproducibility
set.seed(123)

# Simulate bivariate normal data
mu <- c(0, 0)  # Mean vector
Sigma <- matrix(c(1, 0.7, 0.7, 1), 2, 2)  # Covariance matrix
data <- mvrnorm(n = 5000, mu = mu, Sigma = Sigma)
X <- data[, 1]
Y <- data[, 2]

# Compute joint density
kde <- kde2d(X, Y, n = 100)  # 2D Kernel Density Estimation

grid_data <- melt(kde$z)
grid_data$X <- rep(kde$x, each = length(kde$y))
grid_data$Y <- rep(kde$y, length(kde$x))

# Joint Density Contour Plot
ggplot(grid_data, aes(x = X, y = Y, z = value)) +
  geom_contour_filled() +
  labs(title = "Joint Density of X and Y", x = "X", y = "Y") +
  theme_minimal()

# 3D Surface Plot
wireframe(value ~ X * Y, data = grid_data, drape = TRUE, colorkey = TRUE,
          screen = list(z = 30, x = -60), main = "3D Joint Density")

# Compute conditional densities for fixed X values
fixed_x_vals <- c(-1, 0, 1)  # Fixed values of X
plot(NULL, xlim = c(-3, 3), ylim = c(0, 0.6), type = "n", 
     xlab = "Y", ylab = "Conditional Density", 
     main = "Renormalized Conditional Density of Y | X")

for (x_fixed in fixed_x_vals) {
  mu_Y_given_X <- mu[2] + Sigma[2,1] / Sigma[1,1] * (x_fixed - mu[1])
  sigma_Y_given_X <- sqrt(Sigma[2,2] - Sigma[2,1]^2 / Sigma[1,1])
  y_vals <- seq(-3, 3, length.out = 100)
  cond_density <- dnorm(y_vals, mean = mu_Y_given_X, sd = sigma_Y_given_X)
  lines(y_vals, cond_density, col = ifelse(x_fixed == -1, "blue", 
                                           ifelse(x_fixed == 0, "red", "green")), lwd = 2)
  legend("topright", legend = paste("X =", fixed_x_vals), col = c("blue", "red", "green"), lwd = 2)
}
