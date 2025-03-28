###MULTIVARIATE STANDARD

library(MASS)  # For multivariate normal distribution functions

# Set seed for reproducibility
set.seed(123)

# Define dimension k
k <- 2  

# Mean vector (k-dimensional zero vector)
mu <- rep(0, k)

# Covariance matrix (k-dimensional identity matrix)
Sigma <- diag(k)

# Generate multivariate standard normal data
n <- 5000  # number of observations
Z <- mvrnorm(n, mu = mu, Sigma = Sigma)

# Plot simulated data
plot(Z, col="steelblue", pch=20, cex=0.5, 
     xlab="Z1", ylab="Z2", 
     main="Simulated Multivariate Standard Normal (k=2)")

# Add contour lines representing theoretical joint density
library(MASS)
kde <- kde2d(Z[,1], Z[,2], n=100)
contour(kde, add=TRUE, col="red", lwd=2)


#MULTIVARIATE NORMAL DISTRIBUTION

library(MASS)  # for multivariate normal distribution

# Set seed for reproducibility
set.seed(123)

# Define dimension (for visualization, let's choose k=2)
k <- 2  

# Define a non-zero mean vector
mu <- c(1, 3)

# Define a covariance matrix (non-identity, correlated structure)
Sigma <- matrix(c(4, 1.8,
                  1.8, 1), nrow=2, byrow=TRUE)

# Generate simulated data
n <- 5000
Z <- mvrnorm(n, mu = mu, Sigma = Sigma)

# Plot simulated data with density contours
plot(Z, col = "steelblue", pch = 20, cex = 0.5,
     xlab = "Z1", ylab = "Z2",
     main = "Simulated Multivariate Normal")

# Overlay contour lines of the theoretical density
library(MASS)
kde <- kde2d(Z[,1], Z[,2], n = 100)
contour(kde, add = TRUE, col = "red", lwd = 2)

# Add the mean point clearly marked
points(mu[1], mu[2], pch=19, col="darkred", cex=2)
text(mu[1], mu[2], "Mean", col="darkred", pos=4)


