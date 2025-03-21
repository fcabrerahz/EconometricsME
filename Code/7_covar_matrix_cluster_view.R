# Load necessary library
library(Matrix)

# Set seed for reproducibility
set.seed(123)

# Define the number of clusters and observations per cluster
G <- 3  # Number of clusters
n_g <- c(3, 4, 3)  # Observations per cluster

# Define covariance matrices for each cluster
Sigma_1 <- matrix(c(1, 0.5, 0.3, 
                    0.5, 1, 0.2, 
                    0.3, 0.2, 1), 3, 3)

Sigma_2 <- matrix(c(1, 0.4, 0.3, 0.2,
                    0.4, 1, 0.5, 0.3,
                    0.3, 0.5, 1, 0.4,
                    0.2, 0.3, 0.4, 1), 4, 4)

Sigma_3 <- matrix(c(1, 0.6, 0.4,
                    0.6, 1, 0.3,
                    0.4, 0.3, 1), 3, 3)

# Create a block diagonal matrix
block_diag_Sigma <- bdiag(Sigma_1, Sigma_2, Sigma_3)

# Convert to a standard matrix for visualization
block_diag_Sigma <- as.matrix(block_diag_Sigma)

# Print the block diagonal covariance matrix
print(block_diag_Sigma)

# Visualizing the block diagonal structure
image(block_diag_Sigma, main = "Block Diagonal Variance-Covariance Matrix", col = gray.colors(20))
