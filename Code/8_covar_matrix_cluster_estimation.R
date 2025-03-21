library(MASS)

set.seed(123)

# Parameters
G <- 10                # Number of clusters
cluster_size <- 10     # Observations per cluster (try changing G and clus_size to higher numbers)
n <- G * cluster_size  # Total sample size
k <- 2                 # Number of regressors (excluding intercept)

# Generate clusters
cluster <- rep(1:G, each = cluster_size)

# Generate design matrix X and beta coefficients
X <- cbind(1, matrix(rnorm(n * k), n, k))
beta <- c(1, 2, -1)

# Generate cluster-correlated errors
rho <- 0.5  # within-cluster correlation
# the within-cluster variance-covariance matrix
Sigma_g <- matrix(rho, cluster_size, cluster_size) + diag(1 - rho, cluster_size)

# Create block-diagonal covariance matrix for all clusters
Sigma_e <- kronecker(diag(G), Sigma_g)

# Generate correlated errors
e <- mvrnorm(n = 1, mu = rep(0, n), Sigma = Sigma_e)

# Generate Y
Y <- X %*% beta + e  

# OLS estimation (using matrices)
XtX_inv <- solve(t(X) %*% X)
beta_hat <- XtX_inv %*% t(X) %*% Y
residuals <- Y - X %*% beta_hat

# Incorrect (classical) variance-covariance matrix (homoskedastic assumption)
sigma_sq_hat <- as.numeric(t(residuals) %*% residuals / (n - k - 1))
Var_classic <- sigma_sq_hat * XtX_inv

# Correct cluster-robust covariance matrix
sum_term <- matrix(0, ncol = k + 1, nrow = k + 1)

for (g in 1:G) {
  # Select rows belonging to cluster g
  idx <- which(cluster == g)
  X_g <- X[idx, ]
  e_g <- residuals[idx]
  
  # Sum (X_g'e_g)(X_g'e_g)'
  sum_term <- sum_term + (t(X_g) %*% e_g) %*% t(t(X_g) %*% e_g)
}

Var_cluster_robust <- XtX_inv %*% sum_term %*% XtX_inv

# Output both covariance matrices
cat("Classical covariance matrix (assuming homoskedasticity):\n")
print(Var_classic)

cat("\nCluster-robust covariance matrix (corrected for clustering):\n")
print(Var_cluster_robust)

# Compare standard errors
cat("\nStandard errors comparison:\n")
comparison <- data.frame(
  Classic_SE = sqrt(diag(Var_classic)),
  Cluster_Robust_SE = sqrt(diag(Var_cluster_robust))
)

print(comparison)

# With small numbers of clusters or small n, clustered SE estimators can even become smaller due to finite-sample bias.
# If the simulated design matrix (X) happens to create clusters that are internally homogeneous (little variation within clusters) or balanced, the cluster-robust SE could become surprisingly small.
