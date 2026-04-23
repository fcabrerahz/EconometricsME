# ------------------------------------------------------------
# OLS under heavy-tailed double-Pareto errors
# Goal:
# Show that when alpha gets close to 2, the normal approximation
# for the OLS slope becomes poor because extreme errors dominate.
# ------------------------------------------------------------

set.seed(123)

# -----------------------------
# Simulation parameters
# -----------------------------
R <- 5000                 # number of Monte Carlo replications
n <- 200                  # sample size in each replication
beta0 <- 1
beta1 <- 2

# Different tail parameters
alpha_grid <- c(5, 3, 2.3, 2.1)

# -----------------------------
# Function to generate
# symmetric double-Pareto draws
# with density proportional to |e|^(-alpha-1), |e| >= 1
#
# If U ~ Uniform(0,1), then W = U^(-1/alpha) has Pareto(alpha)
# support on [1, infinity).
# Add a random sign for symmetry.
# -----------------------------
rdoublepareto <- function(n, alpha) {
  signs <- sample(c(-1, 1), size = n, replace = TRUE)
  mags  <- runif(n)^(-1 / alpha)
  signs * mags
}

# -----------------------------
# Storage
# -----------------------------
results <- vector("list", length(alpha_grid))
names(results) <- paste0("alpha=", alpha_grid)

# -----------------------------
# Monte Carlo loop
# -----------------------------
for (j in seq_along(alpha_grid)) {
  
  alpha <- alpha_grid[j]
  
  # Theoretical variance of e when alpha > 2
  var_e <- alpha / (alpha - 2)
  
  bhat <- numeric(R)
  zhat <- numeric(R)      # usual CLT-style standardized statistic
  bhat_raw <- numeric(R)
  
  for (r in 1:R) {
    
    # Regressor
    X <- rnorm(n, mean = 0, sd = 1)
    
    # Heavy-tailed error
    e <- rdoublepareto(n, alpha)
    
    # Outcome
    Y <- beta0 + beta1 * X + e
    
    # OLS with intercept
    fit <- lm(Y ~ X)
    b1hat <- coef(fit)[2]
    
    bhat_raw[r] <- b1hat
    bhat[r] <- sqrt(n) * (b1hat - beta1)
    
    # Standardize by asymptotic sd implied by finite variance formula
    # Since Var(X)=1 here, asymptotic variance is proportional to Var(e)
    zhat[r] <- sqrt(n) * (b1hat - beta1) / sqrt(var_e)
  }
  
  results[[j]] <- list(
    alpha = alpha,
    var_e = var_e,
    bhat = bhat,
    zhat = zhat,
    bhat_raw = bhat_raw
  )
}

# ------------------------------------------------------------
# Plot 1:
# Histogram of sqrt(n)(bhat - beta1)
# As alpha approaches 2, dispersion explodes and tails dominate
# ------------------------------------------------------------
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (j in seq_along(results)) {
  
  obj <- results[[j]]
  
  hist(obj$bhat,
       breaks = 60,
       freq = FALSE,
       col = "gray85",
       border = "white",
       main = paste0("alpha = ", obj$alpha,
                     "\nVar(e) = ", round(obj$var_e, 2)),
       xlab = expression(sqrt(n) * (hat(beta)[1] - beta[1])),
       xlim = c(-20, 20))
  
  # Overlay normal with matching asymptotic variance
  curve(dnorm(x, mean = 0, sd = sqrt(obj$var_e)),
        add = TRUE, col = "red", lwd = 2)
}

# ------------------------------------------------------------
# Plot 2:
# Standardized statistic:
# sqrt(n)(bhat - beta1) / sqrt(Var(e))
# If normal approximation were good, this would look close to N(0,1)
# ------------------------------------------------------------
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (j in seq_along(results)) {
  
  obj <- results[[j]]
  
  hist(obj$zhat,
       breaks = 60,
       freq = FALSE,
       col = "lightblue",
       border = "white",
       main = paste0("Standardized, alpha = ", obj$alpha),
       xlab = expression(frac(sqrt(n) * (hat(beta)[1] - beta[1]),
                              sqrt(Var(e)))),
       xlim = c(-8, 8))
  
  curve(dnorm(x, mean = 0, sd = 1),
        add = TRUE, col = "red", lwd = 2)
}

# ------------------------------------------------------------
# Optional numerical summaries
# Compare empirical moments and tail behavior
# ------------------------------------------------------------
summary_table <- data.frame(
  alpha = sapply(results, function(x) x$alpha),
  var_e = sapply(results, function(x) x$var_e),
  sd_sqrt_n_error = sapply(results, function(x) sd(x$bhat)),
  sd_standardized = sapply(results, function(x) sd(x$zhat)),
  p_large_standardized = sapply(results, function(x) mean(abs(x$zhat) > 2))
)

print(summary_table)
