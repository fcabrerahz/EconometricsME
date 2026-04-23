# ------------------------------------------------------------
# Heteroskedasticity and t-tests:
# classical SE vs robust SE
#
# Prompt:
# Show that under heteroskedasticity:
# - classical t-test has wrong size
# - robust t-test is asymptotically valid
# ------------------------------------------------------------

set.seed(123)

# ------------------------------------------------------------
# Settings
# ------------------------------------------------------------
R <- 5000
n_grid <- c(50, 100, 250, 500, 1000)
alpha_test <- 0.05
crit <- qnorm(1 - alpha_test/2)   # asymptotic 5% two-sided critical value

# True parameters
beta0 <- 1
beta1 <- 0   # null is true

# Storage
results <- data.frame()

for (n in n_grid) {
  
  reject_classical <- numeric(R)
  reject_robust <- numeric(R)
  
  for (r in 1:R) {
    
    # --------------------------------------------------------
    # Data generating process
    # X ~ N(0,1)
    # Heteroskedastic error variance depends on X
    # --------------------------------------------------------
    X <- rnorm(n)
    sigma_x <- 1 + 2 * X^2
    e <- rnorm(n, mean = 0, sd = sigma_x)
    
    Y <- beta0 + beta1 * X + e
    
    # --------------------------------------------------------
    # OLS
    # --------------------------------------------------------
    fit <- lm(Y ~ X)
    b1hat <- coef(fit)[2]
    
    Xmat <- cbind(1, X)
    ehat <- resid(fit)
    k <- ncol(Xmat)
    
    XtX_inv <- solve(t(Xmat) %*% Xmat)
    
    # --------------------------------------------------------
    # Classical homoskedastic SE
    # --------------------------------------------------------
    s2 <- sum(ehat^2) / (n - k)
    V_classical <- s2 * XtX_inv
    se_classical <- sqrt(V_classical[2, 2])
    t_classical <- (b1hat - beta1) / se_classical
    
    # --------------------------------------------------------
    # Robust HC0 SE
    # --------------------------------------------------------
    meat <- matrix(0, nrow = k, ncol = k)
    for (i in 1:n) {
      Xi <- matrix(Xmat[i, ], ncol = 1)
      meat <- meat + ehat[i]^2 * (Xi %*% t(Xi))
    }
    V_hc0 <- XtX_inv %*% meat %*% XtX_inv
    se_hc0 <- sqrt(V_hc0[2, 2])
    t_robust <- (b1hat - beta1) / se_hc0
    
    # --------------------------------------------------------
    # Rejection indicators using asymptotic normal critical values
    # --------------------------------------------------------
    reject_classical[r] <- abs(t_classical) > crit
    reject_robust[r] <- abs(t_robust) > crit
  }
  
  results <- rbind(
    results,
    data.frame(
      n = n,
      test = "Classical SE",
      rejection_rate = mean(reject_classical)
    ),
    data.frame(
      n = n,
      test = "Robust HC0 SE",
      rejection_rate = mean(reject_robust)
    )
  )
}

print(results)

# ------------------------------------------------------------
# Plot rejection rates
# ------------------------------------------------------------
par(mfrow = c(1,1))

plot(NULL,
     xlim = range(n_grid),
     ylim = range(c(results$rejection_rate, 0.05)),
     xlab = "Sample size n",
     ylab = "Empirical rejection rate",
     main = "Size of t-tests under heteroskedasticity")

lines(n_grid,
      results$rejection_rate[results$test == "Classical SE"],
      type = "b", pch = 19)

lines(n_grid,
      results$rejection_rate[results$test == "Robust HC0 SE"],
      type = "b", pch = 17, lty = 2)

abline(h = 0.05, col = "red", lwd = 2)

legend("topright",
       legend = c("Classical SE", "Robust HC0 SE", "Nominal 5%"),
       pch = c(19, 17, NA),
       lty = c(1, 2, 1),
       col = c("black", "black", "red"),
       bty = "n")