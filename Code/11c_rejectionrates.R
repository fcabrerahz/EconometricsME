# ------------------------------------------------------------
# Finite-sample noise: homoskedastic vs heteroskedastic variance estimators
#
# Prompt:
# Show that both estimators are consistent, but the HC estimator
# is noisier in finite samples, so it often "needs more n".
# ------------------------------------------------------------

set.seed(123)

# ------------------------------------------------------------
# Simulation settings
# ------------------------------------------------------------
R <- 5000
n_grid <- c(50, 100, 250, 500, 1000)

beta0 <- 1
beta1 <- 2

# Storage
results <- data.frame()

for (n in n_grid) {
  
  se_homo <- numeric(R)      # usual homoskedastic SE for beta1
  se_hc0  <- numeric(R)      # HC0 robust SE for beta1
  bhat1   <- numeric(R)
  
  for (r in 1:R) {
    
    # --------------------------------------------------------
    # Data generating process
    # X ~ N(0,1)
    # Heteroskedastic error: Var(e|X)= (1 + 2X^2)^2
    # --------------------------------------------------------
    X <- rnorm(n)
    sigma_x <- 1 + 2 * X^2
    e <- rnorm(n, mean = 0, sd = sigma_x)
    
    Y <- beta0 + beta1 * X + e
    
    # --------------------------------------------------------
    # OLS
    # --------------------------------------------------------
    fit <- lm(Y ~ X)
    bhat1[r] <- coef(fit)[2]
    
    Xmat <- cbind(1, X)
    ehat <- resid(fit)
    
    XtX_inv <- solve(t(Xmat) %*% Xmat)
    
    # --------------------------------------------------------
    # Homoskedastic variance estimator
    # s^2 (X'X)^(-1)
    # --------------------------------------------------------
    s2 <- sum(ehat^2) / (n - ncol(Xmat))
    V_homo <- s2 * XtX_inv
    se_homo[r] <- sqrt(V_homo[2, 2])
    
    # --------------------------------------------------------
    # HC0 robust variance estimator
    # (X'X)^(-1) [sum Xi Xi' ehat_i^2] (X'X)^(-1)
    # --------------------------------------------------------
    meat <- matrix(0, nrow = 2, ncol = 2)
    for (i in 1:n) {
      Xi <- matrix(Xmat[i, ], ncol = 1)
      meat <- meat + (ehat[i]^2) * (Xi %*% t(Xi))
    }
    V_hc0 <- XtX_inv %*% meat %*% XtX_inv
    se_hc0[r] <- sqrt(V_hc0[2, 2])
  }
  
  # ----------------------------------------------------------
  # Save summaries
  # mean SE and dispersion of SE across Monte Carlo replications
  # ----------------------------------------------------------
  results <- rbind(results,
                   data.frame(
                     n = n,
                     estimator = "Homoskedastic",
                     mean_se = mean(se_homo),
                     sd_se   = sd(se_homo),
                     q10     = quantile(se_homo, 0.10),
                     q90     = quantile(se_homo, 0.90)
                   ),
                   data.frame(
                     n = n,
                     estimator = "HC0 Robust",
                     mean_se = mean(se_hc0),
                     sd_se   = sd(se_hc0),
                     q10     = quantile(se_hc0, 0.10),
                     q90     = quantile(se_hc0, 0.90)
                   ))
  
  # ----------------------------------------------------------
  # Histograms for each n
  # ----------------------------------------------------------
  par(mfrow = c(1, 2))
  
  hist(se_homo,
       breaks = 40,
       col = "gray85",
       border = "white",
       main = paste("Homoskedastic SE, n =", n),
       xlab = "Estimated SE of beta1")
  
  hist(se_hc0,
       breaks = 40,
       col = "lightblue",
       border = "white",
       main = paste("HC0 Robust SE, n =", n),
       xlab = "Estimated SE of beta1")
  
  readline(prompt = paste("n =", n, "- press [Enter] to continue..."))
}

# ------------------------------------------------------------
# Summary table
# sd_se measures finite-sample noisiness of the estimated SE
# ------------------------------------------------------------
print(results)

# ------------------------------------------------------------
# Plot summary: variability of the estimated SE across replications
# ------------------------------------------------------------
par(mfrow = c(1, 1))

plot(NULL,
     xlim = range(n_grid),
     ylim = range(results$sd_se),
     xlab = "Sample size n",
     ylab = "Monte Carlo SD of estimated SE",
     main = "Finite-sample variability of SE estimators")

lines(n_grid,
      results$sd_se[results$estimator == "Homoskedastic"],
      type = "b", pch = 19)

lines(n_grid,
      results$sd_se[results$estimator == "HC0 Robust"],
      type = "b", pch = 17, lty = 2)

legend("topright",
       legend = c("Homoskedastic", "HC0 Robust"),
       pch = c(19, 17),
       lty = c(1, 2),
       bty = "n")

