# Load libraries
library(MASS)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sandwich)
library(knitr)

# Settings
set.seed(123)
sample_sizes <- c(50, 100, 500, 5000)
n_sim <- 1000
alpha <- 0.05

run_simulation <- function(homoskedastic = TRUE) {
  results <- data.frame()
  
  for (n in sample_sizes) {
    t_hc0_covered <- 0
    t_hc2_covered <- 0
    w1_covered <- 0
    w2_covered <- 0
    
    for (i in 1:n_sim) {
      # Regressors
      X1 <- rnorm(n)
      X2 <- rnorm(n)
      X <- cbind(1, X1, X2)
      beta <- c(1, 2, -1)
      
      # Errors
      if (homoskedastic) {
        e <- rnorm(n)
      } else {
        eta <- rexp(n) - 1
        sigma <- 1 + 0.5 * abs(X1)
        e <- eta * sigma
      }
      
      Y <- X %*% beta + e
      model <- lm(Y ~ X1 + X2)
      b <- coef(model)
      
      # Robust SEs
      V_HC0 <- vcovHC(model, type = "HC0")
      V_HC2 <- vcovHC(model, type = "HC2")
      
      # t-test coverage using HC0
      se_b1_hc0 <- sqrt(V_HC0[2, 2])
      t_crit <- qt(1 - alpha / 2, df = n - 3)
      ci_low_hc0 <- b[2] - t_crit * se_b1_hc0
      ci_high_hc0 <- b[2] + t_crit * se_b1_hc0
      if (2 >= ci_low_hc0 && 2 <= ci_high_hc0) {
        t_hc0_covered <- t_hc0_covered + 1
      }
      
      # t-test coverage using HC2
      se_b1_hc2 <- sqrt(V_HC2[2, 2])
      ci_low_hc2 <- b[2] - t_crit * se_b1_hc2
      ci_high_hc2 <- b[2] + t_crit * se_b1_hc2
      if (2 >= ci_low_hc2 && 2 <= ci_high_hc2) {
        t_hc2_covered <- t_hc2_covered + 1
      }
      
      # Wald test 1: beta1 + beta2 = 1
      R1 <- matrix(c(0, 1, 1), nrow = 1)
      r1 <- 1
      V_classical <- vcov(model)
      theta1 <- R1 %*% b
      var1 <- R1 %*% V_classical %*% t(R1)
      W1 <- (theta1 - r1)^2 / var1
      if (pchisq(W1, df = 1) > alpha) {
        w1_covered <- w1_covered + 1
      }
      
      # Wald test 2: beta1 + beta2 = 1, beta2 = -1
      R2 <- matrix(c(0, 1, 1,
                     0, 0, 1), nrow = 2, byrow = TRUE)
      r2 <- c(1, -1)
      theta2 <- R2 %*% b
      var2 <- R2 %*% V_classical %*% t(R2)
      diff2 <- matrix(theta2 - r2, ncol = 1)
      W2 <- t(diff2) %*% solve(var2) %*% diff2
      if (pchisq(W2, df = 2) > alpha) {
        w2_covered <- w2_covered + 1
      }
    }
    
    # Store results
    results <- rbind(results, data.frame(
      SampleSize = n,
      `t-test (HC0)` = t_hc0_covered / n_sim,
      `t-test (HC2)` = t_hc2_covered / n_sim,
      `Wald (1 restriction)` = w1_covered / n_sim,
      `Wald (2 restrictions)` = w2_covered / n_sim
    ))
  }
  
  return(results)
}

# Run for both scenarios
homoskedastic <- run_simulation(TRUE) %>% mutate(Scenario = "Homoskedastic Normal")
heteroskedastic <- run_simulation(FALSE) %>% mutate(Scenario = "Heteroskedastic Non-Normal")

# Combine results
coverage_table <- bind_rows(homoskedastic, heteroskedastic) %>%
  relocate(Scenario, .before = SampleSize)

# Print coverage table
print(kable(coverage_table, digits = 3, caption = "Coverage Rates for t-tests (HC0 & HC2) and Wald Tests"))



