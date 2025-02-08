# Jensen's Inequality Demonstration
set.seed(123)
X_vals <- runif(1000, 1, 100)  # Generate random values between 1 and 10
E_log_X <- mean(log(X_vals))  # Expected value of log(X)
log_E_X <- log(mean(X_vals))  # Log of expected value of X

# Plot demonstration
data_jensen <- data.frame(X_vals, log_X = log(X_vals))
ggplot(data_jensen, aes(x = X_vals, y = log_X)) +
  geom_point(alpha = 0.4, color = "blue") +
  geom_hline(yintercept = E_log_X, linetype = "dashed", color = "red", linewidth = 1.2) +
  geom_hline(yintercept = log_E_X, linetype = "dashed", color = "violet", linewidth = 1.2) +
  labs(title = "Jensen's Inequality: E[log(X)] vs. log(E[X])", 
       x = "X", y = "log(X)") +
  annotate("text", x = 25, y = E_log_X + 0.1, label = "E[log(X)]", color = "red") +
  annotate("text", x = 5, y = log_E_X - 0.1, label = "log(E[X])", color = "violet") +
  theme_minimal()
