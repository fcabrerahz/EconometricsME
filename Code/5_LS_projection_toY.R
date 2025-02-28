set.seed(123)
n <- 100
X <- cbind(1, runif(n, 1, 10))  # Add intercept column
beta <- c(2, 3)  # True coefficients
epsilon <- rnorm(n, 0, 1)  # Random error term
Y <- X %*% beta + epsilon  # Generate Y

# Compute the Projection Matrix P
XtX_inv <- solve(t(X) %*% X)  # (X'X)^-1
P <- X %*% XtX_inv %*% t(X)  # Projection Matrix

# Apply Projection Matrix to Y
Y_hat_proj <- P %*% Y

# Compute fitted Y from lm()
lm_model <- lm(Y ~ X[, 2])  
Y_hat_lm <- fitted(lm_model)  # Extract fitted values

# Compare: Plot Y_hat from Projection Matrix vs. lm()
df <- data.frame(Y_hat_proj = Y_hat_proj, Y_hat_lm = Y_hat_lm)

ggplot(df, aes(x = Y_hat_proj, y = Y_hat_lm)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Projection Matrix vs. lm() Fitted Values",
       x = "Y_hat from Projection Matrix",
       y = "Y_hat from lm()") +
  theme_minimal()
