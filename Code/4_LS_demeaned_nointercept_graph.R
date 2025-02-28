library(ggplot2)

# Simulate data
set.seed(123)
n <- 100
X <- runif(n, 1, 10)   # Random X values
beta <- 2
epsilon <- rnorm(n, 0, 1)   # Random error term
Y <- beta * X + epsilon  # Linear relationship

# Compute demeaned variables
X_demeaned <- X - mean(X)
Y_demeaned <- Y - mean(Y)

# Store in a data frame
df <- data.frame(X, Y, X_demeaned, Y_demeaned)

# Plot Original Regression
p1 <- ggplot(df, aes(x = X, y = Y)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Original Regression", x = "X", y = "Y") +
  theme_minimal()

# Plot Demeaned Regression
p2 <- ggplot(df, aes(x = X_demeaned, y = Y_demeaned)) +
  geom_point(color = "blue", alpha = 0.5) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(title = "Demeaned Regression (Centered at Origin)", x = "Demeaned X", y = "Demeaned Y") +
  theme_minimal()

# Display plots side by side
library(gridExtra)
grid.arrange(p1, p2, ncol = 2)
