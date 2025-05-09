# Load required library
library(ggplot2)
library(gridExtra)

# Define function to compute LAD criterion
compute_LAD <- function(beta_vals, X, Y) {
  sapply(beta_vals, function(b) mean(abs(Y - X * b)))
}

# ---- Simulate LAD Criterion for n = 7 ----
set.seed(123)
n_small <- 7
X_small <- rep(1, n_small)  # Simple case: single regressor = 1
beta_true <- 0.2
Y_small <- X_small * beta_true + rnorm(n_small, mean = 0, sd = 0.1)

beta_vals_small <- seq(0.17, 0.22, length.out = 15)
lad_vals_small <- compute_LAD(beta_vals_small, X_small, Y_small)

df_small <- data.frame(beta = beta_vals_small, LAD = lad_vals_small)

p1 <- ggplot(df_small, aes(x = beta, y = LAD)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = beta_vals_small[which.min(lad_vals_small)], linetype = "dashed") +
  annotate("text", x = beta_vals_small[which.min(lad_vals_small)], y = min(lad_vals_small),
           label = expression(M[n](beta)), vjust = -1) +
  labs(title = expression("(b) LAD Criterion with n = 7"),
       x = expression(beta), y = NULL) +
  theme_minimal(base_size = 14)
p1
# ---- Simulate LAD Criterion for n = 50,742 ----
set.seed(456)
n_large <- 50742
X_large <- rep(1, n_large)
Y_large <- X_large * beta_true + rnorm(n_large, mean = 0, sd = 0.1)

beta_vals_large <- seq(0.16, 0.26, length.out = 200)
lad_vals_large <- compute_LAD(beta_vals_large, X_large, Y_large)

df_large <- data.frame(beta = beta_vals_large, LAD = lad_vals_large)

p2 <- ggplot(df_large, aes(x = beta, y = LAD)) +
  geom_line() +
  geom_vline(xintercept = beta_vals_large[which.min(lad_vals_large)], linetype = "dashed") +
  annotate("text", x = beta_vals_large[which.min(lad_vals_large)], y = min(lad_vals_large),
           label = expression(M[n](beta)), vjust = -1) +
  labs(title = expression("(c) LAD Criterion with n == 50*','*742"),
       x = expression(beta), y = NULL) +
  theme_minimal(base_size = 14)

# ---- Combine plots ----
grid.arrange(p1, p2, ncol = 2)
