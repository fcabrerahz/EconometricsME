# Load library for plotting
library(ggplot2)

# Function g: continuous at c = 1
g <- function(x) log(x)

# Sample sizes to simulate increasing precision
n_values <- c(10, 50, 100, 500, 1000, 5000)
set.seed(123)

# Store results
results <- data.frame()

for (n in n_values) {
  # Generate 1000 samples from N(1, 1/n)
  Z_n <- rnorm(1000, mean = 1, sd = 1 / sqrt(n))
  g_Zn <- g(Z_n)
  
  results <- rbind(results, data.frame(
    n = as.factor(n),
    Z_n = Z_n,
    g_Zn = g_Zn
  ))
}

# Plot the transformed values g(Z_n)
ggplot(results, aes(x = g_Zn, fill = n)) +
  geom_density(alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  labs(
    title = "Continuous Mapping Theorem in Action",
    subtitle = expression("If " * Z[n] %->% 1 * " then log(" * Z[n] * ") %->% 0"),
    x = expression(log(Z[n])),
    y = "Density"
  ) +
  theme_minimal()
