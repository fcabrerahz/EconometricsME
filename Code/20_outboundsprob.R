# Load required libraries
library(ggplot2)
library(dplyr)

set.seed(123)

### 1. LPM where predicted probabilities fall outside [0,1]
n <- 1000
X1 <- rnorm(n, mean = 0, sd = 2)
beta1 <- 0.2 + 0.2 * X1   # linear index
P1 <- beta1  # predicted probabilities (can fall outside [0,1])
Y1 <- rbinom(n, size = 1, prob = pmin(pmax(P1, 0), 1))  # clip prob for sim

df1 <- data.frame(
  X = X1,
  Predicted = P1,
  Type = "Violates bounds"
)

### 2. LPM where predicted probabilities stay within [0,1]
X2 <- runif(n, -1, 1)
P2 <- 0.4 + 0.2 * X2  
Y2 <- rbinom(n, size = 1, prob = P2)

df2 <- data.frame(
  X = X2,
  Predicted = P2,
  Type = "Respects bounds"
)

### Combine both for plotting
df_all <- bind_rows(df1, df2)

# Plot predicted probabilities with custom colors
ggplot(df_all, aes(x = X, y = Predicted, color = Type)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = c(0, 1), linetype = "dashed", color = "grey50") +
  scale_color_manual(
    values = c(
      "Violates bounds" = "navy",
      "Respects bounds" = "maroon"
    )
  ) +
  labs(
    title = "Predicted Probabilities in Linear Probability Models",
    x = "X",
    y = "Predicted Probability",
    color = "Model"
  ) +
  theme_minimal()

