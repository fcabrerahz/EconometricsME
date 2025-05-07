# Load necessary library
library(ggplot2)

# Define a range of residuals
residuals <- seq(-4, 4, length.out = 500)

# Compute the OLS (squared) and LAD (absolute) loss functions
ols_loss <- residuals^2
lad_loss <- abs(residuals)

# Combine into a data frame
loss_df <- data.frame(
  Residual = residuals,
  OLS = ols_loss,
  LAD = lad_loss
)

# Plot
ggplot(loss_df, aes(x = Residual)) +
  geom_line(aes(y = OLS, color = "OLS Loss: $e^2$"), size = 1.2) +
  geom_line(aes(y = LAD, color = "Median Loss: $|e|$"), size = 1.2, linetype = "dashed") +
  scale_color_manual(values = c("OLS Loss: $e^2$" = "blue", "Median Loss: $|e|$" = "red")) +
  labs(
    title = "Loss Functions: OLS vs. Median Regression",
    y = "Loss",
    x = "Residual (Prediction Error)",
    color = "Loss Function"
  ) +
  theme_minimal(base_size = 14)
