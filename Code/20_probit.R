# Load necessary packages
library(tidyverse)

# 1. Simulate data ----------------------------------------------
set.seed(123)
n <- 1000
age <- runif(n, 20, 60)           # simulate ages between 20 and 60
income <- runif(n, 0, 20000)      # simulate income between 0 and 20,000

# True coefficients
beta_true <- c(intercept = -0.5, age = 0.04, income = 0.001)
XB <- beta_true["intercept"] + beta_true["age"] * age + beta_true["income"] * income
P <- pnorm(XB)
Y <- rbinom(n, size = 1, prob = P)
df <- data.frame(Y = Y, age = age, income = income)

# 2. Estimate probit model --------------------------------------
probit_model <- glm(Y ~ age + income, data = df, family = binomial(link = "probit"))
summary(probit_model)

# 3. Predict for a custom observation ---------------------------
new_data <- data.frame(age = 30, income = 500)
X <- model.matrix(~ age + income, data = new_data)
beta_hat <- coef(probit_model)
linear_index <- X %*% beta_hat
predicted_prob <- pnorm(linear_index)

cat("For age = 30 and income = 500:\n")
cat("Linear index (Xᵗβ̂):", round(linear_index, 3), "\n")
cat("Predicted probability (Φ):", round(predicted_prob, 4), "\n")

# 4. Visualize the CDF over a range of index values -------------
index_range <- seq(-4, 4, by = 0.01)
phi_vals <- pnorm(index_range)

cdf_df <- data.frame(index = index_range, probability = phi_vals)

ggplot(cdf_df, aes(x = index, y = probability)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_vline(xintercept = linear_index, linetype = "dashed", color = "red") +
  geom_point(aes(x = linear_index, y = predicted_prob), color = "red", size = 3) +
  labs(
    title = "Standard Normal CDF (Probit Link Function)",
    subtitle = "Dashed red line shows Xᵗβ̂ for age = 30, income = 500",
    x = "Linear Index (Xᵗβ̂)",
    y = "Predicted Probability Φ(Xᵗβ̂)"
  ) +
  theme_minimal()

# 5. AME for income only ----------------------------------------
beta_hat <- coef(probit_model)
X_mat <- model.matrix(probit_model)

# Compute linear index and marginal density
index_hat <- X_mat %*% beta_hat
g_hat <- dnorm(index_hat)

# Compute AME for income only: AME_income = β_income × mean(g)
beta_age <- beta_hat["age"]
g_mean <- mean(g_hat)
AME_age <- beta_age * g_mean

cat("Average Marginal Effect of income:\n")
print(AME_age)

#comparing
install.packages("margins")
library(margins)
margins(probit_model)
