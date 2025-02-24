install.packages("wooldridge")
library(wooldridge)
data("wage1")  # Load the dataset
head(wage1)    # View the first few rows
summary(wage1) # Summary statistics

set.seed(123)  # For reproducibility
sample_data <- wage1[sample(nrow(wage1), 100), c("wage", "educ", "exper", "married")]
head(sample_data)

Y <- log(sample_data$wage)
# Define X matrix (including intercept)
X <- cbind(1, sample_data$educ, sample_data$exper, sample_data$married)

# Display X and Y
print(X[1:5, ])  # Showing only first 5 rows for brevity

#Compute X'X
XTX <- t(X) %*% X
print(XTX)

#compute X'Y
XTY <- t(X) %*% Y
print(XTY)

#compute (X'X)^(-1)
XTX_inv <- solve(XTX)
print(XTX_inv)

#We finally get beta
beta_hat <- XTX_inv %*% XTY
print(beta_hat)

#now directly using lm:
lm_model <- lm(log(wage) ~ educ + exper + married, data = sample_data)
beta_lm <- coef(lm_model)
print(beta_hat)
print(beta_lm)
