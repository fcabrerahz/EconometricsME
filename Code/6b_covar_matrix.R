set.seed(123654)

n <- 2000

# Regressors
x1 <- rnorm(n)
x2 <- rnorm(n)
X  <- cbind(1, x1, x2)              # include intercept
beta <- c(1, 2, -1)

# 1) HOMOSKEDASTIC ERRORS
sigma <- 1
e_homo <- rnorm(n, mean = 0, sd = sigma)
y_homo <- as.vector(X %*% beta + e_homo)

m_homo <- lm(y_homo ~ x1 + x2)

# Variance-covariance matrix (classical OLS)
V_homo <- vcov(m_homo)

cat("\n--- HOMOSKEDASTIC CASE ---\n")
cat("Estimated coefficients:\n")
print(coef(m_homo))

cat("\nTop-left 3x3 of vcov (entire matrix here since 3 params):\n")
print(V_homo[1:3, 1:3])

cat("\nStandard errors (sqrt of diagonal):\n")
print(sqrt(diag(V_homo)))

# 2) HETEROSKEDASTIC ERRORS

sigma_i <- 0.3 + 1.5 * abs(x1)      # observation-specific sd
e_hetero <- rnorm(n, mean = 0, sd = sigma_i)
y_hetero <- as.vector(X %*% beta + e_hetero)

m_hetero <- lm(y_hetero ~ x1 + x2)

# Classical (wrong under heteroskedasticity) vcov:
V_hetero_classical <- vcov(m_hetero)

# Robust (White/HC1) vcov:
# install.packages("sandwich")  # if needed
library(sandwich)
V_hetero_robust <- vcovHC(m_hetero, type = "HC1")

cat("\n--- HETEROSKEDASTIC CASE ---\n")
cat("Estimated coefficients:\n")
print(coef(m_hetero))

cat("\nClassical vcov (not robust):\n")
print(V_hetero_classical[1:3, 1:3])

cat("\nRobust vcov (HC1):\n")
print(V_hetero_robust[1:3, 1:3])

cat("\nStandard errors (classical):\n")
print(sqrt(diag(V_hetero_classical)))

cat("\nStandard errors (robust HC1):\n")
print(sqrt(diag(V_hetero_robust)))