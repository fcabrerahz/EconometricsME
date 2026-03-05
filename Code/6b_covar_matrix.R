set.seed(123654)

n <- 2000

# Regressors
x1 <- rnorm(n)
x2 <- rnorm(n)
X  <- cbind(1, x1, x2)          
beta <- c(1, 2, -1)

# HOMOSKEDASTIC ERRORS
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

#now make the variance of beta larger/smaller.