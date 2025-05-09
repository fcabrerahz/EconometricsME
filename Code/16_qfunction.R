# Define the check function
rho <- function(x, tau) {
  ifelse(x < 0, x * (tau - 1), x * tau)
}

# Create a sequence of x values
x_vals <- seq(-3, 3, length.out = 500)

# Compute values of the check function for different taus
r_tau_05 <- rho(x_vals, 0.5)
r_tau_02 <- rho(x_vals, 0.2)
r_tau_08 <- rho(x_vals, 0.8)

# Plot
plot(x_vals, r_tau_05, type = "l", lwd = 2, col = "black",
     ylim = c(-1.5, 2), ylab = expression(rho[tau](x)), xlab = "x",
     main = "Quantile Check Loss Function")
lines(x_vals, r_tau_02, col = "blue", lwd = 2, lty = 2)
lines(x_vals, r_tau_08, col = "red", lwd = 2, lty = 3)
abline(h = 0, v = 0, col = "gray", lty = 3)

legend("topleft",
       legend = c(expression(tau == 0.5), expression(tau == 0.2), expression(tau == 0.8)),
       col = c("black", "blue", "red"),
       lwd = 2, lty = c(1, 2, 3))
