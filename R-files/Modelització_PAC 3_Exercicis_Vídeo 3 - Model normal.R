# Model normal amb mitjana i variança desconeguda (exemple)

# Dades previes (suposició normal)
min_weight <- 1.3
max_weight <- 3.3
avg_weight <- 2.3
n_obs <- 10

# Prior distribution: normal(mu0, sigma / sqrt(kappa0))
mu0 <- avg_weight
kappa0 <- n_obs
sigma <- ((max_weight - min_weight) / 6) # Normal range is 6*sd
sigma_2 <- (sigma / 6) ^ 2 # Variance
nu0 <- n_obs

# Plot distribution of the prior MEAN
# Sequence of x values for plotting
x_values <- seq(mu0 - 3*sigma, mu0 + 3*sigma, length.out = 100)
# Calculate the density
density_values <- dnorm(x_values, mean = mu0, sd = sigma / sqrt(kappa0))
# Plot the distribution
plot(x_values, density_values, type = "l", main = "Prior Distribution", xlab = "x", ylab = "Density")

# Dades noves
kiwi_weight <- c(1.76, 3.29, 2.02, 2.04, 1.5, 2.36, 2.34, 2.19, 2.45, 2.71, 2.11,
                 1.85, 2.11, 2.51, 2.42, 2.15, 3.09, 2.67, 2.75, 2.35)

# Likelihood
y_i <- kiwi_weight
y_bar <- mean(kiwi_weight)
n <- length(kiwi_weight)
s_2 <- sum((y_i - y_bar) ^ 2) / n
# Calculate distribution ???????????

# Posterior mean
posterior_mean <- (n * y_bar + kappa0 * mu0) / (n + kappa0)
# Posterior variance
nu_n <- nu0 + n
sigma_2_n <- (n * s_2 + nu0 * sigma_2 + ((n * kappa0)
             / # -----------------------------------------
             (n + kappa0)) * (y_bar - mu0) ^ 2) / (nu0 + n)
