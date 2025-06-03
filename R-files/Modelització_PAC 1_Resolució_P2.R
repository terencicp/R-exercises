# a)

C1 <- log(0.74) - log(0.22)
C1

# b)

prior <- function(θ) { 1 / (θ * C1) }
integrate(prior, lower=0.6, upper=0.9)$value

# c)

C_post <- function(θ) { exp(-θ) }
C2 <- integrate(C_post, lower=0.22, upper=0.74)$value
integrate(C_post, lower=0.6, upper=0.9)$value / C2

# d)

y_marg <- function(y) {
    y * (1 / (y * C1) * (exp(-0.22 * y) - exp(-0.74 * y)))
}
integrate(y_marg, lower=0, upper=365)$value