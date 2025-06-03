library(coda)
library(rjags)

# 1
model_string <- "
    model {
        a0 ~ dunif(-100, 100)
        a1 ~ dunif(-100, 100)
        for (i in 1:n) {
            lambda[i] <- exp(a0 + a1 * x[i])
            y[i] ~ dpois(lambda[i])
    }}
"

# 2
data <- list(
    n = 21,
    x = c(20,21,23,22,30,33,35,33,30,35,22,20,36,34,53,64,50,40,58,57,58),
    y = c(44,48,46,47,39,33,34,33,34,38,43,34,34,35,26,10,21,31,22,25,26)
)

# 2a
model_text <- textConnection(model_string)
model <- jags.model(model_text, data, n.chains = 3, n.adapt = 500)

# 2b
update(model, n.iter = 500)

# 2c
variables <- c("a0", "a1")
samples <- coda.samples(model, variables, n.iter = 4500, thin = 10)

# 3a
gelman.diag(samples)

# 3b
effectiveSize(samples)

# 3c
# explain: good or bad?

# 4
a0_samples <- do.call(c, lapply(samples, function(x) x[, "a0"]))
a1_samples <- do.call(c, lapply(samples, function(x) x[, "a1"]))
mean(a0_samples)
mean(a1_samples)

# 5
alpha_half <- (1 - 0.95) / 2
quantile(a0_samples, probs = c(alpha_half, 1 - alpha_half))
quantile(a1_samples, probs = c(alpha_half, 1 - alpha_half))

# 6
traceplot(samples)

