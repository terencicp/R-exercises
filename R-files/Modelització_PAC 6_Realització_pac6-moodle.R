library(coda)
library(rjags)

data <- list(
    n = 10,
    goals = c(14, 98, 220, 904, 2, 475, 232, 21, 1, 113),
    speed = c(5.9, 5.1, 6.6, 7.0, 9.5, 7.1, 6.5, 8.7, 7.0, 7.7),
    acceleration = c(6.0, 6.9, 8.5, 9.9, 8.1, 9.5, 8.5, 8.8, 5.9, 9.1)
)

model_string <- "
    model {
        for (i in 1:n) {
            goals[i] ~ dpois(lambda[i])
            log(lambda[i]) <- b0 + b1*speed[i] + b2*acceleration[i]
        }
        b0 ~ dnorm(0, 1/100^2)
        b1 ~ dnorm(0, 1/100^2)
        b2 ~ dnorm(0, 1/100^2)
    }
"

init <- function() rnorm(1, 0, 100)

model_text <- textConnection(model_string)
model <- jags.model(model_text, data, n.chains = 3, n.adapt = 5000)

update(model, n.iter = 5000)

vars <- c("b0", "b1", "b2")
samples <- coda.samples(model, vars, n.iter = 500000, thin = 20, inits = init())

gelman.diag(samples)
effectiveSize(samples)

results <- summary(samples)
b0 <- results$statistics["b0", "Mean"]
b1 <- results$statistics["b1", "Mean"]
b2 <- results$statistics["b2", "Mean"]
exp(b0 + b1 * 7.11 + b2 * 8.12)
