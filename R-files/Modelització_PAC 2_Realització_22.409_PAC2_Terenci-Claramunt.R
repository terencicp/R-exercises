# Activitat 1

# a)

set.seed(14)
n_1a <- 10000
mean_1a <- 2
sd_1a <- 0.07
sim_1a <- rnorm(n_1a, mean = mean_1a, sd = sd_1a)
mean(sim_1a)
sd(sim_1a)

# b)

sum(sim_1a < 1.8) / n_1a
pnorm(1.8, mean = mean_1a, sd = sd_1a)

# c)

defective <- sum(sim_1a < 1.8 | sim_1a > 2.2)
defective
defective / n_1a

# Activitat 2

# c)

rcasera <- function(n, α) {
    if (α <= 2) stop("α > 2")
    return(-α / (runif(n) - α))
}

# d)
set.seed(14)
mean(rcasera(10000, 5))
