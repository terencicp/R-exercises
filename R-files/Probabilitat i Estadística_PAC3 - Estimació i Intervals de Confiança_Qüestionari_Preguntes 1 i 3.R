z.test <- function(MOSTR, desv.tipica, nivell.confiança) {
    esquerra <- mean(MOSTR) - qnorm(1 - (1 - nivell.confiança) / 2) * desv.tipica / sqrt(length(MOSTR))
    dreta <- mean(MOSTR) + qnorm(1 - (1 - nivell.confiança) / 2) * desv.tipica / sqrt(length(MOSTR))
    return(c(esquerra,dreta))
}

# Pregunta 1

sample <- c(465,455,457,455,461,478,489,452,473,462)
z.test(sample, 28, 0.90)

# Pregunta 3
n <- length(sample)
critical_value <- qnorm(p = 1 - (1 - 0.95) / 2)
sd <- 25
me <- 3
min_sample_size <- round(critical_value^2 * sd^2 / me^2)
min_sample_size