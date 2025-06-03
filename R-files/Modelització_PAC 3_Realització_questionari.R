library(ggplot2)

# plots
plot_gamma <- function(shape, rate, label) {
    x <- seq(30, 80, by = 0.1)
    densities <- dgamma(x, shape = shape, rate = rate)
    data_plot <- data.frame(lambda = x, density = densities, class = label)
    ggplot(data_plot, aes(x = lambda, y = density, color = class, linetype = class)) +
        geom_line(size = 1.2) +
        xlab(expression(lambda)) +
        theme(legend.position = "bottom", legend.title = element_blank())
}

# b
a_priori_mean <- 58
a_priori_var <- 64
# a / b = 58
# a / b^2 = 64
# 58b / b^2 = 64
b <- a_priori_mean / a_priori_var
a <- a_priori_mean * b

# c
higher_than <- 56
1 - pgamma(higher_than, shape = a, rate = b)
# Plot
plot_gamma(a, b, "a priori")

# d
a_priori_mean <- 58
a_priori_var <- 9
b <- a_priori_mean / a_priori_var
a <- a_priori_mean * b

# e
higher_than <- 56
1 - pgamma(higher_than, shape = a, rate = b)
# Plot
plot_gamma(a, b, "a priori")

# f
data <- c(56, 49, 55, 63, 50, 58, 55, 68, 65, 58, 67, 65, 60, 52, 65)
r <- sum(data)
n <- length(data)
aP <- a + r
bP <- b + n

# g
higher_than <- 59
1 - pgamma(higher_than, shape = aP, rate = bP)
# Plot
plot_gamma(aP, bP, "a posteriori")
