library(ggplot2)

set.seed(100)

# data <- data.frame(Wp = rpois(25, lambda = 140))
data <- data.frame(Wp = c(134, 121, 150, 141, 143, 133,
                          145, 135, 141, 141, 137,
                          154, 133, 135, 154, 143, 126, 
                          149, 143, 149, 130, 129, 142, 126, 130))
y <- data$Wp
r <- sum(y)

# Funció de versemblança
Lpois <- function(y, lambda){
    prod(dpois(y, lambda = lambda))
}

# Valor de la funció de versemblança en diferents punts
step_size <- 0.01
lambda_x <- seq(100, 165, by = step_size)
likelihood <- sapply(lambda_x, FUN = Lpois, y = y)

# Likelihood function plot
data_vero <- data.frame(x = lambda_x, y = likelihood, class = "versemblança")
ggplot(data = data_vero) +
    geom_line(aes(x = lambda_x, y = y, color = class, linetype = class),
              size = 1.2) + scale_linetype_manual(values=c("solid"))+
    scale_color_brewer(palette="Dark2") +
    ylab(expression(f(lambda))) +
    xlab(expression(lambda)) +
    theme_light() +
    theme(axis.text = element_text(size=15),
          axis.title = element_text(size=18),
          legend.text = element_text(size=15)) +
    theme(legend.position = "bottom", legend.title = element_blank())

# Gamma a priori distribution
a <- 1024
b <- 8
data_prior <- data.frame(
    x = lambda_x,
    y = dgamma(lambda_x, shape = a, rate = b),
    class = "a priori")

# A priori + Likelihood function plot
normalizing_constant <- (1 / (step_size * sum(likelihood)))
data_vero$y <- data_vero$y * normalizing_constant # So likelihood func sums to 1
data_plot <- rbind(data_vero, data_prior)
ggplot(data = data_plot) +
    geom_line(aes(x = x, y = y, color = class, linetype = class),
              size = 1.2) + ylab(expression(f(lambda))) +
    xlab(expression(lambda)) +
    theme_light() +
    theme(axis.text = element_text(size=15),
          axis.title = element_text(size=18),
          legend.text = element_text(size=15)) + scale_linetype_manual(values=c("solid", "twodash"))+ scale_color_brewer(palette="Dark2") +
    theme(legend.position = "bottom", legend.title=element_blank())

# Paràmetres de la distribució gamma a posteriori
aP <- a + r
bP <- b + length(y)

# Distribució gamma a posteriori
data_posterior <- data.frame(
    x = lambda_x, 
    y = dgamma(lambda_x, shape = aP, rate = bP ), 
    class = "posteriori")

# Posterior + A priori + Likelihood function plot
data_plot <- rbind(data_vero, data_prior, data_posterior)
ggplot(data = data_plot) +
    geom_line(aes(x = x, y = y, color = class, linetype = class), 
              size = 1.2) +
    ylab(expression(f(lambda))) +
    xlab(expression(lambda)) +
    theme_light() +
    theme(axis.text  = element_text(size=15),
          axis.title = element_text(size=18),
          legend.text = element_text(size=15)) +
    scale_linetype_manual(values=c("solid", "twodash", "dotted"))+
    scale_color_brewer(palette="Dark2") +
    theme(legend.position = "bottom", legend.title=element_blank())

# Mitjana de la distribuació a posteriori
aP / bP

# Interval de credibilitat del 95% de la distribuació a posteriori
c(qgamma(0.025, aP, bP), qgamma(0.975, aP, bP))

# distribució predictiva (usa logaritmes per estabilitat computacional)
dgammapois <- function(r, a, b, n){
    C <- a * log(b) - lgamma(a)
    C2 <- lgamma(a + r) + log(n) * r - lgamma(r + 1) - log(b + n) * (a + r)
    exp(C+C2)
}

# distribució predictiva a priori
ggplot(data = data.frame(
    x = 90:180, 
    y = dgammapois(90:180, a = a, b = b, n = 1)), 
    aes(x = x, y = y)) + 
    geom_bar(stat="identity", fill = "#56B4E9", width = 0.4) +
    theme_light() +
    xlab("Nombre de WhatsApps en una hora") +
    ylab("Probabilitat") +
    theme(axis.text  = element_text(size=15),
          axis.title = element_text(size=16),
          legend.text = element_text(size=15)) 

# distribució predictiva a posteriori és:
ggplot(data = data.frame(
    x = 90:180, 
    y = dgammapois(90:180, a = aP, b = bP, n = 1)), 
    aes(x = x, y = y)) + 
    geom_bar(stat="identity", fill = "#56B4E9", width = 0.4) +
    theme_light() +
    xlab("Nombre de WhatsApps en una hora") +
    ylab("Probabilitat") +
    theme(axis.text  = element_text(size=15),
          axis.title = element_text(size=16),
          legend.text = element_text(size=15)) 