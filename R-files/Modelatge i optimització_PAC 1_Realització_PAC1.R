# Modelatge i optimització
# PAC 1

# Taylor polynomials plot
f <- function(x) {
  x * exp(2 * x)
}
p2 <- function(x) {
  exp(2) + 3 * exp(2) * (x-1) + 4 * exp(2) * (x-1) ** 2
}
x <- seq(0, 2, 0.01)
plot(x, f(x), type = "l", ylab = "f", col = "red", ylim = c(0,50), lwd = 2)
abline(a = -2*exp(2), b = 3*exp(2), col = "blue", lwd = 2)
lines(x, p2(x), col = "green3", lwd = 2)

# Neural network differentiation
f = expression( ( (1/(1+exp(-((1/(1+exp(-(w*x+t*y))))+w*z+d*s)))) * (1/(1+exp(-(w*x+t*y)))) )^2 )
D(f,'d')
