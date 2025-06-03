library(fracture)

# APROXIMACIÓ

# Producte escalar discret
x <- c()
y <- c()
fracture(sum((-1 + 2*x^2) * y))

# Polinomis de Txebixev
library(mpoly)
chebyshev(2) # Polinomi
chebyshev_roots(k = 2, n = 3) # Zeros: k = root, n = degree of polynomial (k ≤ n)
# Canvi de variable
a = -10
b = 10
# De [a,b] a [-1,1]
t = 10
x = (((1 / 2) * (a + b)) - t) / ((-1 / 2) * (b - a))
# De [-1,1] a [a,b]
x = 1
t = ((1 / 2) * (a + b)) + ((1 / 2) * (b - a) * x)
# Aproximació (no fa falta canvi de variable!)
library(pracma)
a <- -pi
b <- pi
x <- seq(a, b, length.out = 101)
x <- c(-5,-2,2,6,8)
y <- c(0,1/2,-2,5/4,2)
chebApprox(x, y, -5, 8, n = 2) # n = degree

# Polinomis de Legendre
library(orthopolynom)
legendre.polynomials(3)

# Coeficients dels polinòmis trigonomètrics (Series de Fourier)
fs_coeffs = function(x, y, n) {
  M = length(x) - 1
  coeffs = c()
  for (k in seq(-n,n)) {
    c_coeff = sum(y * exp(-1i * k * x)) / (M + 1)
    coeffs = append(coeffs, c_coeff) }
  return(coeffs)
}
# Escalar les dades entre (-π i π) o (0 i 2π)
x <- c()
y <- c()
x_pi = x * 2*pi / max(x)
coeffs_fourier = fs_coeffs(x_pi, y, n=2)


# REGRESSIÓ

# Desviació típica - Comparació
x <- c(1,2,5,8,9)
y1 <- c(10,7,3,15,2)
y2 <- c(14,16,3,3,18)
# Sense x
sd(y1)
sd(y2)
# Amb x
sqrt(sum(resid(lm(y1 ~ x))^2) / (length(y1) - 2))
sqrt(sum(resid(lm(y2 ~ x))^2) / (length(y2) - 2))

# Variància
x <- c(1,2,5,8,9)
# Variància
sum((x - mean(x)) ** 2) / length(x)
# Variància mostral
sum((x - mean(x)) ** 2) / (length(x) - 1)

# Covariància
x <- c(1,4,7,9,12)
y <- c(13,5,14,2,17)
# Covariància
fracture(sum((x - mean(x)) * (y - mean(y))) / length(y))
# Covariància mostral
fracture(sum((x - mean(x)) * (y - mean(y))) / (length(y) - 1))

# Coeficient de correlació mostral
x <- c(1,2,5,8,9)
y <- c(3,4,20,10,19)
cor(x, y)

# Regressió lineal simple
x <- c(1,2,5,7,9)
y <- c(19,17,5,20,17)
l <- lm(y ~ x)
fracture(l$coefficients)
# Error estàndard
summary(l)$sigma
# Suma de quadrats totals: SQT = SQR + SQE
sum((y - mean(y)) ** 2)
# Suma de quadrats de la regressió: SQR
sum((fitted(l) - mean(y)) ** 2)
# Suma de quadrats dels errors: SQE
sum(resid(l)^2)

# Comparació de regressions lineals simples + Predicció
x_pred <- 20 # Valor a predir
x <- c(1,3,5,7,8)
y1 <- c(12,14,20,7,11)
y2 <- c(4,3,19,13,17)
l1 <- lm(y1 ~ x)$coefficients
l2 <- lm(y2 ~ x)$coefficients
fracture(l1[1] + l1[2] * x_pred)
fracture(l2[1] + l2[2] * x_pred)

# Regressió polinòmica: Returns c0, c1, c2...
poly = function(x, y, degree){
  Phi = matrix(1, length(x), degree+1)
  for (i in 1:degree) {
    Phi[, i+1] = x**i }
  PhiT = t(Phi)
  return(solve(PhiT %*% Phi, PhiT %*% y))
}
x <- c(0,3,4,6,8)
y <- c(-1,-5,5,-1,-5)
fracture(poly(x, y, degree = 2))

# Avalua la regressió polinòmica
polyeval = function(x, coeffs) {
  y = 0
  for (i in 1:length(coeffs)) {
    y = y + coeffs[i] * x**(i-1) }
  return(y)
}

# Coeficient de determinació, R^2 (SQR / SQT)
coeff_det = function(y, y_fit) {
  Sr = sum((y - y_fit) ^ 2)
  y_mean = mean(y)
  St = sum((y - y_mean) ^ 2)
  return((St - Sr) / St)
}
x <- c(10,30,60,90,120)
y <- c(0.5,1,3,5,6.5)
coeff_det(y, fitted(lm(y ~ x)))

# Incògnita en punt de regressió lineal 
c0 <- -9/11 # Calcular c0 a CalcMe
for (i in -10:10) {
  x <- c(0,1,2,4,7)
  y <- c(-1,2,i,-1,-2)
  if (fracture(lm(y ~ x)$coefficients[1]) == c0) {
    print(i)
  }
}

# Regressió lineal múltiple
x1 <- c(0,1,3,4,6)
x2 <- c(1,2,3,4,5)
y2 <- c(3,5,3,8,2)
lm(y ~ x1 + x2)



