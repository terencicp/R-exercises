library(fracture)

# INTERPOLACIÓ

# Vandermonde - Determinant
x <- c(-5,-2,0,4,6)
V <- outer(x, seq(0, length(x) - 1), `^`)
det(V)

# Lagrange
library("PolynomF")
x = c(1,2,4,5,8)
y = 3^x + 2
lagrange = poly_calc(x, y)
lagrange_coefficients = print(lagrange)
fracture(print(lagrange))
# Avaluar Lagrange
horner = function(x, coefs) {
  y = rep(0, length(x))
  for(i in length(coefs):1)
    y = coefs[i] + x*y
  return(y) }
x <- 10
approx <- horner(x, lagrange_coefficients)
fracture(approx)
# Error lagrange
actual <- 2^(-2*x) + 2
abs(actual - approx)

# Diferències dividides
x = c(1,2,4,5,8)
y = c(10,5,2,4,14)
ndd = cbind(x, y, matrix(NA, nrow = length(x), ncol = length(x)-1))
for(col in 3:ncol(ndd)) {
  for(row in 1:nrow(ndd)) {
    ndd[row, col] = (ndd[row+1, col-1] - ndd[row, col-1]) / (ndd[row+col-2, 1] - ndd[row, 1])
    if (row == nrow(ndd) - col+2) break
  }
}
print(ndd) # Taula de les diferències dividides
fracture(ndd[1,]) # Primera fila en fraccions
# Avaluació de ndd
ndd_polynomial = function(x) {
  y = ndd[1, 2]
  for(col in 3:ncol(ndd)) {
    z = ndd[1, col]
    for(i in 1:(col-2)) {
      z = z * (x - ndd[i, 1]) }
    y = y + z }
  return(y)
}
ndd_polynomial(8)

# Interpolació per trams lineal
x1 = 0
x2 = 3
y1 = 3*x1^2 - 4*x1 + 2
y2 = 4*x2^2 + 5*x2 + 5
l <- lm(c(y1, y2) ~ c(x1, x2))
fracture(l$coefficients)
x <- 85/3
fracture(l$coefficients[1] + l$coefficients[2] * x)
# Valors de y
for (i in 0:10) {
  print(paste(i, l$coefficients[1] + l$coefficients[2] * i))
}


# DERIVACIÓ

# Primera derivada
findiff = function(f, x, h) {
  return((f(x + h) - f(x)) / h)
}
findiff(function(x) exp(x), 4, 0.001)

# Segona derivada
findiff2 = function(f, x, h) {
  return((f(x + h) - 2 * f(x) + f(x - h)) / (h^2))
}
findiff2(function(x) 2*x**2, 3, 0.0001)

# INTEGRACIÓ

# Regla dels trapezis (usant una funció) [trapezi simple: m = 1]
trapezis_func = function(f, a, b, m) {
  y = f(seq(a, b, length.out=m+1))
  return(sum((y[2:(m+1)] + y[1:m])) * (abs((b-a)/m) / 2))
}
f = function(x) {
  return(2*log10(x+1))
}
trapezis_func(f, 0, 1, 10)
# Problema: Valor a+b amb error absolut
possible_values <- c(1,-1,0) # Fill in (a=b -> 0)
for (i in possible_values) {
  if (i == 0) { a <- 1; b <- 1 }
  else { a <- i / 2; b <- i / 2 }
  f = function(x) a*exp(x) + b*exp(-x)
  A <- exp(1) - exp(-1) # Fill in
  T <- trapezis_func(f, a=-1, b=1, m=1) # Fill in
  actual_error <- 2*exp(-1) # Fill in (|A-T|)
  if(abs(A - T) == actual_error) { print(i) }
}

# Regla dels trapezis (per punts)
trapezis_punts = function(x, y) {
  area = 0
  for (i in 1:(length(x)-1)) {
    area = area + ((x[i+1] - x[i]) / 2) * (y[i] + y[i+1]) }
  return(area)
}
fracture(trapezis_punts(
  x = c(0,1/6,1/4),
  y = c(0,0.3083,0.4463)
))

# Buscar valor de t
fa = -3 * exp(-1) - 3 * exp(1)
T = (-27*exp(16) + 27*exp(9) + 27*exp(7) - 27) / (2*exp(8))

# Regla de Simpson
simp = function(f, a, b, m) {
  x.ends = seq(a, b, length.out=m+1)
  y.ends = f(x.ends)
  x.mids = (x.ends[2:(m+1)] - x.ends[1:m]) / 2 + x.ends[1:m] 
  y.mids = f(x.mids)
  p.area = sum(y.ends[2:(m+1)] + 4 * y.mids[1:m] + y.ends[1:m])
  p.area = p.area * abs((b - a) / (2 * m)) / 3 # h/3
  return(p.area)
}
f = function(x) {
  return(log10(x+3))
}
simp(f,0,1,10)

