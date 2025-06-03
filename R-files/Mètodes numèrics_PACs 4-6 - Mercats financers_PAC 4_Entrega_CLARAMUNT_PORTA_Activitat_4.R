### PAC 4
### Terenci Claramunt Porta

# Tipus d'interès
ti1 = 3.30 / 100
ti2 = 3.61 / 100
ti3 = 3.81 / 100
ti4 = 4.31 / 100
ti5 = 4.50 / 100
ti6 = 4.48 / 100
ti7 = 4.47 / 100
ti8 = 4.25 / 100
ti9 = 4.15 / 100
ti10= 4.00 / 100

ti = c(ti1, ti2, ti3, ti4, ti5, ti6, ti7, ti8, ti9, ti10)

# Temps de venciment:
v1 = 1 / 12
v2 = 2 / 12
v3 = 3 / 12
v4 = 6 / 12
v5 = 1
v6 = 2
v7 = 3
v8 = 5
v9 = 7
v10= 10

v = c(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)

# Venciments a interpolar
T1 = 1.5 / 12
T2 = 10 / 12
T3 = 6.5
T4 = 9


# Regla de Horner per avaluar polinomis
horner = function(x, coefs) {
  y = rep(0, length(x))
  for(i in length(coefs):1)
    y = coefs[i] + x*y
  return(y)
}

# Calcul del polinomi pel mètode de Lagrange
library("PolynomF")
lagrange = poly_calc(v, ti)
lagrange_coefficients = print(lagrange)

# Càlcul dels tipus d'interès interpolats pel mètode de Lagrange
print(horner(T1, lagrange_coefficients) * 100)
print(horner(T2, lagrange_coefficients) * 100)
print(horner(T3, lagrange_coefficients) * 100)
print(horner(T4, lagrange_coefficients) * 100)

# Gràfic del polinomi interpolador de Lagrange
x = seq(v1, v10, 0.001)
y = horner(x, lagrange_coefficients)
plot(x, y, cex = 0.2)
title(main = "Polinomi interpolador de Lagrange")


# Calcul del polinomi pel mètode de les diferències dividides de Newton

# Crea la matriu que contindrà els càlculs de les diferències dividides
# on la primera columna són els venciments i la segona els tipus d’interès
ndd = cbind(v, ti, matrix(NA, nrow = 10, ncol = 9))

# Itera sobre la matriu a partir de la 3a columna, per cada fila
for(col in 3:ncol(ndd)) {
  for(row in 1:nrow(ndd)) {
    # Calcula les diferències dividides
    ndd[row, col] = (ndd[row+1, col-1] - ndd[row, col-1]) / (ndd[row+col-2, 1] - ndd[row, 1])
    # Fi dels càlculs (cel·les buides)
    if (row == nrow(ndd) - col+2) break
  }
}

# Taula de les diferències dividides de Newton
print(ndd)

# Funció que aplica la fòrmula d'interpolació de Newton
# per avaluar el polinomi interpolador de les diferències dividides
ndd_polynomial = function(x) {
  # Primer terme de la fòrmula (sense variable)
  y = ndd[1, 2]
  # Avaluació de cada terme amb variable
  for(col in 3:ncol(ndd)) {
    # Factor sense variable
    z = ndd[1, col]
    # Producte dels factors de cada terme
    for(i in 1:(col-2)) {
      z = z * (x - ndd[i, 1])
    }
    # Suma el valor de cada terme al resultat
    y = y + z
  }
  return(y)
}

# Càlcul dels tipus d'interès interpolats pel mètode de
# les diferències dividides de Newton
print(ndd_polynomial(T1) * 100)
print(ndd_polynomial(T2) * 100)
print(ndd_polynomial(T3) * 100)
print(ndd_polynomial(T4) * 100)

# Gràfic del polinomi interpolador de Newton
x = seq(v1, v10, 0.001)
y = ndd_polynomial(x)
plot(x, y, cex = 0.2)
title(main = "Polinomi interpolador de Newton")


# Interpolació per trams lineal
linterp = function(x1, y1, x2, y2) {
  m = (y2 - y1) / (x2 - x1)
  b = y2 - m * x2
  return(c(b, m))
}

# Resultats interpolació per trams lineal
l1 = linterp(v1, ti1, v2, ti2)
r1 = l1[[2]] * T1 + l1[[1]] * 100

l2 = linterp(v4, ti4, v5, ti5)
r2 = l2[[2]] * T2 + l2[[1]] * 100

l3 = linterp(v8, ti8, v9, ti9)
r3 = l3[[2]] * T3 + l3[[1]] * 100

l4 = linterp(v9, ti9, v10, ti10)
r4 = l4[[2]] * T4 + l4[[1]] * 100

