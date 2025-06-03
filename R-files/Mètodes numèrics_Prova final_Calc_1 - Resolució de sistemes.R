# Fraccions: fracture()
library(fracture)

# ERRORS

# Valor real i aproximat
p <- 62
p_aprox <- 60
# Errors
paste("Error absolut: ", abs(p - p_aprox))
paste("Error realtiu: ", abs(p - p_aprox) / abs(p))

# ------------------------------------------------------------------------------

# MÈTODES DIRECTES DE RESOLUCIÓ DE SISTEMES

# Matriu i vector
# 2
A <- matrix(c(-7,3,9,2), nrow = 2, byrow = TRUE)
b <- c(-8,9)
# 3
A <- matrix(c(-8,-32,24,-5,-17,-9,-6,-20,-9), nrow = 3, byrow = TRUE)
b <- c(7,3,6)

# Solució del sistema:
x <- solve(A, b)
print(x)

# Mètode de Gauss-Jordan
x <- inv(A) %*% b
print(x)

# Descomposició LU
library(pracma)
# Requisits
for (k in 1:nrow(A)) {
  submatriu_principal <- matrix(A[1:k, 1:k], nrow=k, byrow=TRUE)
  menors_principals[k] <- det(submatriu_principal)
}
print(paste("Can LU be applied?:", !(0 %in% menors_principals)))
# Doolittle
L <- lu(A)$L
U <- lu(A)$U
# Crout
L <- lu_crout(A)$L
U <- lu_crout(A)$U
# Print
print(L)
print(U)
# Resolució del sistema
y = solve(L, b) # resoldre Ly = b
x = solve(U, y) # resoldre Ux = y
print(y)
print(x)

# ------------------------------------------------------------------------------

# MÈTODES ITERATIUS DE RESOLUCIÓ DE SISTEMES

# 1 Matriu i vector  # Important ! ! !
# a <- 0.7; b <- 0.7;
# k <- -6.1
# 2
A <- matrix(c(a,b,c,d), nrow = 2, byrow = TRUE)
b <- c(1,1)
# 3
A <- matrix(c(4,-2,1,1,2,3,-1,3,3), nrow = 3, byrow = TRUE)
b <- c(3,6,5)
# 4
A <- matrix(c(1,-1,-3,3,-4,1,-1,2,-1,2,1,-2,-2,-2,-4,1), nrow = 4, byrow = TRUE)
b <- c(4,-4,0,0)

# 2 Descomposició A = D-L-U  # Important ! ! !
D <- matrix(0, nrow(A), ncol(A))
diag(D) <- diag(A)
L = -A; U = -A
L[upper.tri(A, diag=TRUE)] <- 0
U[lower.tri(A, diag=TRUE)] <- 0

# 3 Iterant inicial  # Important ! ! !
x0 = rep(0, 4)

# 4 Jacobi
# Matriu i vector d'iteració
J = inv(D) %*% (L+U)
c = inv(D) %*% b
print(J)
print(c)
# Primeres iteracions
x1 = J %*% x0 + c
x2 = J %*% x1 + c
x3 = J %*% x2 + c
print(x3)
# N iteracions
nmax = 100
solJ = itersolve(A, b, x0, nmax, method="Jacobi") # Set "tol" to limit error
print(solJ)

# 4 Gauss-Seidel
# Matriu i vector d'iteració
G = inv(D - L) %*% U
d = inv(D - L) %*% b
print(G)
print(d)
# Primeres iteracions
x1 = G %*% x0 + d
x2 = G %*% x1 + d
x3 = G %*% x2 + d
print(x3)
# N iteracions
nmax = 100
solG = itersolve(A, b, x0, nmax, method="Gauss-Seidel") # Set "tol" to limit error
print(solG)

# Convergència < 1
max(abs(eigen(J)$values)) # Jacobi
max(abs(eigen(G)$values)) # Gauss-Seidel

# Cota de l'error relatiu
norm(inv(A), "2") * norm(A, "2") * norm((A %*% solG$x - b), "2") / norm(b, "2") 

# Error absolut (en norma infinit)
x <- solve(A, b)
norm(x - x1, "I")

# ------------------------------------------------------------------------------

# CÀLCUL DE VALORS PROPIS

# Matriu
A <- matrix(c(1,1,1, 1,1,1, -1,0,-1), nrow = 3, byrow = TRUE)

# Valors exactes (per a el calcul d'errors)
exact_value <- max(eigen(A)$values)
exact_vector <- eigen(A)$vectors[,1]

# Mètode de la potència
power_method <- function(niter, x0) {
  xk = x0
  for (i in 1:niter){
    xk = A %*% xk
    max_xk = max(xk)
    xk = xk / max_xk
  }
  return(list("value" = max_xk, "vector" = xk))
}
approx <- power_method(niter = 10, x0 = c(1,1,1))

# Error relatiu de l'autovalor en norma 1
abs(exact_value - approx$value) / abs(exact_value)
# Error relatiu de l'autovector en norma 1 (ajustat)
adj_approx_vector <- approx$vector / norm(approx$vector, "2")
norm(exact_vector - adj_approx_vector, "1") / norm(adj_approx_vector, "1")

# Mètode de Raleigh
niter = 4
I = diag(4) #Matriu identitat
xk = matrix(c( 0, 0, 0)) #Iterant inicial
for (i in 1:niter){
  sigma_i = t(xk) %*% A %*% xk / t(xk) %*% xk 
  sigma_i = sigma_i[1,1]
  xk = inv(A - sigma_i * I) %*% xk # Iteracio potencia inversa
  xk = xk / max(abs(xk))
}
sigma_i #Autovalor
xk #Autovector

