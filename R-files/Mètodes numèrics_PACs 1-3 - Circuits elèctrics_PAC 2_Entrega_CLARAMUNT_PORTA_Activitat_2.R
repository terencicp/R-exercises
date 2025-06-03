### PEC 2

### Nom de l'alumne: Terenci Claramunt Porta

installed.packages("pracma")
library(pracma)

## Dades
# Valors per a les resistencies
R_12 = 15
R_23 = 10
R_34 = 5
R_45 = 15
R_25 = 10
R_56 = 15

# Valor del Voltatge
V0 = 150


## Exercici 1.1
# Variables auxiliars
R_I2 = R_23 + R_34 + R_45

# Definicio de matriu A i vector b del sitema de la forma Ax=b
A = matrix(c(1, 1, 0, 1, 
			 0, R_I2, 0, -R_25, 
			 -R_12, R_I2, R_56, 0,
			 0, -1, 1, -1), nrow=4, byrow=TRUE)

b = c(0, 0, -V0, 0)

# Primera iteracio de Jacobi, definir les matrius necessaries
# Es crea una matriu zero amb les dimensions de A
D <- matrix(0, nrow(A), ncol(A))
# Es copien els elements de la diagonal de A a la diagonal de D
diag(D) <- diag(A)
# Es copia la matriu A a les matrius L i U amb els elements multiplicats per -1
L = -A
U = -A
# Els elements de la diagonal i el triangle superior d'L es substiteixen per 0
L[upper.tri(A, diag=TRUE)] <- 0
# Els elements de la diagonal i el triangle inferior d'U es substiteixen per 0
U[lower.tri(A, diag=TRUE)] <- 0

# Definir matriu i vector d'iteracio
J = inv(D) %*% (L+U)
c = inv(D) %*% b
print(J)
print(c)

x0 = rep(0, 4) # vector inicial
x1 = J %*% x0 + c # 1a iteracio de Jacobi
print("Primera iteracio Jacobi:")
print(x1)

# Calcul dels primers iterants
x2 = J %*% x1 + c
x3 = J %*% x2 + c
x4 = J %*% x3 + c

# Resolució amb Jacobi
nmax = 100
solJ = itersolve(A, b, x0, nmax, method="Jacobi") # configurar itersolve
print("Solucio Jacobi:")
print(solJ)
 
# Primera iteracio de Gauss-Seidel definir les matrius necessaries
# Les matrius D, L i U s'han definit anteriorment
M = D - L

# Definir matriu i vector d'iteracio
G = inv(M) %*% U
d = inv(M) %*% b
print(G)
print(d)

# Vector inicial x0 definit anteriorment
x1 = G %*% x0 + d # 1a iteracio de Gauss-Seidel
print("Primera iteracio Gauss-Seidel:")
print(x1)

# Calcul dels primers iterants
x2 = G %*% x1 + d
x3 = G %*% x2 + d
x4 = G %*% x3 + d

# Resolucio amb Gauss-Seidel
nmax = 100
solG = itersolve(A, b, x0, nmax, method="Gauss-Seidel") #configurar itersolve
print("Solucio Gauss-Seidel:")
print(solG)

# Podem calcular el radi espectral de la matriu de cada mètode:
J_spectral_radius <- max(abs(eigen(J)$values))
G_spectral_radius <- max(abs(eigen(G)$values))


## Exercici 1.2

# Noves dades
# Valors per a les resistencies
# omplir els valors de les noves dades
R_12 = 5
R_23 = 10
R_34 = 5
R_45 = 15
R_25 = 10
R_56 = 40

# Valor del Voltatge
V0 = 150

# Variables auxiliars
R_I2 = R_23 + R_34 + R_45

# Definicio de matriu A i vector b del sitema de la forma Ax=b
A = matrix(c(1, 1, 0, 1, 
			 0, R_I2, 0, -R_25, 
			 -R_12, R_I2, R_56, 0,
			 0, -1, 1, -1), nrow=4, byrow=TRUE)

b = c(0, 0, -V0, 0)

# Calculem el radio espectral per a Jacobi matriu de Jacobi
# Es crea una matriu zero amb les dimensions de A
D <- matrix(0, nrow(A), ncol(A))
# Es copien els elements de la diagonal de A a la diagonal de D
diag(D) <- diag(A)
# Es copia la matriu A a les matrius L i U amb els elements multiplicats per -1
L = -A
U = -A
# Els elements de la diagonal i el triangle superior d'L es substiteixen per 0
L[upper.tri(A, diag=TRUE)] <- 0
# Els elements de la diagonal i el triangle inferior d'U es substiteixen per 0
U[lower.tri(A, diag=TRUE)] <- 0
# Matriu del mètode Jacobi
J = inv(D) %*% (L+U)

# Radi espectral - Valor absolut del valor propi màxim
max_autovalor <- max(abs(eigen(J)$values))
print("Radi espectral per a Jacobi:")
print(max_autovalor)

# Calculem el radi espectral per a Gauss-Seidel matriu de Gauss-Seidel
# D, L, U ja s'han definit anteriorment
M = D - L
G = inv(M) %*% U

# Radi espectral - Valor absolut del valor propi màxim
max_autovalor <- max(abs(eigen(G)$values))
print("Radi espectral per a Gauss-Seidel:")
print(max_autovalor)

# Resolucio amb Jacobi, amb tolerància
nmax = 100
x0 = rep(0, 4)
tol = 1e-6
solJ = itersolve(A, b, x0, nmax, tol, method="Jacobi")
print("Solucio Jacobi:")
print(solJ)

# Resolució amb Jacobi, 100 iteracions
solJ = itersolve(A, b, x0, nmax=100, tol=1e-10, method="Jacobi") 
print("Solucio Jacobi:")
print(solJ)

# Acotació de l'error relatiu
norm(inv(A), "2") * norm(A, "2") * norm((A %*% solJ$x - b), "2") / norm(b, "2") 

# Resolucio amb Gauss-Seidel, amb tolerància
solG = itersolve(A, b, x0, nmax, tol, method="Gauss-Seidel")
print("Solucio Gauss-Seidel:")
print(solG)

# Resolucio amb Gauss-Seidel, 100 iteracions
solG = itersolve(A, b, x0, nmax=100, tol=1e-20, method="Gauss-Seidel")
print("Solucio Gauss-Seidel:")
print(solG)

# Acotació de l'error relatiu
norm(inv(A), "2") * norm(A, "2") * norm((A %*% solG$x - b), "2") / norm(b, "2") 

# Comprovacio
x_ = solve(A, b)
sprintf(x_, fmt = '%#.4f')
