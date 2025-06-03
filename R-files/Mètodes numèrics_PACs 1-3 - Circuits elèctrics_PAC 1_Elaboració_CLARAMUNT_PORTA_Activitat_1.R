### PEC 1

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
# Completar la matriu del sistema
A = matrix(c(1,     1,    0,    1, 
			       0,     R_I2, 0,    -R_25, 
			       -R_12, R_I2, R_56, 0,
			       0,     -1,   1,    -1), nrow=4, byrow=TRUE)
# Definir el costat dret
b = c(0, 0, 150, 0)


## Exercici 1.2

# Comprovar existencia factoritzacio LU calculant els menors principals de A
# Implementar un codi que calculi els menors principals i comprovi que no son zero

# Inicia el vector que contindrà els valors dels menors principals
menors_principals <- c(0, 0, 0, 0)

# Per cada número de fila k (de 1 a 4)
for (k in 1:nrow(A)) {
  # Genera la submatriu eliminant les últimes k files i columnes
  submatriu_principal <- matrix(A[1:k, 1:k], nrow=k, byrow=TRUE)
  # Guarda el valor del determinant de la submatriu
  menors_principals[k] <- det(submatriu_principal)
}

# Mostra TRUE si no hi ha cap menor principal igual a 0
# i, per tant, la matriu A admet descomposició LU
print(!(0 %in% menors_principals))


# Factoritzacio LU
mlu = lu(A, scheme = 'ijk') # metode de Doolittle
L = mlu$L
U = mlu$U
print(L)
print(U)


# Resolucio del sistema a partir de L i U
y = solve(L, b) # resoldre Ly = b
x = solve(U, y) # resoldre Ux = y
print("Solucio del sistema. Intensitats:")
sprintf(x, fmt = '%#.4f')

# Comprovació
x_ = solve(A, b) # resoldre Ax_ = b
sprintf(x_, fmt = '%#.4f')

