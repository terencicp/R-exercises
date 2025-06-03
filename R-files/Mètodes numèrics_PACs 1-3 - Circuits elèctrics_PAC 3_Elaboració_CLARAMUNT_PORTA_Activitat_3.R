### PAC 3

installed.packages("pracma")
library(pracma)

## Exercici 1.1
# Definim la matriu A del problema
# Completar la matriu A
A = matrix(c(1, -1, 0, 0, 
             -1, 2, -1, 0,
             0, -1, 2, -1,
             0, 0, -1, 2), nrow=4, byrow=TRUE)


# Autovalor mes gran i autovector associat (referencies per a el calcul d'errors)
eigA = eigen(A)
lambda1 = max(eigA$values)
print("Autovalor (referencia):")
print(lambda1)
v1 = matrix(eigA$vectors[,1])
print("Autovector (referencia):")
print(v1)

# Comprovacio matriu definida positiva
# Implementar un codi que calculi els menors principals i comprovi que tots siguin majors que zero

# Inicia el vector que contindrà els valors dels menors principals
menors_principals <- c(0, 0, 0, 0)

# Per cada número de fila k (de 1 a 4)
for (k in 1:nrow(A)) {
  # Genera la submatriu eliminant les últimes k files i columnes
  submatriu_principal <- matrix(A[1:k, 1:k], nrow=k, byrow=TRUE)
  # Guarda el valor del determinant de la submatriu
  menors_principals[k] <- det(submatriu_principal)
}

# Retorna FALSE si tots els menors principals són positius
# i per tant la matriu és definida positiva
any(menors_principals <= 0)

## Exercici 1.2

# Funció per al càlcul del mètode de la potència
power_method <- function(niter, x0) {
  xk = x0
  for (i in 1:niter){
    #Iteracio metode de la potencia
    xk = A %*% xk
    #Normalitzacio
    max_xk = max(xk)
    xk = xk / max_xk
  }
  return(list("value" = max_xk, "vector" = xk))
}

# 1.2.1 Calcul autovalor mes gran i autovector associat: Metode de la potència
x0 = matrix(c(1, 1, 1, 1)) #Iterant inicial
niter = 10
print(power_method(niter, x0))

# 1.2.2 Calcul de la major freqüència d'oscil·lació

#Resultats
power_results = power_method(niter = 10, x0 = c(0, 1, -4, 5))
lambdaP = power_results$value #Autovalor
print("Autovalor - Potencia:")
print(lambdaP)
print(power_results$vector)

print("Freqüència d'oscil·lacio - Potencia:")
print(sqrt(lambdaP * 2/3))

#Dades del circuit
L = 1/2
C = 3
wP = sqrt(1/L * 1/C * lambdaP) #Formula de la freqüència
print(wP)

# Calcul de l'error relatiu

print("Error Autovalor - Potencia:")
error_lambdaP = abs(lambda1 - lambdaP) / abs(lambda1) #Error relatiu en norma 1
print(error_lambdaP)

vP = power_results$vector #Autovector
vP =  vP/norm(xk, "2") #Ajust requerit per a l'autovector (no modificar)
print("Autovector - Potencia:")
print(vP)

# Correcció
vP = power_results$vector / norm(power_results$vector, "2")
print(vP)

print("Error Autovector - Potencia:")
error_vP = norm(v1 - vP, "1") / norm(vP, "1") #Error relatiu en norma 1
print(error_vP)


## Exercici 1.3
# Calcul autovalor mes gran i autovector associat: Metode de Rayleigh
x0 = matrix(c( 0, 1, -4, 5)) #Iterant inicial
niter = 4
I = diag(4) #Matriu identitat

xk = x0
for (i in 1:niter){
  #Quocient Rayleigh 
  sigma_i = t(xk) %*% A %*% xk / t(xk) %*% xk 
  sigma_i = sigma_i[1,1]
  #Iteracio potencia inversa
  xk = inv(A - sigma_i * I) %*% xk
  #Normalitzacio
  xk = xk / max(abs(xk))
}

#Resultats
lambdaR = sigma_i #Autovalor
print("Autovalor - Rayleigh:")
print(lambdaR)
vR = xk #Autovector
print("Autovector - Rayleigh:")
print(vR)

print("Freq?encia d'oscil?lacio - Rayleigh:")
#Dades del circuit
L = 1/2
C = 3
wR = sqrt(1/L * 1/C * lambdaR) #Formula de la freq?encia
print(wR)

print("Error Autovalor - Rayleigh:")
error_lambdaR = abs(lambda1 - lambdaR) / abs(lambdaR) #Error relatiu en norma 1
print(error_lambdaR)

vR = xk #Autovector
vR = vR/norm(xk, "2") #Ajust requerit per a l'autovector (no modificar)
print("Autovector - Rayleigh:")
print(vR)

print("Error Autovector - Rayleigh:")
error_vR = norm(v1 - vR, "1") / norm(vR, "1") #Error relatiu en norma 1
print(error_vR)


# Càlcul de l'error relatiu del mètode de la potència amb 4 iteracions

# Mètode de la potència usant la funció definida anteriorment
power_4 = power_method(niter = 4, x0 = c(0, 1, -4, 5))

print("Error Autovalor - Potència (4 iteracions):")
lambda4 = power_4$value
error_lambda4 = abs(lambda1 - lambda4) / abs(lambda4) #Error relatiu en norma 1
print(error_lambda4)

v4 = power_4$vector / norm(power_4$vector, "2") #Ajust requerit per a l'autovector (no modificar)
print("Error Autovector - Potència (4 iteracions):")
error_v4 = norm(v1 - v4, "1") / norm(v4, "1") #Error relatiu en norma 1
print(error_v4)

