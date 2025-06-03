library(Rglpk)

# Coeficients de la funció objectiu
FO <- c(x1=3, x2=2, x3=5, x4=25, x5=18)

# Restriccions
R1 <- list(c(x1=3, x2=2, x3=4, x4=6, x5=5), '<=', 180)
R2 <- list(c(x1=0.13, x2=0.15, x3=0.09, x4=0.5, x5=0.85), '<=', 30)
R3 <- list(c(x1=1, x2=1, x3=1, x4=0, x5=0), '>=', 50)
TYPES <- c(x1='I', x2='I', x3='I', x4='C', x5='C')

# Resol el model de programació lineal
CR <- rbind(R1[[1]], R2[[1]], R3[[1]])
DIR <- c(R1[[2]], R2[[2]], R3[[2]])
TI <- c(R1[[3]], R2[[3]], R3[[3]])
RES <- Rglpk_solve_LP(FO, CR, DIR, TI, types=TYPES, max=TRUE)

# Mostra el resultat
print(RES)
