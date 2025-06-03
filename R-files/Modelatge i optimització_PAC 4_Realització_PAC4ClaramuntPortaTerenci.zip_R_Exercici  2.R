library(linprog)

# Coeficients de la funció objectiu
FO <- c(x1=0.09, x2=0.04, x3=0.1, x4=0.05, x5=0.05)

# Restriccions
R1 <- list(c(x1=1, x2=1, x3=1, x4=1, x5=1), '<=', 100)
R2 <- list(c(x1=1, x2=0, x3=1, x4=0, x5=0), '<=', 50)
R3 <- list(c(x1=1, x2=0, x3=0, x4=0, x5=0), '>=', 10)
R4 <- list(c(x1=0, x2=1, x3=0, x4=0, x5=0), '>=', 25)
R5 <- list(c(x1=0, x2=0, x3=1, x4=0, x5=0), '>=', 10)
R6 <- list(c(x1=0, x2=0, x3=0, x4=1, x5=0), '>=', 10)
R7 <- list(c(x1=0, x2=0, x3=0, x4=0, x5=1), '>=', 10)

# Solució del model de programació lineal
CR <- rbind(R1[[1]], R2[[1]], R3[[1]], R4[[1]], R5[[1]], R6[[1]], R7[[1]])
DIR <- c(R1[[2]], R2[[2]], R3[[2]], R4[[2]], R5[[2]], R6[[2]], R7[[2]])
TI <- c(R1[[3]], R2[[3]], R3[[3]], R4[[3]], R5[[3]], R6[[3]], R7[[3]])
RES <- solveLP(FO, TI, CR, maximum=TRUE, const.dir=DIR)

# Mostra el resultat
print(RES)