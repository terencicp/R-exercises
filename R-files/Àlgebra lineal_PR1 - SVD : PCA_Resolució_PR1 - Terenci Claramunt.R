# Àlgebra lineal - Pràctica 1 - SVD i PCA

# Required library:
# install.packages("fields")

# Dades
setwd("~/Desktop/UOC/Data")
project_path <- "1 Algebra lineal/pr1/"
file_path1 <- paste(project_path, "TADPOLE_BASELINE.csv", sep = "")
file_path2 <- paste(project_path, "TADPOLE_BASELINE_demo.csv", sep = "")
# Percentatge de volum de cada àrea del cervell per cada subjecte
volumes <- read.csv(file_path1)
# Edats i diàgnositic (1=Alzheimer, 0=sa) dels mateixos subjectes
demo <- read.csv(file_path2)

##########################

# PREGUNTA 1

# Crea dos vectors a partir del dataframe demo
age <- demo$AGE
dx <- demo$DX

# Comprova que la classe dels vectors és "numeric"
class(age)
class(dx)

# Converteix el dataframe volumes en matriu (eliminant la columna "Subject")
W <- as.matrix(volumes[-1])

# Comprova que la classe de la matriu és "matrix" "array"
class(W)

# El nombre de subjectes observats és el nombre de files de la matriu:
dim(W)[1]

##########################

# PREGUNTA 2

# Podem buscar els pacients més gran i més jove usant l'índex
# del valor màxim i mínim de "age"
# El pacient més gran:
demo[which.max(age),]
# El pacient més jove:
demo[which.min(age),]

# Nombre de subjectes amb Alzheimer (DX=1) i de subjectes sans (DX=0)
c(sum(dx == 1), sum(dx == 0))

# Regió del cervell més gran i més petita en mitjana de volum de matèria gris:
# Calcula la mitjana per cada regió
means <- colMeans(volumes[-1])
# Extreu el nom del la regió amb la mitjana màxima i mínima
largest <- names(which(means == max(means)))
smallest <- names(which(means == min(means)))
# Mostra el resultat
c(largest, smallest)

##########################

# PREGUNTA 3

# Podem normalitzar W usant la fòrmula:
# LaTeX: x = x-µ/s (pàgina 12)

# Copia la matriu W a la variable Ws
Ws <- W
# Itera sobre cada columna de la matriu W
for(col in 1:ncol(W)) {
  # Calcula la mitjana de la columna actual
  col_mean <- mean(W[,col])
  # Calcula la desviació estàndard de la columna actual
  col_sd <- sd(W[,col])
  # Itera sobre cada fila de la columna col de la matriu W
  for(row in 1:nrow(W)) {
    # Escala el valor actual
    scaled = (W[row,col] - col_mean) / col_sd
    # Guarda el valor escalat a Ws
    Ws[row, col] = scaled
  }
}

##

# Calcula la matriu de covariàncies aplicant la fòrmula
# LaTeX: 1/n-1 XT X (pàgina 16)

# Calcula el nombre d'observacions n
n <- dim(W)[1]
# Aplica la fòrmula
CWs <- (t(Ws) %*% Ws) / (n - 1)

##

# Per trobar la covariància entre "LeftAmygdala", "RightThalamus",
# busquem l'element de la fila "LeftAmygdala" i la columna "RightThalamus"
CWs["LeftAmygdala", "RightThalamus"]

##########################

# PREGUNTA 4

# Carrega la llibreria i dibuixa la matriu de covariàncies
library("fields")
image.plot(CWs[,nrow(CWs):1])

##

# 1) CALCULA LA MITJANA DE LES DIAGONALS SUPERIORS (MATRIU SIMÈTRICA)

# Nombre de columnes de la matriu
ncol <- ncol(CWs)
# Vector de longitud fixa
diagonals <- vector(mode = "numeric", length = ncol)
# Per cada columna de la matriu
for(col in 1:ncol) {
  # Inicia la variable a 0
  sum <- 0
  # Vector que conté la posició actual
  position <- c(1, col)
  # Nombre de files de la diagonal que comença a la columna actual
  nrow <- nrow(CWs) - col + 1
  # Per cada fila de la diagonal
  for(row in 1:nrow) {
    # Suma l'element actual a "sum"
    sum <- sum + CWs[position[1], position[2]]
    # Assigna la posició del següent element de la diagonal
    position <- c(position[1] + 1, position[2] + 1)
  }
  # Guarda la mitjana de la covariància a "diagonals"
  diagonals[col] <- sum / nrow
}

# 2) BUSCA LA COLUMNA ON COMENÇA LA DIAGONAL SECUNDÀRIA

# Valor màxim de les mitjanes de les diagonals (excepte la diagonal principal)
second_diagonal_value <- max(diagonals[-1])
# Índex de l'element amb el valor anterior
second_diagonal_start <- which(diagonals == second_diagonal_value)

# 3) BUSCA LES AREES AMB LA COVARIÀNCIA MÀXIMA DE LA DIAGONAL SECUNDÀRIA

# Nombre de files de la diagonal secundària
nrows <- nrow(CWs) - second_diagonal_start + 1
# Elements de la diagonal secundària
second_diagonal <- vector(mode = "numeric", length = nrows)
# Posició actual
position <- c(1, second_diagonal_start)
# Per cada fila de la diagonal
for(row in 1:nrows) {
  # Guarda la covariància a "second_diagonal"
  second_diagonal[row] <- CWs[position[1], position[2]]
  # Assigna la posició del següent element de la diagonal
  position <- c(position[1] + 1, position[2] + 1)
}
# Valor absolut màxim de la diagonal secundària
second_diagonal_max_value <- max(abs(second_diagonal))
# Índex del valor absolut màxim de la diagonal secundària
second_diagonal_max_i <- which(second_diagonal == second_diagonal_max_value)
# Àrees amb la covariància màxima
rownames(CWs)[second_diagonal_max_i]
colnames(CWs)[second_diagonal_start + second_diagonal_max_i - 1]

##

# A partir del resultat anterior podem deudir que la diagonal secundària
# conté les covariàncies de cada regió cerebral de l'hemisferi esquerre
# amb la regió corresponent de l'hemisferi dret. Podem observar que
# la diagonal secundària conté la meitat d'elements que variables té la matriu W
# cosa que reforça aquesta hipòtesi.

# Columnes de la matriu W / Elements de la diagonal secundària
dim(W)[2] / length(second_diagonal)

##

# Les regions amb covariància negativa amb la resta, corresponents a les lines
# horitzontals i verticals són:

# Observacions de la 1a fila
first_row_covariance <- CWs[1,]
# Noms de les variables amb covariància negativa
names(which(first_row_covariance  < 0))

##########################

# PREGUNTA 5

# Podem calcular els valors propis usant eigen()
eigenvalues <- eigen(CWs)$values
# Mostra els valors propis
eigenvalues
# La suma dels valors propis és igual a la traça de la matriu de covariàncies
eigenvalues_sum <- sum(eigenvalues)
# Per tant es pot calcular la proporció d'informació que conté
# cada component principal
pc_variance_prop <- eigenvalues / eigenvalues_sum
pc_variance_prop
# I comprovar que la suma del total és 1
sum(pc_variance_prop)
# a) Proporció de la variància explicada per cada component principal
plot(pc_variance_prop,
  main="Proporció de variància",
  xlab="Components principals",
  ylab="Proporció de variància explicada", ylim=c(0,0.5)
)
# b) Proporció de la variància acumulada explicada pels components principals
plot(cumsum(pc_variance_prop),
  main="Proporció de variància acumulada",
  xlab="Components principals",
  ylab="Proporció de variància explicada acumulada", ylim=c(0,1)
)
# Es pot observar que la primera componenent principal explica gran part
# de la proporció de la variància:
pc_variance_prop[1]

##########################

# PREGUNTA 6

# Podem calcular els vectors propis usant la següent instrucció,
# que ens dona la matriu que conté els vectors propis per cada valor propi
P <- eigen(CWs)$vectors
# Només hem de multiplicar Ws per aquesta matriu per obtenir la projecció de
# les dades al nou espai de components principals
T <- Ws %*% P
# Calcula la matriu de covariàncies
CT <- cov(T)
# Dibuixa la matriu de covariàncies
image.plot(CT[,nrow(CT):1])

# A la imatge podem observar que es covariàncies fora de la diagonal principal
# són 0 o properes a 0, per tant la seva suma serà 0 o propera a 0

# Es pot comprovar sumant tots valors de la matriu i restant els de la diagonal
round(sum(CT) - sum(diag(CT)), 5)

##########################

# PREGUNTA 7

# Seleccionem els 20 primers components principals
P_20 <- P[,1:20]
# Projecció a les dades a l'espai dels 20 primers components principals
T_20 <- Ws %*% P_20
# Inversió de la projecció per obtenir les dades originals amb pèrdua d'informació
Ws_20 <- T_20 %*% t(P_20)
# Calcula l'error residual
E <- Ws - Ws_20
# Desviació típica de l'error residual
sd(E)

##########################

# PREGUNTA 8

# Seleccionem els 2 primers components principals
P_2 <- P[,1:2]

# Diagrama de dispersió
par(mfrow=c(1,1), cex=0.9)
plot(P_2[,1], P_2[,2],
     xlab = "1r component principal",
     ylab = "2n component principal",
)

# Calculem el valor del primer i el segon component principals
nrows <- nrow(Ws)
pc_1 <- vector(mode = "numeric", length = nrows)
pc_2 <- vector(mode = "numeric", length = nrows)
for(row in 1:nrows) {
  pc_1[row] <- sum(Ws[row,] * P[,1])
  pc_2[row] <- sum(Ws[row,] * P[,2])
}

# Usem la variable dx per dividir les dades entre casos d'Alzheimer i sans
pc_1_alzheimer <- pc_1[dx == 1]
pc_1_healthy <- pc_1[dx == 0]
pc_2_alzheimer <- pc_2[dx == 1]
pc_2_healthy <- pc_2[dx == 0]

# Mitjana de la 1a component principal per casos d'Alzheimer
mean(pc_1_alzheimer)
# Mitjana de la 1a component principal per casos sans
mean(pc_1_healthy)

# Mitjana de la 2a component principal per casos d'Alzheimer
mean(pc_2_alzheimer)
# Mitjana de la 2a component principal per casos sans
mean(pc_2_healthy)

# poden ser útils per discriminar els pacients amb Alzheimer dels sans
# tenint en compte solament el volum de materia gris de les regions cerebrals?

# Les dos primeres components principals podrien ser útils per distingir els
# pacients sans dels pacients amb Alzheimer ja que podem observar que per cada
# component els dos grups mostren mitjanes amb signes oposats.


