# Àlgebra lineal - PR2 - 6/6/2022

# PREGUNTA 1

# Diagrama de transició:
# diagrama.jpeg

#------------

# PREGUNTA 2

# Crea la matiu de transició
states <- c('RR', 'RB', 'BB')
p <- c(
  0.5,  0.5, 0, 
  0.25, 0.5, 0.25,
  0,    0.5, 0.5
)
P <- matrix(p, nrow=3, byrow=TRUE, dimnames=list(states,states))

# Sumes de les files
print(rowSums(P))

# Sumes de les columnes
print(colSums(P))

#------------
  
# PREGUNTA 3

# Carrega la llibreria
library("markovchain")

# Crea la cadena de Markov
mcP <- new("markovchain", states=states, transitionMatrix=P, name="Rosa/Rosae")

# Dibuixa la cadena de Markov
plot(mcP, package="diagram")

#------------

# PREGUNTA 4

# Valors donats
pRR <- 69
pRB <- 7
pBB <- 24
# Vector d'estat inicial
q <- c(pRR, pRB, pBB) / 100

# Distribució de genotips a la següent generació
print(q %*% P * 100)

# Funció per calcular probabilitats a partir d'un estat inicial
state_probabilities <- function(initial_state, transition_matrix, iterations) {
  p <- initial_state %*% transition_matrix
  for (i in 2:iterations) {
    p <- p %*% transition_matrix
  }
  return(p)
}

# Distribució de genotips a l'n-èsima generació
n <- 9
print(state_probabilities(q, P, n) * 100)

# Distribució de genotips a la 20a generació
print(state_probabilities(q, P, 20) * 100)

#------------

# PREGUNTA 5

# Valors d'a i b donats
a <- 0.21
b <- 0.09

# La distribució de probabilitats ha de sumar 1, per tant:
c <- 1 - a - b
print(c)

# Vector d'estat inicial
q1 <- c(a, b, c)

# Distribució de genotips a l'n-èsima generació
print(state_probabilities(q1, P, n) * 100)

# Distribució de genotips a la 20a generació
print(state_probabilities(q1, P, 20) * 100)

# Els resultats de les preguntes 4 i 5 són molt similars ja que a llarg plaç
# les probabilitats d'estar en un determinat estat depenen, no de l'estat inicial,
# sino  de la probabilitat de passar de RB a un altre estat i viceversa, ja que
# la probabilitat de romandre en qualsevol dels estats és la mateixa i no hi ha
# cap estat absorbent

# Com hem observat a la pregunta 5, la
# distribució estacionaria de mcP tendeix a (0.25, 0.5, 0.25)


#------------

# PREGUNTA 7

# Crea la matiu de transició
pq <- c(
  0, 1,   0, 
  0, 0.5, 0.5,
  0, 0,   1
)
Q <- matrix(pq, nrow=3, byrow=TRUE, dimnames=list(states,states))

# Sumes de les files
print(rowSums(Q))

# Sumes de les columnes
print(colSums(Q))

# Crea la cadena de Markov
mcQ <- new("markovchain", states=states, transitionMatrix=Q, name="Rosa/Rosae")

# Dibuixa la cadena de Markov
plot(mcQ, package="diagram")


#------------

# PREGUNTA 8

# Hi ha una potència positiva de la matriu de transició amb totes les entrades
# estrictament més grans que 0, per tant, la cadena de Markov mcP és regular.

# Mínima potència de P > 0
min_power <- 1
# Matriu P multiplicada per si mateixa a cada iteració
PP <- P
# Potència a la que s'eleva P
i <- 2
# Mentre no s'hagi trobat la mínima potència
while (min_power == 1) {
  # Multiplica P per si mateixa iterativament
  PP <- PP %*% P
  # Si tots els valors de P són més grans que 0
  if(all(PP > 0)) {
    # Guarda el valor de l'exponent
    min_power <- i
  }
  # Augmenta l'índex
  i <- i + 1
}
# Mostra el resultat
print(min_power)

#--

# Com que la primera columna de Q està formada per zeros, al multiplicar Q
# per si mateixa seguirà estant formada per zeros, per tant, no hi ha cap
# potència que faci que els elements de la matriu siguin majors de zero i 
# la matriu no és regular.

print(Q %*% Q)

#------------

# PREGUNTA 9

# a)

# A mcP tots els estats són recurrents ja que la probabilitat de tornar a
# qualsevol dels estats és 1.

# A mcQ tots els estats són transitoris excepte BB, que es un estat absorbent.
# Això és deu a que la probabilitat de romandre a BB és 1 mentre que la
# probabilitat de romandre a qualsevol altre estat és menys d'1.

# b)

# Sabem que mcP és regular i, per tant, té una única distribució estacionaria:
v <- eigen(t(P))$vectors[,1]
stationary_mcP <- v / sum(v)
names(stationary_mcP) <- states
stationary_mcP

# En el cas de mcQ tindrem una única distribució estacionaria ja que BB és un
# estat absorbent i tots els altres estats s'hi comuniquen unidireccionalment.
# El seu estat estacionari és:
v <- eigen(t(Q))$vectors[,1]
stationary_mcP <- v / sum(v)
names(stationary_mcP) <- states
stationary_mcP

