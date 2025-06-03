# 22.426 - Aplicacions per a la presa de decisions
# Repte 1
# Terenci Claramunt Porta

# Instruccions d'execució:
# Executar l'script sencer o el bloc corresponent a cada pregunta
# Instal·lar les llibreries següents si es necessari:
# install.packages("lpSolveAPI")
# install.packages("SCperf")
# install.packages("readxl")
# install.packages("geosphere")
# install.packages("TSP")
# install.packages("ROI")
# install.packages("ROI.plugin.glpk")

# El codi està descrit detalladament al document PDF adjunt


# PREGUNTA 1

# Reinicia l'entorn
rm(list = ls())
library(lpSolveAPI)
# Definició del problema
lprec <- make.lp(0, 3)
out <- lp.control(lprec, sense = "max")
set.type(lprec, columns = 1, type = "integer")
set.type(lprec, columns = 2, type = "integer")
set.type(lprec, columns = 3, type = "integer")
set.objfn(lprec, c(10000, 20000, 18000))
add.constraint(lprec, c(100000, 120000, 3000), "<=", 500000)
add.constraint(lprec, c(0, 1, 0), "<=", 50)
add.constraint(lprec, c(-1, -1, 1), "<=", 0)
add.constraint(lprec, c(9, -1, -1), ">=", 0)
add.constraint(lprec, c(0, 1, 0), ">=", 0)
add.constraint(lprec, c(0, 0, 1), ">=", 0)
# Solució
out <- solve(lprec)
print("Nombre òptim d'anuncis per canal:")
get.variables(lprec)
print("Audiència que aconseguirà la campanya:")
get.objective(lprec)


# PREGUNTA 3

# Reinicia l'entorn
rm(list = ls())
library(SCperf)
# Solució
eoq <- EOQ(11200, 100, 0.3)
print("3.1 EOQ:")
sprintf("%.2f", eoq[1]) 
print("3.2 Nombre de comandes anuals:")
sprintf("%.4f", 1 / eoq[2])
print("3.3 Dies entre comandes:")
sprintf("%.2f", 240 * eoq[2])
print("3.4 Cost total de comandes i manteniment anual:")
sprintf("%.2f", eoq[3])


# PREGUNTA 5

# Reinicia l'entorn
rm(list = ls())
library(readxl)
library(geosphere)
library(TSP)
# Dades
col_types <- c("text", "numeric", "numeric")
capitals <- read_excel("Repte 1 - Terenci Claramunt Porta.xlsx", col_types = col_types)
# Matriu de distàncies
distances <- distm(capitals[, c("lon", "lat")], fun = distHaversine) / 1000
rownames(distances) <- colnames(distances) <- capitals$capital
tsp <- TSP(distances, method = "haversine")
# Millor algoritme en 100 iteracions
score <- c(NNA = 0, RNNA = 0, CIA = 0)
solve_length <- function(method) tour_length(solve_TSP(tsp, method))
for (i in 1:100) {
    best_algorithm <- names(which.min(c(
        NNA = solve_length("nn"),
        RNNA = solve_length("repetitive_nn"),
        CIA = solve_length("cheapest_insertion")
    )))
    score[best_algorithm] <- score[best_algorithm] + 1
}
# Diagrama
barplot(score, main = "Millor algoritme en 100 iteracions")
# Ruta
tour <- solve_TSP(tsp, "cheapest_insertion")
cut_tour(tour, cut = c("A Coruña", "Alacant"), exclude_cut = FALSE)


# PREGUNTA 6

# Reinicia l'entorn
rm(list = ls())
library(ROI)
library(ROI.plugin.glpk)
# Plantejament
lop <- OP(
    maximum = TRUE,
    objective = c(50, 200),
    type = c("I", "I"),
    constraints = L_constraint(
        L = rbind(c(3, 7), c(0.5, 0.9)),
        dir = c("<=", "<="),
        rhs = c(240, 60)
    ),
    bounds = V_bound(lb = c(20, 0), ub = c(Inf, 30))
)
# Solució
lop_solved <- ROI_solve(lop, "glpk")
solution(lop_solved)
lop_solved






