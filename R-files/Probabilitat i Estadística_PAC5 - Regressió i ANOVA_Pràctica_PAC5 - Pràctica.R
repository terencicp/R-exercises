# Probabilitat i estadística - PAC 5

# Dades
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac5/practica/"
file_path <- paste(project_path, "data_pac5.csv", sep = "")
df <- read.table(file_path, sep = ",", dec = ".", header = TRUE)


# PREGUNTA 1

# a)
plot(df$amount_paid ~ df$housearea)
# El diagrama de dispersió no mostra cap tipus de relació entre les variables.
# El núvol de punts és dispers i no pren una forma tubular, com s'esperaria
# si hi hagués una relació lineal entre les variables.

# b)
lm_1 <- lm(df$amount_paid ~ df$housearea)
lm_summary <- summary(lm_1)
B0 <- round(lm_summary$coefficients[[1]], 3)
B1 <- round(lm_summary$coefficients[[2]], 3)
# Equació de la recta: y = 506.587 + 0.118x
print(paste0("y = ", B0, " + ", B1, "x"))

# c)
summary(lm_1)$r.squared
# El valor del coefficient de correlació ens indica que només un 0.92%
# de la variabilitat de amount_paid és explicada per la variable housearea
# Un R^2 de 0 ens indica que no hi ha relació entre les variables i 
# un R^2 de 1 ens indica que l'ajust del model és perfecte.
# En aquest cas el coefficient de correlació és molt proper a 0,
# cosa que ens indica de l'ajust del model és molt dolent


# PREGUNTA 2

# Transformació de la variable a factor
df$num_children <- as.factor(df$num_children)

# Calcula el model ANOVA
anova <- aov(amount_paid ~ num_children, data = df)

# Mostra el resum del model
summary(anova)


# PREGUNTA 3

# Transformació de la variable a factor
df$is_urban <- as.factor(df$is_urban)

# a)
lm_3 <- lm(amount_paid ~ num_rooms + ave_monthly_income + is_urban, data=df)
summary(lm_3)
B0 <- round(summary(lm_3)$coefficients[[1]], 3)
B1 <- round(summary(lm_3)$coefficients[[2]], 3)
B2 <- round(summary(lm_3)$coefficients[[3]], 3)
B3 <- round(summary(lm_3)$coefficients[[4]], 3)

coefficients <- round(summary(lm_3)$coefficients, 3)[1:4]
# Equació de la recta: y = 347.923 + 0.101 x1 + 0.001 x2 + 241.293 x3
print(paste0("y = ", B0, " + ", B1, " x1 + ", B2, " x2 + ", B3, " x3"))

# b)
# Podem fer un contrast d'hipòtesi per a cada paràmetre el model,
# amb les següents hipòtesis:
# Per a cada Bi:
# La hipòtesi nula és H0: Bi = 0 (la variable Xi no és explicativa)
# La hipòtesi alternativa és H1: Bi ≠ 0
# Aplicant la funció summary() al model de regressió lineal múltiple,
# podem obtenir el p-valor del contrast d'hipòtesi per a cada paràmetre,
# la probabilitat d'obtenir aquest resultat o un més extrem
# donat que l'ha hipòtesi nula és certa,
# que es pot veure a l'apartat anterior sota la columna "Pr(>|t|)"
# Segons el nivell de significació escollit del 5%, podem observar que
# per a tots els paràmetres el p-valor, és inferor al 0.05 del nivell
# de significació, per tant hem de rebutjar la hipòtesi nula i considerarem
# que totes les variables contribueixen a explicar el valor de la variable 
# amount_paid

# c)
# Segons la informació que summary() ens proporciona del model tenim que 
# el p-valor per al contrast d'hipòtesi conjunt per a totes les variables
# del model és inferior a 2.2e-16, cosa que amb un nivell de significació
# de 0.05 ens indica que com a mínim una de les variables és explicativa.
# El valor de R^2 0.4314 ens indica que el model explica un 43.14% de la
# variabilitat de amount_paid. Es podria fer un model usant altres variables
# per intentar aconseguir un coefficient de correlació més alt i, per tant,
# un model amb un millor ajust.
# RESIDUALS PLOT ? ??? ?? ?? ? ? ?? ??

# d)

# Manual 
print(434.176 - 2.409*7 + 0.001*2000 + 241.924*0)

num_rooms <- c(7)
ave_monthly_income <- c(2000)
is_urban <- c(0)

paco <- data.frame(num_rooms, ave_monthly_income, is_urban)

predict(lm_3, paco)[[1]]
