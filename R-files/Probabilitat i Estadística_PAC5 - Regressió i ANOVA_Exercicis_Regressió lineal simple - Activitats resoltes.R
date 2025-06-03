# Regressió lineal simple - Activitats resoltes

# Dades
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac5/exercicis/"
file_path <- paste(project_path, "ActR10NBUF.csv", sep = "")
df <- read.table(file_path, sep = ";", dec = ",", header = TRUE)


# ACTIVITAT 1

# Model lineal 
lm_1 <- lm(df$Y ~ df$X1)
summary(lm_1) # Recta: Y = −0.06 + 2.0187X
# El coefficient de correlació lineal r és l'arrel quadrada de R²

# Gràfic
plot(df$Y ~ df$X1)
abline(lm_1)

# Diagrama de residus
plot(residuals(lm_1) ~ fitted.values(lm_1), main="Diagrama de residus",
     xlab="Valors estimats", ylab="Residus")
abline(0,0) # Línia horitzontal


# ACTIVITAT 2

# Dades
LON <- c(100,110,120,150,190,200,225,265,280,300)
TMP <- c(52,75,62,61,84,98,110,94,100,135)

# Calculem els paràmetres β₀ i β₁ del model lineal:
mean(TMP)
var(LON)
cov(TMP,LON)
B1 <- cov(LON,TMP) / var(LON)
B0 <- mean(TMP) - B1 * mean(LON)

# Calculem r i R²
r <- cov(LON,TMP) / sqrt(var(TMP) * var(LON))
R2 <- r^2


# ACTIVITAT 4
# Interval de confiança al 95% per al pendent de la recta

# Pendent β₁ (de l'enunciat)
beta1 <- 30.5

# Valor crític
t <- qt(0.025, (12-2), lower.tail=FALSE) # n-2 graus de llibertat

# Interval de confiança
c(beta1 - 2 * sqrt(82/39), beta1 + t * sqrt(82/39))

