# Anàlisi de dades i estadística descriptiva
# Part 1 - Estadística descriptiva amb R


### DADES ###

# Fruites (p7)
pera <- c(2,5,4,7,6,9,8,5,2,4)
kiwi <- c(7,9,5,8,5,1,3,2,4,7)
fru <- cbind(pera, kiwi)

# Animals (p7)
gos <- c(7,2,7,3,6,5,2,1,4)
gat <- c(8,6,8,2,2,4,7,8,6)
lloro <- c(7,2,6,5,2,3,9,7,4)
animal <- cbind(gos, gat, lloro)

# CSV (p9)
setwd("~/Desktop/UOC/Data") # Set working directory (p9)
test <- read.table("1 Estadística/NCA.csv",
                   header = TRUE,
                   sep = ";",
                   dec =",",
                   skip = 3,
                   fileEncoding = "UTF-8")

# Dades generades (p13)
set.seed(30)
al_norm <- rnorm(100, mean=10, sd=3)


### ANÀLISI ESTADÍSTIC ###

# Resums (p9)
summary(gos) # One variable
summary(animal) # Multiple variables

# Resums + SD --> Matrix (p10)
nou_res <- rbind(c(
  summary(gos), sd=sd(gos)), c(
  summary(gat), sd=sd(gat)), c(
  summary(lloro), sd=sd(lloro
  )))

row.names(nou_res) <- c("gos","gat","lloro")

# Agrupar variables + Columna factor (p10)
df <- stack(list(Gat = gat, Gos = gos, Lloro = lloro))
colnames(df) <- c("total", "classe")

# Freqüències del factor (p11)
table(df$classe) # absoluta
prop.table(table(df$classe)) * 100 # relativa %

# Mitjana per factors (p12)
tapply(df$total, list(classe=df$classe), mean, na.rm=TRUE)


### VISUALITZACIÓ ###

# Histograma (p12)
hist(gat) 
hist(al_norm, freq=FALSE) # Histograma de densitat
hist(al_norm, breaks=20) # Nombre de barres
hist(al_norm, breaks=c(0, 5, 10, 15, 20)) # Punts de canvi de classe

# Diagrama de barres (p15)
plot(df$classe)

# Diagrama de caixa (p15)
boxplot(gat)
boxplot(df$total ~ df$classe) # Per factors

# Diagrama de dispersió (p16)
plot(lloro, gos)
abline(lm(gos ~ lloro), col="red")

# Transformació de variables (p17)
mamifers <- gat + gos
calculate_z_score <- function(x) (x - mean(x)) / sd(x)
z_gat <- calculate_z_score(gat)
z_lloro <- calculate_z_score(lloro)
animal <- cbind(animal, z_gat, z_lloro)


