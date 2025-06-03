# Estadística descriptiva - Selecció d’activitats resoltes


# Activitat 1: còmput del temps de CPU

# a) Data
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac1/dadesrepte1/"
file_path <- paste(project_path, "ActR01TCPU.csv", sep="")
temps <- read.table(file_path, skip=3, sep=";", header=TRUE)

# c) Stats
stats <- summary(temps)
sd1 <- sd(temps$TCPU)
sd2 <- sd(temps$LCOD)

# d-e) Plots
hist(temps$TCPU)
boxplot(temps$TCPU)


# Activitat 2: còmput del temps de CPU agrupat

# Agrupament
clt <- cut(temps$TCPU, breaks=c(48,81,114,149),
           labels=c('TC','T','TL'), include.lowest = TRUE)

# Resum numéric
table(clt)

# Plot
pie(table(clt))


# Activitat 3: immersió de les TIC en els municipis
file_path <- paste(project_path, "ActR01TICM.csv", sep="")
data <- read.table(file_path, skip=4, sep=";", dec = ",", header=TRUE)
tic <- data.frame(data$PUORD, data$PUSUA) # Remove unused columns
names(tic) <- c("PUORD", "PUSUA") # Rename columns

# Stats - Adding Standard Deviation to summary
summary_statistics <- rbind(
  c(summary(tic$PUORD), SD=sd(tic$PUORD)),
  c(summary(tic$PUSUA), SD=sd(tic$PUSUA))
)
rownames(summary_statistics) <- c("PUORD", "PUSUA")

# Histograms
par(mfrow=c(2,1)) # 2 rows
# The x range is the same for both variables
hist(tic$PUORD, xlim = c(30, 80), main="PUORD")
hist(tic$PUSUA, xlim = c(30, 80), main="PUSUA")

# Box plots
par(mfrow=c(1,1))
# The y range is the same for both variables
boxplot(tic$PUORD, tic$PUSUA, names=c("PUORD", "PUSUA"))





