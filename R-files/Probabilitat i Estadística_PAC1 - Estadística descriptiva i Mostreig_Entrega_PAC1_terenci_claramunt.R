# PAC 1

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac1/practica/"
file_path <- paste(project_path, "data_pac1.csv", sep="")
df <- read.table(file_path, sep=",", header=TRUE)

# Pregunta 1
# Summary + Mode + SD
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
stats1 <- summary(df$electronic_equipment)
stats1["Mode"] <- mode(df$electronic_equipment)
stats1["Range"] <- max(df$electronic_equipment) - min(df$electronic_equipment)
stats1["IQR"] <- stats1[5] - stats1[2]
stats1["SD"] <- round(sd(df$electronic_equipment), 2)
stats1
# Histogram + Box plot
par(mfrow=c(1,2), cex=0.5)
hist(df$electronic_equipment, xlim = c(0, 60), main="Histograma", ylab="Freqüència relativa", xlab="% de compres per internet")
boxplot(df$electronic_equipment, ylim = c(0, 60), horizontal=TRUE, main="Diagrama de caixa", xlab="% de compres per internet")

# Pregunta 2
par(mfrow=c(1,1))
table2_absolute <- table(df$sex)
table2_relative <- table2_absolute / sum(table2_absolute)
table2_absolute

library(RColorBrewer)
color <- brewer.pal(5, "Set2")
barplot(table2_relative, main="Diagrama de barres", xlab="Sexe", ylab="Freqüència relativa", col=color[2])

# Pregunta 3

# Summary by sex
stats3 <- tapply(df$electronic_equipment, df$sex, summary)
stats3_female <- stats3[[1]]
stats3_male <- stats3[[2]]
# Range
stats3_female["Range"] <- stats3_female[6] - stats3_female[1]
stats3_male["Range"] <- stats3_male[6] - stats3_male[1] 
# IQR
stats3_female["IQR"] <- stats3_female[5] - stats3_female[2]
stats3_male["IQR"] <- stats3_male[5] - stats3_male[2] 
# SD
stats3_sd <- tapply(df$electronic_equipment, df$sex, sd)
stats3_female["SD"] <- stats3_sd[[1]]
stats3_male["SD"] <- stats3_sd[[2]]
# Echo
print("Females")
round(stats3_female, digits = 2)
print("Males")
round(stats3_male, digits = 2)
# Data frame by sex
df_male <- df[df$sex == "Males", ]
df_female <- df[df$sex == "Females", ]
par(mfrow=c(2,1))
hist(df_female$electronic_equipment, xlim = c(0, 60), breaks = 7, main="Females", ylab="Freq. relativa", xlab="Percentatge de compres per internet")
hist(df_male$electronic_equipment,   xlim = c(0, 60), breaks = 7, main="Males", ylab="Freq. relativa", xlab="Percentatge de compres per internet")
par(mfrow=c(1,1))
boxplot(df_male$electronic_equipment, df_female$electronic_equipment, names=c("Males", "Females"), ylim = c(0, 60), horizontal=TRUE)


df_male <- df[df$sex == "Males", ]
df_female <- df[df$sex == "Females", ]
par(mfrow=c(2,1), cex=0.75)
hist(df_female$electronic_equipment, xlim = c(0, 60), breaks = 7, main="Females", ylab="Freq. relativa", xlab="% de compres per internet", col=color[1])
hist(df_male$electronic_equipment,   xlim = c(0, 60), breaks = 7, main="Males", ylab="Freq. relativa", xlab="% de compres per internet", col=color[2])

par(mfrow=c(1,1), cex=0.75)
boxplot(df_male$electronic_equipment, df_female$electronic_equipment, names=c("Males", "Females"), ylim = c(0, 60), horizontal=TRUE, col=c(col=color[2], col=color[1]))


# Pregunta 4
tha_min <- min(df$travel_and_holiday_accommodation)
tha_max <- max(df$travel_and_holiday_accommodation)
min_df <- df[df$travel_and_holiday_accommodation == tha_min, ]
max_df <- df[df$travel_and_holiday_accommodation == tha_max, ]
min_countries <- unique(min_df[,1])
max_countries <- max_df[,1]
c(paste("Mínim:", tha_min), min_countries)
c(paste("Màxim:", tha_max), max_countries)


