# PAC 3 - Pràctica - Intervals de confiança

# Dades
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac3/practica/"
file_path <- paste(project_path, "BBDD.csv", sep="")
dadesPM10 <- read.table(file_path, header = TRUE, sep = ";",
  na.strings = "NA", fileEncoding = "UTF-8", quote = "\"",
  colClasses = c(rep("character", 4), rep("numeric", 2), rep("character", 2)))

# PREGUNTA 1

# Com que no coneixem la desviació típica poblacional, utilitzarem
# una distribució t de Student per crear l'interval de confiança
conf_level <- 0.95

# a) Interval 95% per la mitjana de PM10Concentration1999 - Low income
PM10_low_income <- subset(dadesPM10, IncomeGroup == "Low income")$PM10Concentration1999
t.test(PM10_low_income, conf.level = conf_level)[[4]]

# b) Interval 95% per la mitjana de PM10Concentration1999 - High income
PM10_high_income <- subset(dadesPM10, IncomeGroup == "High income")$PM10Concentration1999
t.test(PM10_high_income, conf.level = conf_level)[[4]]

# c) Conclusions sobre la concentració de PM10 per als dos nivells d’ingressos
length(PM10_low_income)
mean(PM10_low_income)
sd(PM10_low_income)
length(PM10_high_income)
mean(PM10_high_income)
sd(PM10_high_income)
# La mitjana és més del doble als països amb ingresos baixos
# per tant hi ha molta més contaminació
# L'interval és molt més petit per als països amb ingressos alts,
# això és degut a que els països amb ingresos alts tenen deu vegades més d'observacions
# i també a que els països amb ingressos baixos tenen més variabilitat en les dades

# d) Mida mínima de la mostra amb ME < 0.1 - High income
n <- length(PM10_high_income)
critical_value <- qt(p = 1 - (1 - conf_level) / 2, df = n - 1)
sd <- sd(PM10_high_income)
me <- 0.1
min_sample_size <- round(critical_value^2 * sd^2 / me^2)
min_sample_size

# PREGUNTA 2 - Proporció de ciutats amb PM10 > 50µg/m^3
conf_level <- 0.90

# a) Interval de confiança del 90% usant prop.test
successes <- nrow(subset(dadesPM10, PM10Concentration1999 > 50))
n <- nrow(dadesPM10)
ci <- prop.test(successes, n, conf.level = conf_level, correct = FALSE)[[6]]
ci

# b) Interval de confiança del 90% usant la fórmula
p <- successes / n
se <- sqrt(p * (1 - p) / n)
critical_value <- qnorm(1 - (1 - conf_level) / 2)
me <- critical_value * se
ci <- c(p - me, p + me)
ci

# c) Mida mínima de la mostra amb ME < 0.02 - Usant la mostra
me <- 0.02
min_sample_size <- round(critical_value^2 * (p * (1 - p) / me^2))
min_sample_size

# d) Mida mínima de la mostra amb ME < 0.02 - Sense info previa
# El valor de p*(1-p) en la pregunta anterior mai pot ser superior a 1/4,
# i això ens permet garantir el marge d'error desitjat,
# donat que no coneixem el valor real de la probabilitat de la població:
min_sample_size <- round(1/4 * (critical_value / me)^2)
min_sample_size



