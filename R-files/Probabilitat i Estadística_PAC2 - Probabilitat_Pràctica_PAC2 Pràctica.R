# PAC 2 - Pràctica

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac2/practica/"
file_path <- paste(project_path, "data_pac.csv", sep="")
df <- read.table(file_path, sep=",", header=TRUE)


# PREGUNTA 1

# New variable (Grouped using quartiles)
quantiles <- quantile(df$electronic_equipment, include.lowest = TRUE,
                     probs = c(0,0.25, 0.5, 0.75,1))
no_electronic <- cut(df$electronic_equipment, quantiles, 
    labels = c("grupo_1", "grupo_2", "grupo_3", "grupo_4"))
# a) Contingency table
table_sex_no_electronic <- addmargins(table(df$sex, no_electronic))
# b) 
p_females_grupo_2 <- 11/61
# c) 
p_males_given_grupo_3 <- 12/16
# d) 
p_pick3_grupo_2_no_replacement <- 16/61 * 15/60 * 14/59
# e) 3/4 = p of failure on one pick, ^6 = p of 6 failures, 1- = Complement
p_pick6_atleast1from_grupo_3 <- 1 - (3/4)^6


# PREGUNTA 2

geom_p <- 0.1
# a) Distribució gemomètrica
# b) 
p_0 <- dgeom(0, geom_p)
# c)
p_3_or_more <- pgeom(2, geom_p, lower.tail = FALSE)
# d)
n_95_percent_p <- qgeom(0.95, geom_p, lower.tail = TRUE)
# e) Mediana de simulacions
median_25 <- median(rgeom(25, 0.1))


# PREGUNTA 3

# a)
lambda <- 6^(-1)
var <- 1/(lambda^2)
# b)
p_garantia <- pexp(2, rate=1/6, lower.tail = TRUE)
sprintf("%0.1f%%", p_garantia * 100)
# c)
p_bat_mes_4 <- pexp(4, rate=1/6, lower.tail = FALSE)
# d)
p_5 <- pexp(5, rate=1/6, lower.tail = FALSE)
p_9 <- pexp(9, rate=1/6, lower.tail = FALSE)
p_5_4 <- p_9 / p_5
# La p que un article duri 4 anys més donat que ha durat 5
# és la mateixa que la p que duri 4 anys,
# per tant són independents!












