# PAC 1 - Qüestionari

# Dades
setwd("~/Desktop/UOC/Rdata")
project_path <- "1 Estadística/pac1/questionari/"
file_path1 <- paste(project_path, "vendes_pac1_P_15_1.csv", sep="")
file_path2 <- paste(project_path, "vendes_pac1_P_15_2.csv", sep="")
file_path3 <- paste(project_path, "vendes_pac1_P_15_3.csv", sep="")
file_path4 <- paste(project_path, "vendes_pac1_P_15_4.csv", sep="")
data1 <- read.table(file_path1, sep=";", dec=",", header=TRUE)
data2 <- read.table(file_path2, sep=";", dec=",", header=TRUE)
data3 <- read.table(file_path3, sep=";", dec=",", header=TRUE)
data4 <- read.table(file_path4, sep=";", dec=",", header=TRUE)


# Pregunta 1
q1 <- c(10,7,8,9,8,4,9)
round(mean(q1), digits = 3)
round(median(q1), digits = 3)
 # SD Poblacional
pop_N <- length(q1)
pop_sd <- sd(q1) * sqrt((pop_N-1)/pop_N)
round(pop_sd, digits = 3)


# Pregunta 2
q2 <- rbind(c(9,105), c(7,90), c(7,84), c(2,78))
mean_sum <- 0
mean_count <- 0
for(row in 1:nrow(q2)) {
  mean_sum <- mean_sum + (q2[row,1] * q2[row,2])
  mean_count <- mean_count + q2[row,1]
}
mean <- mean_sum / mean_count
m2 <- c(
  105,105,105,105,105,105,105,105,105,
  90,90,90,90,90,90,90,
  84,84,84,84,84,84,84,
  78,78
)
mean
summary(m2)
m2_pop_N <- length(m2)
m2_pop_sd <- sd(m2) * sqrt((m2_pop_N-1)/m2_pop_N)
round(m2_pop_sd, digits = 3)


# Pregunta 3
q3 <-      c(14,6,8,18,17,10,3,14,10,4)
m3 <- sort(c(14,6,8,18,17,10,3,14,10,4))
m3
h1 <- m3[1:5]
h2 <- m3[5:10]
summary(q3)
median(h1) #Q1
median(m3) #Q2
median(h2) #Q3


# Pregunta 4
m4 <- sort(c(2,6,6,6,6,6,14,14,14,14,14))
summary(m4)


# Pregunta 5
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# original
q5 <- sample(1:86, 91.4, replace=TRUE)
summary(q5)
mode(q5)
sd(q5)
# + 10
q5_plus_38 = q5 + 10
summary(q5_plus_38)
mode(q5_plus_38)
sd(q5_plus_38)
# * 10
q5_by_8 = q5 * 10
summary(q5_by_8)
mode(q5_by_8)
sd(q5_by_8)
# STATS
q5_mean <- 43.14130
q5_median <- 41
q5_mode <- 20
q5_sd <- 20.36782
# Part 1
q5_increase <- 38
mean_plus_38 <- q5_mean + q5_increase
median_plus_38 <- q5_median + q5_increase
mode_plus_38 <- q5_mode + q5_increase
sd_plus_38 <- q5_sd
# Part 2
mean_by_8 <- q5_mean * 8
median_by_8 <- q5_median * 8
mode_by_8 <- q5_mode * 8
sd_by_8 <- q5_sd * 8
# Part 3
mean_by_minus_2 <- q5_mean * -2
median_by_minus_2 <- q5_median * -2
mode_by_minus_2 <- q5_mode * -2
sd_by_minus_2 <- q5_sd * -2


# Pregunta 7 
hist(data2$PreuAm2)

# Pregunta 8
summary(data4)

# Pregunta 9
boxplot(data3$PreuAm2)

