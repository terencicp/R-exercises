# Anàlisi multivariant - Pràctica - Exercici 4

# Console output width
options(width=280)

# Font de les dades
# https://vincentarelbundock.github.io/Rdatasets/datasets.html
# https://vincentarelbundock.github.io/Rdatasets/doc/Stat2Data/FirstYearGPA.html

# GPA: First-year college GPA on a 0.0 to 4.0 scale
# HSGPA:	High school GPA on a 0.0 to 4.0 scale
# SATV:	Verbal/critical reading SAT score
# SATM:	Math SAT score
# Male:	1= male, 0= female
# HU:	Number of credit hours earned in humanities courses in high school
# SS:	Number of credit hours earned in social science courses in high school
# FirstGen:	1= student is the first in her or his family to attend college, 0=otherwise
# White:	1= white students, 0= others
# CollegeBound:	1=attended a high school where >=50% students intended to go on to college, 0=otherwise

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/practica/"
file_path <- paste(project_path, "FirstYearGPA.csv", sep = "")
csv <- read.csv(file_path, sep = "," , header = TRUE)

# Remove index column
df <- csv[,-1]

# Variable distribution
for (col in names(df[,-1])) {
  if(df[,col][1] %in% c(0,1)) {
    boxplot(df$GPA ~ df[,col], main = col)
  } else {
    plot(df[,"GPA"], df[,col], main = col)
  }
}

# Regressió lineal múltiple
mlr_all <- lm(GPA ~ ., data = df)
library(olsrr)
ols_step_forward_p(mlr_all, progress=TRUE, details=TRUE)

# Variables seleccionades
mlr_5 <- lm(GPA ~ HSGPA + HU + White + SATV + SS, data = df)

# Test assumptions
layout(matrix(c(1,2,3,4),2,2))
plot(mlr_5)

# Coefficients
summary(mlr_5)
coefficients(lm(GPA ~ HSGPA + HU + White + SATV, data = df))

