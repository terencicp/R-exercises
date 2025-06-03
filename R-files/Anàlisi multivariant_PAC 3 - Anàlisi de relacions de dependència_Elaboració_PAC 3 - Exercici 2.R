# Anàlisi multivariant - PAC 3

# Exercici 2

library(dplyr)
library(ggplot2)

# Console output width
options(width=180)

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac3/"
file_path <- paste(project_path, "Ej2.csv", sep = "")
csv <- read.table(file_path, sep = "," , header = TRUE)

# Variable selection
heart <- csv %>%
  select(DEATH_EVENT,
         time, age, ejection_fraction, serum_creatinine, serum_sodium, sex, smoking)

# Distribution
hist(heart$time)
hist(heart$age)
hist(heart$ejection_fraction)
hist(heart$serum_creatinine)
hist(heart$serum_sodium)
xtabs(~ DEATH_EVENT + sex, data=heart)
xtabs(~ DEATH_EVENT + smoking, data=heart)

# CONDICIONS

# 5) Abscència de colinealitat
cor(heart)
library(corrplot)
corrplot(cor(heart))

# Logistic regression model
model1 <- glm(DEATH_EVENT ~ ., data=heart, family=binomial)
summary(model1)

# Variable selection
model2 <- step(model1)
summary(model2)

# Model without non-significant variables
model3 <- glm(DEATH_EVENT ~ serum_creatinine + ejection_fraction + age + time, data=heart, family=binomial)
summary(model3)

# McFadden's Pseudo-R²
library(DescTools)
PseudoR2(model3, which = "McFadden")

# Odds ratio
exp(coef(model3))

# The Hosmer-Lemeshow Goodness-of-Fit Test
library(glmtoolbox)
hltest(model3, verbose = TRUE)

