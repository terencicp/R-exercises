# Anàlisi multivariant - Pràctica - Exercici 2

# Console output width
options(width=180)

# Font de les dades
# https://vincentarelbundock.github.io/Rdatasets/datasets.html

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/practica/"
file_path <- paste(project_path, "spam7.csv", sep = "")
csv <- read.csv(file_path, sep = "," , header = TRUE)

# Clean data
# Remove index
df <- csv[,2:8]
# Rename factors
library(dplyr)
df$yesno <- recode(df$yesno, y = 1, n  = 0)

# Variable distribution
for (col in names(df)) {
  if(df[,col][1] %in% c(0,1)) {
    boxplot(df$yesno ~ df[,col], main = col)
  } else {
    plot(df[,"yesno"], df[,col], main = col)
  }
}

# Logistic regression model
lr_6 <- glm(yesno ~ ., data = df, family = binomial)

# Variable selection
lr_5 <- step(lr_6)
summary(lr_5)

# MacFadden's PseudoR2
library(DescTools)
PseudoR2(lr_5, which = "McFadden")

# Outliers
pearson_residuals <- residuals(lr_5, "pearson")
length(pearson_residuals[abs(pearson_residuals ) > 3])

# The Hosmer-Lemeshow Goodness-of-Fit Test
library(glmtoolbox)
hltest(lr_5, verbose = TRUE)

# Linearity (aprox.)
par(mfrow = c(1, 5))
log_odds <- lr_5$linear.predictors
for (col in names(df[,1:5])) {
  line <- lm(df[,col] ~ logodds)
  title <- paste(col, " R² =", round(summary(line)$r.squared, 2))
  plot(logodds, df[,col], ylab=col, main=title)
  abline(line, col="red", lw=2)
}

# Correlation matrix
library("corrplot")
corrplot(cor(df))

# Interaction (no improvement)
lr_5i <- glm(yesno ~ crl.tot + dollar + bang + money + n000 + dollar:n000, data = df, family = binomial)
PseudoR2(lr_5i, which = "McFadden")

# Alternative link functions
lr <- glm(yesno ~ ., data = df, family = binomial(link = "probit"))
lr <- step(lr)
summary(lr)
PseudoR2(lr, which = "McFadden")
hltest(lr)

# Alt tests
chisq.test(lr_5)

# Odds ratio
exp(coef(lr_5))

# Intercept probabilities
logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
logit2prob(coef(lr_5)[1])

