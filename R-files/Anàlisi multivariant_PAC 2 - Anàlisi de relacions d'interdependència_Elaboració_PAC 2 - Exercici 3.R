# Anàlisi multivariant - PAC 2

# Exercici 3

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac2/"
file_path <- paste(project_path, "Ej3.csv", sep = "")
data <- read.table(file_path, sep = "," , header = TRUE)
# Data frame with first 2 columns removed
customers <- data[, -c(1:2)]

# Does it have missing values?
any(is.na(customers))

# Pre-PCA exploratory analysis
# Summary
summary(customers)
apply(customers, 2, sd)
# Scatterplot matrix
pairs(customers, pch=".", cex=1.5)
# Individual boxplots (outliers?)
for (i in 1:ncol(customers)) {
  boxplot(customers[i], main=colnames(customers)[i])
}
# Correlation matrix
cor(customers)
library("corrplot")
corrplot(cor(data))

# PCA
pr <- prcomp(customers, scale.=TRUE)
prin <- princomp(customers, cor=TRUE)
loadings(prin)

# How many components to choose?
plot(prin, type="l", main="Diagrama de sedimentació")
summary(prin)

# Should we rotate components?
# Plot of the first 2 components
plot(loadings(prin), xlim=c(-0.1, 0.8), col="white")
abline(h=0, col="blue"); abline(v=0, col="blue")
text(loadings(prin), labels=colnames(customers))

# Scores for the first 2 components
predict(prin, newdata=customers)

