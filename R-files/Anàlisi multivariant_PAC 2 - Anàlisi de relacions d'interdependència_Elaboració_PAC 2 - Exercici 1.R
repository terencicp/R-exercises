# Anàlisi multivariant - PAC 2

# Exercici 1

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac2/"
file_path <- paste(project_path, "Ej1.csv", sep = "")
csv <- read.table(file_path, sep = "," , header = TRUE)

# Pre-processing
# Data frame with first 2 columns removed
data <- csv[, c(5,6,8,9,10,11,12,13,14)]
# Transforming variables: Categorical to numeric
unique(data$location)
unique(data$education)
employees <- data
employees$location <- c("City"=0, "Suburb"=1)[data$location]
employees$education <- c("UG"=0, "PG"=1)[data$education]


# Does it have missing values?
any(is.na(employees))

# Pre-Clustering exploratory analysis
# Summary
summary(employees)
str(employees)
apply(employees, 2, sd)
# Scatterplot matrix
pairs(employees, pch=".", cex=1.5)
# Individual bar plot
for (i in 1:ncol(employees)) {
  if (i==8) { next }
  barplot(table(employees[i]), main=colnames(employees)[i])
}
# Boxplot for salary
boxplot(employees$salary, main="Salary")
# Correlation matrix
cor(employees)
library("corrplot")
corrplot(cor(employees))


# k-means attributes
set.seed(25)
nstart <- 20


# k-means (standarization) ----------------
employees <- data.frame(scale(employees))

# Select n of clusters
par(mfrow = c(1, 1))
twss <- c()
for (i in 1:9) {
  km <- kmeans(employees, centers = i)
  twss[i] <- km$tot.withinss
}
plot(1:9, twss, type = "b", xlab = "Clusters", 
     ylab = "Suma de quadrats intragrupals")

# Plotting clusters over pca
par(mfrow = c(1, 1))
prin <- princomp(employees, cor=TRUE)
k_means <- kmeans(employees, centers=3, nstart=nstart)
plot(prin$scores, col=k_means$cluster, pch=employees$satisfied+1)

# View data by clusters
cluster_data <- data
cluster_data$cluster <- k_means$cluster
cluster1 <- subset(cluster_data, cluster==1)
cluster2 <- subset(cluster_data, cluster==2)
cluster3 <- subset(cluster_data, cluster==3)

for (df in list(cluster1, cluster2, cluster3)) {
  par(mfrow = c(1, 3))
  for (i in c(3,8,9)) {
    barplot(table(df[i]), main=colnames(df)[i])
}}



# K-MODES (without salary and normalization) ----------------
library('klaR')
empl <- data.frame(scale(employees[,-8], center = FALSE))

# Select n of clusters
par(mfrow = c(1, 1))
twss <- 0
for (i in 1:9) {
  km <- kmodes(empl, i)
  twss[i] <- sum(km$withindiff)
}
plot(1:9, twss, type = "b",
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

# Plotting clusters over pca
par(mfrow = c(1,1))
prin <- princomp(empl, cor=TRUE)
k_modes <- kmodes(empl, 2)
plot(prin$scores, col = k_modes$cluster)

# View data by clusters
cluster_data <- data[,-8]
cluster_data$cluster <- k_modes$cluster
cluster1 <- subset(cluster_data, cluster==1)
cluster2 <- subset(cluster_data, cluster==2)
cluster3 <- subset(cluster_data, cluster==3)

for (df in list(cluster_data, cluster1, cluster2, cluster3)) {
  par(mfrow = c(3, 3))
  for (i in 1:ncol(df)) {
    counts <- table(df[i])
    barplot(counts, main=colnames(df)[i])
  }
}



# k-means (discrete variables) ----------------
empl <- data.frame(scale(employees[,c(3,4,6)], center = FALSE))

# Select n of clusters
par(mfrow = c(1, 1))
twss <- 0
for (i in 1:9) {
  km <- kmeans(empl, centers = i, nstart = nstart)
  twss[i] <- km$tot.withinss
}
plot(1:9, twss, type = "b",
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")
clust3
# Plotting clusters over pca
prin <- princomp(empl, cor=TRUE)
k_means <- kmeans(empl, centers=3, nstart=nstart)
plot(prin$scores, col = k_means$cluster, pch=empl$education)

# View data by clusters
cluster_data <- data[,c(3,4,6)]
cluster_data$cluster <- k_means$cluster
cluster1 <- subset(cluster_data, cluster==1)
cluster2 <- subset(cluster_data, cluster==2)
cluster3 <- subset(cluster_data, cluster==3)

for (df in list(cluster_data, cluster1, cluster2, cluster3)) {
  par(mfrow = c(3, 3))
  for (i in 1:ncol(df)) {
    counts <- table(df[i])
    barplot(counts, main=colnames(df)[i])
  }
}
