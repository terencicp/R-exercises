# Anàlisi multivariant - PAC 2

# Exercici 2

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac2/"
file_path <- paste(project_path, "Ej2.csv", sep = "")
csv <- read.table(file_path, sep = "," , header = TRUE)

# Pre-processing
data <- csv[, c(2,4,5,6,7,8)]
countries <- scale(data[,-1])

# Bivariate simplification
mean_price <- rowMeans(csv[,6:8])
bv <- data.frame(country = data$Country, library_size=csv$Total.Library.Size, mean_price)

# Does it have missing values?
any(is.na(data))

# Pre-Clustering exploratory analysis
# Summary
summary(data)
str(data)
apply(data, 2, sd)
# Scatterplot matrix
pairs(data[,-1], pch=".", cex=1.5)
# Histograms
hist(data$No..of.TV.Shows)
hist(data$No..of.Movies)
hist(data$Cost.Per.Month.Basic)
hist(data$Cost.Per.Month.Standard)
hist(data$Cost.Per.Month.Premium)
# Correlation
names(countries)<- c("TV Shows","Movies","Basic", "Standard", "Premium")
round(cor(countries), 2)
library("corrplot")
corrplot(cor(countries))

# Hierarchical clustering
d <- dist(countries, method="euclidean")
hc <- hclust(d, method="complete")

# Colors
cool1 <- c("orange", "cyan3", "magenta2", "blue", "green3", "red")
cool2 <- c("orange", "red", "green3", "blue", "magenta2", "cyan3")

# Dendrogram
library("dendextend")
par(mfrow = c(1, 1))
dendrogram <- color_branches(as.dendrogram(hc), h=rev(hc$height)[6], col=cool1)
plot(dendrogram, ylab="Altura")

# Observations by cluster
par(mfrow = c(2, 3))
for (i in 2:7) {
  bv$cluster <- cutree(hc, i)
  plot(bv$csv.Total.Library.Size, bv$mean_price, col=bv$cluster, main=paste("k =", i))
  abline(h=mean(bv$mean_price), lty="dotted")
  abline(v=mean(bv$csv.Total.Library.Size), lty="dotted")
}

# Number of clusters chosen
par(mfrow = c(1, 1))
bv$cluster <- cutree(hc, 6)
plot(bv$library_size, bv$mean_price, col=bv$cluster)
abline(h=mean(bv$mean_price), lty="dotted")
abline(v=mean(bv$library_size), lty="dotted")
#text(bv$library_size, bv$mean_price, labels=bv$country, col=bv$cluster)

# Cluster plot
data <- csv
mean_price <- rowMeans(csv[,6:8])
plot(data$Total.Library.Size, mean_price, col=cool2[cutree(hc, 6)], pch=19)
abline(v=mean(data$Total.Library.Size), lty="dotted")
abline(h=mean(mean_price), lty="dotted")

# Change in height
height <- rev(hc$height)
height_changes <- data.frame(k=c(2:10), change=NA)
for (i in 1:9) { height_changes$change[i] <- (height[i+1] - height[i]) * i+1 }
print(height_changes)


