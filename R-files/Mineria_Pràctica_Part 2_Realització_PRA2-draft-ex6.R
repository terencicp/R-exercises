library(ggplot2)
library(tidyr)
library(clusterSim)
library(fpc)
library(dplyr)
library(dbscan)
library(fpc)
library(patchwork)
library(ROSE)
library(C50)
library(DescTools)
library(reshape2)
library(caret)
library(grid)
library(class)

songs <- read.csv("songs.csv")
songs_svd <- read.csv("songs_svd.csv")

# Previously
set.seed(204)
new_nrow <- 2 * (nrow(songs) - sum(as.integer(songs$hit) == 1))
songs_balanced <- ovun.sample(hit ~ ., songs, "over", new_nrow)$data
n_rows <- nrow(songs_balanced)
set.seed(204)
train_indexs <- sample(1:n_rows, size = round(0.7 * n_rows))

# NORMALIZE ! ! ! ! ! !! ! ! 
songs_svd <- as.data.frame(scale(songs_svd[, 1:9]))

# Afegeix "hit" al conjunt de dades procedent de SVD
songs_svd$hit <- songs$hit

# Oversampling
set.seed(204)
songs_svd_balanced <- ovun.sample(hit ~ ., songs_svd, "over", new_nrow)$data

# SPLIT

# Reordena les classes
songs_svd_balanced$hit <- factor(songs_svd_balanced$hit, levels = c("1", "0"))

# Dades d'entrenament
train_svd <- songs_svd_balanced[train_indexs, ]
train_svd_x <- train_svd[, -which(names(train_svd) == "hit"), drop = FALSE]
train_svd_y <- as.factor(train_svd$hit)

# Dades de test
test_svd <- songs_svd_balanced[-train_indexs, ]
test_svd_x <- test_svd[, -which(names(test_svd) == "hit"), drop = FALSE]
test_svd_y <- as.factor(test_svd$hit)

mean(as.integer(train_svd_y) == 1)
mean(as.integer(test_svd_y) == 1)




# Calculate means for each dataset
means_songs_svd <- colMeans(songs_svd[, 1:9])
means_train_svd_x <- colMeans(train_svd_x)
means_test_svd_y <- colMeans(test_svd_x)

# Create a data frame for plotting
mean_values <- data.frame(
    component = rep(paste("V", 1:9, sep = ""), 3),
    mean = c(means_songs_svd, means_train_svd_x, means_test_svd_y),
    dataset = rep(c("songs_svd", "train_svd_x", "test_svd_x"), each = 9)
)

# Create the plot
ggplot(mean_values, aes(x = component, y = mean, color = dataset)) +
    geom_point() +
    labs(x = "SVD Component", y = "Mitjana", color = "Conjunt de dades") +
    ylim(-0.2, 0.2)










# K

# Valors de k a provar
k <- seq(1, 20, by = 1)
# Calcula la taxa d'error pels diferents valors de k
error_rates <- numeric(length(k))
for (i in 1:length(k)) {
    predicted <- knn(train_svd_x, test_svd_x, train_svd_y, k = k[i])
    error_rates[i] <- mean(predicted != test_svd_y)
}
# Diagrama de la taxa d'error
plot(k, error_rates, type = "b", ylab = "Taxa d'error")


# k-NN
knn_model <- knn(train_svd_x, test_svd_x, train_svd_y, k = 5)


# Avaluació
confusionMatrix(knn_model, test_svd_y)





knn_model <- kknn(hit ~ ., train = train_svd, test = test_svd, k = 5, kernel = "epanechnikov")

predictions <- predict(knn_model, newdata = test_svd_x)
confusionMatrix(predictions, test_svd_y)


# proportion of songs hit == 1
mean(as.integer(songs$hit) == 1) *100
