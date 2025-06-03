library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(clusterSim)
library(fpc)

songs <- read.csv("songs_prepared.csv")
songs_svd <- read.csv("songs_svd.csv")

### EXERCICI 1

set.seed(204)
sample_indexs <- sample(nrow(songs), size = 100000)
songs_sample <- songs[sample_indexs, ]
songs_svd_sample <- songs_svd[sample_indexs, ]

distance_1 <- dist(songs_svd_sample, method = "euclidean")
hc_1 <- hclust(distance_1, method = "complete")
clusters_1 <- cutree(hc_1, 2)

p1 <- ggplot(as.data.frame(songs_svd_sample), aes(x = V1, y = V2, color = factor(clusters_1))) +
    geom_point(alpha = 0.2, stroke = 0) +
    labs(color = 'Cluster') +
    guides(color = guide_legend(override.aes = list(alpha = 1)))
p2 <- ggplot(songs_sample, aes(x = acousticness, y = danceability, color = factor(clusters_1))) +
    geom_point(alpha = 0.2, stroke = 0) +
    labs(color = 'Cluster') +
    guides(color = guide_legend(override.aes = list(alpha = 1)))
grid.arrange(p1, p2, ncol = 2)

DB_1 <- index.DB(songs_svd_sample, clusters_1)$DB
CH_1 <- calinhara(songs_svd_sample, clusters_1)

DB_1
CH_1

#---

hc_1 <- hclust(distance_1, method = "ward.D2")


### EXERCICI 2

distance_manhattan <- dist(songs_svd_sample, method = "manhattan")
hc_manhattan <- hclust(distance_manhattan, method = "ward.D2")
clusters_manhattan <- cutree(hc_manhattan, 2)

DB_manhattan <- index.DB(songs_svd_sample, clusters_manhattan)$DB
CH_manhattan <- calinhara(songs_svd_sample, clusters_manhattan)
DB_manhattan
CH_manhattan

p1 <- ggplot(songs_svd_sample[no_outlier_indexs, ], aes(x = V1, y = V2, color = factor(clusters_manhattan))) +
    geom_point(alpha = 0.2, stroke = 0) +
    labs(color = 'Cluster') +
    guides(color = guide_legend(override.aes = list(alpha = 1)))
p2 <- ggplot(songs_sample[no_outlier_indexs, ], aes(x = acousticness, y = danceability, color = factor(clusters_manhattan))) +
    geom_point(alpha = 0.2, stroke = 0) +
    labs(color = 'Cluster') +
    guides(color = guide_legend(override.aes = list(alpha = 1)))
grid.arrange(p1, p2, ncol = 2)




