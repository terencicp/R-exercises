library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)
library(dbscan)
library(clusterSim)
library(fpc)
library(patchwork)

# Dades

songs <- read.csv("songs_prepared.csv")
songs_svd <- read.csv("songs_svd.csv")

set.seed(204)
sample_indexs <- sample(nrow(songs), size = 10000)
songs_sample <- songs[sample_indexs, ]
songs_svd_sample <- songs_svd[sample_indexs, ]

# Ex 1

distance_euclidean <- dist(songs_svd_sample, method = "euclidean")
hc_euclidean <- hclust(distance_euclidean, method = "ward.D2")
clusters_euclidean <- cutree(hc_euclidean, 2)
DB_euclidean <- index.DB(songs_svd_sample, clusters_euclidean)$DB
CH_euclidean <- calinhara(songs_svd_sample, clusters_euclidean)

distance_manhattan <- dist(songs_svd_sample, method = "manhattan")
hc_manhattan <- hclust(distance_manhattan, method = "ward.D2")
clusters_manhattan <- cutree(hc_manhattan, 2)
DB_manhattan <- index.DB(songs_svd_sample, clusters_manhattan)$DB
CH_manhattan <- calinhara(songs_svd_sample, clusters_manhattan)

# EXERCICI 2 

minPts_start <- ncol(songs_svd) + 1
eps_start <- 2

# Interval de valors a provar
eps_search <- seq(eps_start/2, eps_start, by = 0.05)
minPts_search <- seq(minPts_start, minPts_start*20, by = 5)

# Dataframe que contindrà els millors agrupaments
results <- data.frame(eps = numeric(), minPts = numeric(), DB = numeric(), CH = numeric(), noise_prop = numeric())

for (eps in eps_search) {
    print(eps)
    for (minPts in minPts_search) {
        
        # Agrupa amb DBSCAN
        result <- dbscan::dbscan(songs_svd_sample, eps = eps, minPts = minPts)
        clusters <- as.integer(result$cluster)
        
        # Si s'ha obtingut més d'un clúster
        if (length(unique(clusters)) > 2) {
            
            # Calcula els índexs Davies-Bouldin i Calinski-Harabasz
            DB <- index.DB(songs_svd_sample, clusters)$DB
            CH <- calinhara(songs_svd_sample, clusters)
            
            # Calcula la proporció de punts identificats com a soroll
            noise_prop <- sum(clusters == 0) / length(clusters)
            
            # Desa els resultats
            results <- rbind(results, data.frame(eps, minPts, DB, CH, noise_prop))
        }}}

results_eps <- results %>%
    group_by(eps) %>%
    slice_min(DB)

db_colors <- c("-1" = "#000", "1" = "#00bfc4", "2" = "#f7766d")
for (i in c(1, 5, 10, 14)) {
    result <- dbscan::dbscan(songs_svd_sample, eps = results_eps$eps[i], minPts = results_eps$minPts[i])
    clusters <- factor(as.integer(result$cluster))
    DB <- round(results_eps$DB[i], 3)
    p <- ggplot(songs_sample, aes(x = acousticness, y = danceability, color = clusters)) +
        geom_point(alpha = ifelse(clusters == "0", 0.05, 0.2), stroke = 0) +
        scale_color_manual(values = db_colors, name = "Clúster") +
        labs(color = 'Cluster') +
        guides(color = guide_legend(override.aes = list(alpha = 1))) +
        ggtitle(paste0("DBSCAN, minPts=", result$minPts, ", eps =", result$eps, ", DB=", DB)) +
        theme(plot.title = element_text(size = 12))
    print(p)
}

# OPTICS

dbscan_final <- data.frame(
    eps = 1.65,
    minPts = 200,
    DB = 0.8876778,
    CH = 13070.19,
    noise_prop = 0.2244
)

optics_result <- optics(songs_svd_sample, minPts = dbscan_final$minPts)
plot(optics_result)

optics_clusters <- extractDBSCAN(optics_result, eps_cl = dbscan_final$eps)
plot(optics_clusters, main = "Reachability plot, eps_cl=0.3", ylim = c(0, 5))

DB_optics <- index.DB(songs_svd_sample, optics_clusters$cluster)$DB
CH_optics <- calinhara(songs_svd_sample, optics_clusters$cluster)


# Comparació

# Índexs precalculats per algoritme i conjunt de dades
indexs <- data.frame(
    "Davies-Bouldin" = c(DB_euclidean, DB_manhattan, dbscan_final$DB, DB_optics),
    "Calinski-Harabasz" = c(CH_euclidean, CH_manhattan, dbscan_final$CH, CH_optics)
)

# Transformació de les dades pel diagrama
indexs$label <- c('Jeràrquic (dist. euclidiana)', 'Jeràrquic (dist. manhattan)',
                  'DBSCAN', 'OPTICS')
indexs$label <- factor(indexs$label, levels = indexs$label)
indexs$color <- ifelse(grepl('dist', indexs$label), '#E69A8D', '#7b65ad')
indexs_long <- indexs %>% pivot_longer(-c(label, color), names_to = "index", values_to = "value")

# Diagrama dels índexs
ggplot(indexs_long, aes(x = label, y = value, fill = color)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    scale_fill_manual(values = c('#E69A8D', '#7b65ad'), labels = c('Basat en densitat', 'Jeràrquic')) +
    labs(y = "Índex", fill = "Model") +
    facet_wrap(~ index, scales = "free_y") +
    theme(
        axis.text.x = element_text(angle = 35, hjust = 1, vjust = 1),
        axis.title.x = element_blank(), 
        plot.margin = unit(c(1, 1, 1, 3), "lines")
    ) 





