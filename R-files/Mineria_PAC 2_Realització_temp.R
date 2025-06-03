library("dbscan")
library("clusterSim")
library("fpc")
library("mclust")
library("ggplot2")
library("dplyr")
library("tidyr")
library("gridExtra")
library("cluster")
library("Stat2Data")
data("Hawks")

df <- Hawks[, c("Wing", "Weight", "Culmen", "Hallux", "Species")]
df$Species <- factor(df$Species, levels = c("RT", "CH", "SS"))

# NA imputation
for (species in unique(df$Species)) {
    species_df <- df[df$Species == species, ]
    for (measure in names(df)[1:4]) {
        is_na <- is.na(species_df[[measure]])
        species_mean <- mean(species_df[[measure]], na.rm = TRUE)
        species_df[[measure]][is_na] <- species_mean
    }
    df[df$Species == species, ] <- species_df
}

# Outliers
are_outliers <- function(x) {
    z_scores <- scale(x)
    return(abs(z_scores) > 3)
}
df_outliers <- df %>%
    group_by(Species) %>%
    mutate(outliers_Wing = are_outliers(Wing),
           outliers_Weight = are_outliers(Weight),
           outliers_Culmen = are_outliers(Culmen),
           outliers_Hallux = are_outliers(Hallux)) %>%
    ungroup()
df_no_outliers <- df_outliers %>%
    filter(!(outliers_Wing | outliers_Weight | outliers_Culmen | outliers_Hallux)) %>%
    select(-starts_with("outliers_"))
df_no_outliers$Species <- factor(df_no_outliers$Species, levels = c("RT", "CH", "SS"))

# Estandardització
measures <- c("Wing", "Weight", "Culmen", "Hallux")
df_std <- data.frame(scale(df[, measures]))
df_no_outliers_std <- data.frame(scale(df_no_outliers[, measures]))
df_std$Species <- df$Species
df_no_outliers_std$Species <- df_no_outliers$Species
df_std$Species <- factor(df_std$Species, levels = c("RT", "CH", "SS"))
df_no_outliers_std$Species <- factor(df_no_outliers_std$Species, levels = c("RT", "CH", "SS"))

# K-MEANS
# k=4
set.seed(204)
n_clusters <- 4
km_4 <- kmeans(df_std[1:4], centers = n_clusters, nstart = 30)
df_std$cluster <- km_4$cluster
# k=3
set.seed(204)
n_clusters <- 3
km_no_outliers_3 <- kmeans(df_no_outliers_std[1:4], centers = n_clusters, nstart = 30)
df_no_outliers_std$cluster <- km_no_outliers_3$cluster

# DBSCAN
minPts = 8
# Agrupament DBSCAN amb eps = 0.4, amb valors extrems
set.seed(204)
db_04 <- dbscan(df_std[1:4], eps = 0.4, MinPts = minPts)
df_std$cluster <- db_04$cluster

# Agrupament DBSCAN amb eps = 0.5, sense valors extrems
set.seed(204)
db_no_outliers_05 <- dbscan(df_no_outliers_std[1:4], eps = 0.5, MinPts = minPts)
df_no_outliers_std$cluster <- db_no_outliers_05$cluster


