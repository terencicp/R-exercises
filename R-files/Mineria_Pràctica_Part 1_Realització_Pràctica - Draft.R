library(knitr)
library(dplyr)
library(ggplot2)
library(scales)
library(gridExtra)
library(tidyr)
library(corrplot)
library(mltools)
library(data.table)

theme_set(theme_minimal(base_size = 12)) # ggplot2

# 1 Objectiu analític

# Regressió: Predir la popularitat
# Classificació: Predir hits (>95th percentile popularity)

# 2 Verificar les dades

# Load data
songs <- read.csv("spotify.csv")

# Data description: https://developer.spotify.com/documentation/web-api/reference/get-audio-features
# The data represents a wide range of genres, artists, and time periods to avoid bias in your model.
# API (machine generated data) = No errors

# track_id, artist_name, track_name
nrow_songs <- nrow(songs)
plot_cols <- c("IDs úniques", "Noms de cançó únics", "Artistes únics")
summary_df <- data.frame(
    name = factor(plot_cols, levels = plot_cols),
    value = c(length(unique(songs$track_id)),
              length(unique(songs$track_name)),
              length(unique(songs$artist_name)))
)
ggplot(summary_df, aes(x = name, y = value, fill = name)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_text(aes(label = value), vjust = -0.5) +  # Display full value
    labs(title = "Valors únics de track_id, track_name, artist_name", x = "", y = "") +
    theme(legend.position = "none", axis.text.x = element_text(angle = 45, vjust = 0.5, color = "black")) +
    ylim(0, 250000) +
    geom_hline(yintercept = nrow_songs, linetype = "dashed") +
    annotate("text", x = 2, y = nrow_songs, label = paste("Files del conjunt de dades:", nrow_songs), vjust = -1.5)
# track_id duplicate count:
sum(duplicated(songs$track_id))
# view duplicates (reason: genres):
duplicates <- songs %>%
    group_by(track_id) %>%
    filter(n() > 1) %>%
    ungroup() %>%
    arrange(track_name) %>%
    select(genre, artist_name, track_name)
kable(head(duplicates, 7))

# Genre is entered manually as a search term
unique(songs$genre)
genre_counts <- songs %>%
    group_by(genre) %>%
    summarise(count = n())
ggplot(genre_counts, aes(x = count, y = factor(genre_counts$genre, levels = rev(sort(unique(genre_counts$genre)))))) +
    geom_bar(stat = "identity", position = "dodge") +
    ggtitle("Nombre de cançons per gènere") +
    xlab("Nombre de cançons") +
    ylab("Gènere")

# POPULARITY
ggplot(spotify, aes(x = popularity)) +
    geom_histogram(binwidth = 1, fill = "darkblue", color = "darkblue") +
    labs(title = "Distribució de la popularitat", 
         x = "Popularitat", 
         y = "Nombre de cançons")
# by genres
spotify_popularity <- spotify %>%
    group_by(genre) %>%
    summarize(mean_popularity = mean(popularity, na.rm = TRUE)) %>%
    arrange(desc(mean_popularity)) %>%
    merge(spotify, ., by = "genre")
ggplot(spotify_popularity, aes(x = reorder(genre, -mean_popularity), y = popularity)) +
    geom_boxplot(fill = "cyan2", color = "darkblue", outlier.color = "darkblue") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = "Distribució de la popularitat per gènere", x = "Gènere", y = "Popularitat")

# DURATION, TEMPO, KEY
p1 <- ggplot(songs, aes(x = duration_ms)) +
    geom_histogram(bins = 60, fill = "#003f5c") +
    scale_x_continuous(labels = label_number()) +
    labs(title = "Distribució de duration_ms", x = "Duració (ms)", y = "Freqüència")
p2 <- ggplot(songs, aes(x = tempo)) +
    geom_histogram(bins = 60, fill = "#444e86") +
    labs(title = "Distribució de tempo", x = "Tempo (BPM)", y = "")
p3 <- ggplot(songs, aes(x = loudness)) +
    geom_histogram(bins = 60, fill = "#955196") +
    labs(title = "Distribució de loudness", x = "Volum (dB)", y = "")
p4 <- ggplot(songs, aes(x = time_signature)) +
    geom_bar(fill = "#dd5182") +
    labs(title = "Distrib. de time_signature", x = "Temps", y = "")
p5 <- ggplot(songs, aes(x = key)) +
    geom_bar(fill = "#ff6e54") +
    labs(title = "Distribució de key", x = "Tonalitat", y = "")
p6 <- ggplot(songs, aes(x = mode)) +
    geom_bar(fill = "#ffa600") +
    labs(title = "Distribució de mode", x = "Mode", y = "")
grid.arrange(p1, p2, p3, p4, p5, p6, nrow = 3, ncol = 2)
# duration median
median <- median(songs$duration_ms)
minutes <- (median %/% 60000)
seconds <- (median %% 60000 %/% 1000)
paste0(minutes, ":", seconds)
# time 0/4
songs[songs$time_signature == "0/4", c("genre", "artist_name", "track_name")]
# r^2
install.packages("corrplot")
library(corrplot)
numerical_data <- songs[, c("popularity", "duration_ms", "tempo", "loudness")]
cor_matrix <- cor(numerical_data)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black", tl.srt = 45)
# by pop
ggplot(data = songs, aes(x = duration_ms, y = popularity)) +
    geom_point() +
    labs(title = "Scatterplot of Duration vs. Popularity")
ggplot(data = songs, aes(x = tempo, y = popularity)) +
    geom_point() +
    labs(title = "Scatterplot of Tempo vs. Popularity")
ggplot(data = songs, aes(x = loudness, y = popularity)) +
    geom_point() +
    labs(title = "Scatterplot of Loudness vs. Popularity")
ggplot(data = songs, aes(x = time_signature, y = popularity)) +
    geom_boxplot()
ggplot(data = songs, aes(x = factor(key), y = popularity)) +
    geom_boxplot() +
    facet_wrap(~mode) +
    labs(title = "Boxplot of Key and Mode vs. Popularity")

# musicality
songs_long <- gather(songs, key = "feature", value = "value", 
                     acousticness, danceability, energy, instrumentalness, 
                     liveness, speechiness, valence)
ggplot(songs_long, aes(x = value)) +
    geom_density(fill = "blue", alpha = 0.5) +
    facet_wrap(~ feature, scales = "free", ncol = 2) +
    labs(x = "", y = "Densitat")
# by pop
cor_matrix <- cor(songs[, c("popularity", "acousticness", "danceability", "energy", "instrumentalness", "liveness", "speechiness", "valence")])
corrplot(cor_matrix, method = "circle", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black", order = "AOE")

avg_speechiness_by_genre <- aggregate(speechiness ~ genre, data = songs, mean)
ggplot(avg_speechiness_by_genre, aes(x = reorder(genre, speechiness), y = speechiness)) +
    geom_bar(stat = "identity", fill = "darkblue") +
    coord_flip() +
    labs(title = "Average Speechiness by Genre", 
         x = "Genre", 
         y = "Average Speechiness")

# 3.1 Data cleaning

songs <- read.csv("spotify.csv")

corrplot(cor(songs[, c("popularity", "loudness", "duration_ms", "tempo")]),
         method = "circle", type = "upper", order = "AOE",
         tl.srt = 45, tl.col = "black", addCoef.col = "black")

songs <- select(songs, c(-artist_name, -track_name))
songs <- subset(songs, time_signature != "0/4")
songs <- subset(songs, genre != "Children's Music")
songs <- subset(songs, speechiness < 0.5)

songs_onehot <- songs %>%
    distinct(track_id, .keep_all = TRUE) %>%
    left_join(songs %>%
                  distinct(track_id, genre) %>%
                  mutate(value = 1) %>%
                  spread(key = genre, value = value, fill = 0),
              by = "track_id")
songs_onehot$genre <- NULL

songs_dt <- as.data.table(songs_onehot)
songs_dt[, time_signature := as.factor(time_signature)]
songs_dt[, key := as.factor(key)]
songs_dt <- one_hot(songs_dt, cols = c("time_signature", "key"))
songs_onehot <- as.data.frame(songs_dt)

songs_onehot$mode <- ifelse(songs_onehot$mode == "Major", 1, 0)

songs_onehot_scaled <- songs_onehot
cols_to_normalize <- c("duration_ms", "tempo", "loudness", "acousticness", 
                       "danceability", "energy", "instrumentalness", 
                       "liveness", "speechiness", "valence")
songs_onehot_scaled[cols_to_normalize] <- scale(songs_onehot_scaled[cols_to_normalize])

songs_onehot_scaled$hit <- ifelse(songs_onehot_scaled$popularity >= 70, 1, 0)

# SVD
predictors <- select(songs_onehot_scaled, c(-track_id, -popularity, -hit))
svd_res <- svd(predictors)

# CUMULASTIVE EXPLAINED VARIANCE 
explained_variance <- svd_res$d^2 / sum(svd_res$d^2)
plot(cumsum(explained_variance), type="o", main = "Proporció de variança acumulada",
     xlab="Valors singulars", ylab="Variança explicada")

# CHOOSE K COMPONENTS & CALCULATE EXPLAINED VARIANCE
k <- 9
sum(explained_variance[1:k])

# Calcula les projeccions
U_reduced <- svd_res$u[, 1:k]
D_reduced <- diag(svd_res$d[1:k])
songs_svd <- U_reduced %*% D_reduced

head(songs_svd)

# plot: component 1 loadings
par(mar = c(5, 8, 4, 2))
barplot(svd_res$v[, 1], main = "Càrregues del 1r component", xlab = "Càrrega",
        horiz = TRUE, names.arg = colnames(predictors), las = 1, cex.names = 0.6)

# plot: component 1 highest loading
ggplot(data=as.data.frame(songs_svd), aes(x = V1, y = V2, color = songs_onehot_scaled$energy)) + 
    geom_point(alpha=0.5) +
    xlab("Component 1") +  ylab("Component 2") +
    scale_color_gradient(low = "blue", high = "red", name = "energy")


# plot: component 2 loadings
par(mar = c(5, 8, 4, 2))
barplot(svd_res$v[, 2], main = "Càrregues del 2n component", xlab = "Càrrega",
        horiz = TRUE, names.arg = colnames(predictors), las = 1, cex.names = 0.6)

# plot: component 2 highest loading
time_signature_4_4 <- factor(songs_onehot_scaled$"time_signature_4/4")
ggplot(data=as.data.frame(songs_svd), aes(x = V1, y = V2, color = time_signature_4_4)) + 
    geom_point(alpha=0.5) +
    xlab("Component 1") +  ylab("Component 2") +
    scale_color_manual(values=c("0" = "grey", "1" = "green4"))


write.csv(songs_onehot_scaled, "songs_prepared.csv", row.names = FALSE)
write.csv(as.data.frame(songs_svd), "songs_svd.csv", row.names = FALSE)

