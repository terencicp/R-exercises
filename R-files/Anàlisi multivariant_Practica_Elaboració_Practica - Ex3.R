# Anàlisi multivariant - Pràctica - Exercici 3

# Console output width
options(width=200)

# Font de les dades i descripció
# https://vincentarelbundock.github.io/Rdatasets/datasets.html
# https://vincentarelbundock.github.io/Rdatasets/doc/carData/BEPS.html

# Data from the 1997-2001 British Election Panel Study (BEPS):
# vote: Party choice: Conservative, Labour, or Liberal Democrat
# age: in years
# economic.cond.national: Assessment of current national economic conditions, 1 to 5 (more is better).
# economic.cond.household: Assessment of current household economic conditions, 1 to 5 (more is better).
# Blair: Assessment of the Labour leader, 1 to 5 (more is better).
# Hague: Assessment of the Conservative leader, 1 to 5 (more is better).
# Kennedy: Assessment of the leader of the Liberal Democrats, 1 to 5 (more is better).
# Europe: an 11-point scale that measures respondents' attitudes toward European integration.
#         High scores represent ‘Eurosceptic’ sentiment.
# political.knowledge: Knowledge of parties' positions on European integration, 0 to 3.
# gender: female or male.

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/practica/"
file_path <- paste(project_path, "BEPS.csv", sep = "")
csv <- read.csv(file_path, sep = "," , header = TRUE)

# Remove 1st col + Filter by Conservative
df <- csv[csv$vote == "Conservative", 3:11]
# Rename gender
df$gender <- c("male" = 0, "female" = 1)[df$gender]

# Distribution of each variable
library(ggplot2)
for (col in names(df)) {
  print(ggplot(df, aes(x = df[,col])) + geom_bar() + ggtitle(col))
}

# Standarize data
df_std <- scale(df)


# HIERARCHICAL

# Hierarchical clustering
d <- dist(df_std, method="euclidean")
hc <- hclust(d, method="complete")

# Choose k groups
height <- rev(hc$height)
diff <- data.frame(k = c(2:10), h_change = NA)
for (i in 1:9) {
  diff$h_change[i] <- (height[i] - height[i+1]) * i+1
}
print(diff)

# Add groups to df
df$group <- as.factor(cutree(hc, 4))
# Sort groups by age
df$group <- c("1" = 1, "2" = 3, "3" = 2, "4" = 4)[df$group]


# ANALYSIS

# Distribution of each variable by group
library(ggpubr)
for (col in names(df[,-ncol(df)])) {
  plot <- ggbarplot(df, x = "group", y = col, add = c("mean_se"), fill = "group") +
            ggtitle(col) +
            theme(legend.position = "none")
  print(plot)
}

#
dendrogram <- as.dendrogram(hc)
plot(dendrogram, ylab="Altura")

