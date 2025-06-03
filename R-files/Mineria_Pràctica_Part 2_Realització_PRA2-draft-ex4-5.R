library(dplyr)
library(ROSE)
library(C50)
library(DescTools)
library(reshape2)
library(caret)

# Load the data
songs <- read.csv("songs.csv")

# 2 DISCRETIZATION

continuous_vars <- setdiff(names(songs), c("genre", "key", "mode", "time_signature", "hit"))
for(var in continuous_vars) {
    # Calcula els quartils per crear tres grups de la mateixa mida
    quantiles <- quantile(songs[[var]], c(1/3, 2/3))
    # Discretitza en 'low', 'mid', 'high'
    songs[[var]] <- cut(songs[[var]], c(-Inf, quantiles, Inf), c("low", "mid", "high"), include.lowest=TRUE)
}

songs$hit <- factor(songs$hit, levels = c("1", "0"))

mean(as.integer(songs$hit) == 1)

# OVERSAMPLING

set.seed(204)
new_nrow <- 2 * (nrow(songs) - sum(as.integer(songs$hit) == 1))
songs_balanced <- ovun.sample(hit ~ ., songs, "over", new_nrow)$data

mean(as.integer(songs$hit) == 1)

n_rows <- nrow(songs_balanced)

# SPLIT

set.seed(204)
train_indexs <- sample(1:n_rows, size = round(0.7 * n_rows))

# Reordena les classes
songs_balanced$hit <- factor(songs_balanced$hit, levels = c("1", "0"))

# Dades d'entrenament
train <- songs_balanced[train_indexs, ]
train_x <- train[, -which(names(train) == "hit"), drop = FALSE]
train_y <- as.factor(train$hit)

# Dades de test
test <- songs_balanced[-train_indexs, ]
test_x <- test[, -which(names(test) == "hit"), drop = FALSE]
test_y <- as.factor(test$hit)


















# VAR SELECTION

# Crea la matriu
cols <- names(songs)
size <- length(cols)
cramerV_matrix <- matrix(NA, ncol = size, nrow = size)
colnames(cramerV_matrix) <- cols
rownames(cramerV_matrix) <- cols

# Calcula la V de Cramer per cada combinació de variables
for (i in 1:size) {
    for (j in 1:size) {
        cramerV_matrix[i, j] <- CramerV(songs[[cols[i]]], songs[[cols[j]]])
    }
}

# Mapa de calor
cramerV_long <- melt(cramerV_matrix, varnames = c("Var1", "Var2"), value.name = "CramerV")
ggplot(cramerV_long, aes(Var1, Var2, fill = CramerV)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", CramerV)), size = 2) +
    scale_fill_gradient2(low = "white", high = "black", mid = "blue2", midpoint = 0.5) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank())


# COMBI LOOP

# Variables més rellevants 
sorted_vars <- names(sort(cramerV_matrix["hit", -ncol(cramerV_matrix)], decreasing = TRUE))
top_combinations <- lapply(1:14, function(n) sorted_vars[1:n])

# Dataframe que contindrà els resultats
models <- data.frame(n_rules = integer(), accuracy = numeric(), stringsAsFactors = FALSE)

for (vars in top_combinations) {
    
    # Model basat en regles i avaluació
    rule_model <- C5.0(x = train_x[, vars, drop = FALSE], y = train_y, rules = TRUE)
    predictions <- predict(rule_model, test_x[, vars, drop = FALSE])
    accuracy <- unname(confusionMatrix(predictions, test_y)$overall['Accuracy'])
    
    # Add results to dataframe
    models <- rbind(models, data.frame(n_rules = rule_model$size, accuracy = accuracy))
    
    print(length(vars))
}





train_x_subset <- train_x[, sorted_vars[1:3], drop = FALSE]
rule_model <- C5.0(x = train_x_subset, y = train_y, rules = TRUE)
summary(rule_model)



library(grid)
tree_model <- C5.0(x = train_x_subset, y = train_y)
plot(tree_model, gp = gpar(fontsize = 6))
capture.output(summary(tree_model))[15:24]


predicted <- predict(tree_model, test_x)
confusionMatrix(predicted, test_y)



