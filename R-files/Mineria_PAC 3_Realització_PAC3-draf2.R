library(ggplot2)
library(dplyr)
library(DescTools)
library(reshape2)
library(C50)
library(grid)
library(caret)
library(randomForest)

credit_subset <- read.csv("credit.csv")[, c(
    "default", "checking_balance", "credit_history", "savings_balance",
    "purpose", "amount", "property", "employment_length", "age"
)]

categories_to_aggregate <- c("fully repaid this bank", "fully repaid")
credit_subset$credit_history[credit_subset$credit_history %in% categories_to_aggregate] <- "fully repaid"

categories_to_aggregate <- c("repairs", "others", "domestic appliances", "retraining")
credit_subset$purpose[credit_subset$purpose %in% categories_to_aggregate] <- "others"

credit_subset <- credit_subset %>%
    mutate_if(is.character, as.factor) %>%
    mutate(default = as.factor(default))

# Separa els predictors (x) de la variable objectiu (y)
x <- credit_subset[, !(names(credit_subset) %in% "default")]
y <- credit_subset[, "default"]

# Mostra aleatòria d'índexs
set.seed(616)
indexes = sample(1:nrow(credit_subset), size = nrow(credit_subset) * 2/3)

# Conjunt d'entrenament
trainx <- x[indexes, ]
trainy <- y[indexes]

# Conjunt de prova
testx <- x[-indexes, ]
testy <- y[-indexes]

rule_model <- C5.0(trainx, trainy, rules = TRUE)
tree_model <- C5.0(trainx, trainy)




# random forest model
set.seed(616)
rf_model <- randomForest(x = trainx, y = trainy)
rf_model
predicted <- predict(rf_model, testx)
confusionMatrix(predicted, testy)

# random forest model + cross-validation
control <- trainControl(method = "cv", number = 10, savePredictions = TRUE)
set.seed(616)
rfcv_model <- train(x, y, method = "rf", trControl = control)
print(rfcv_model)
confusionMatrix(rfcv_model$pred$pred, rfcv_model$pred$obs)









set.seed(616)
control <- trainControl(method = "cv", number = 10, savePredictions = "final")
gbm_model <- train(x, y, method = "gbm", trControl = control, tuneLength = 5)
print(gbm_model)
confusion_matrix_gbm <- confusionMatrix(gbm_model$pred$pred, gbm_model$pred$obs)
confusion_matrix_gbm







set.seed(616)
control <- trainControl(method = "cv", number = 10, savePredictions = "final")
levels(y) <- c("Good", "Bad")
rulecv_model <- train(x, y, method = "C5.0", trControl = control,
    tuneGrid = data.frame(.model = "rules", .trials = 1, .winnow = FALSE)
)
print(rulecv_model)













set.seed(616)
control <- trainControl(method = "cv", number = 10, savePredictions = "final")
levels(y) <- c("Good", "Bad")
rfcv_model <- train(x, y, method = "rf", trControl = control, tuneLength = 5)

set.seed(616)
control <- trainControl(method = "cv", number = 10, savePredictions = "final")
treecv_model <- train(x, y, method = "C5.0", trControl = control,
                      tuneGrid = data.frame(.model = "tree", .trials = 1, .winnow = FALSE)
)

tree_precisions <- confusionMatrix(treecv_model$pred$pred, treecv_model$pred$obs)$byClass['Precision']
rf_precisions <- confusionMatrix(rfcv_model$pred$pred, rfcv_model$pred$obs)$byClass['Precision']

t_test_result <- t.test(tree_precisions, rf_precisions, paired = TRUE)
print(t_test_result)


