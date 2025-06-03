# Repte 4 - Interpretabilitat

# Terenci Claramunt Porta

# Requisits per executar l'script:

# Llibreries usades
if (!requireNamespace("car")) install.packages("car")
if (!requireNamespace("randomForest")) install.packages("randomForest")
if (!requireNamespace("pdp")) install.packages("pdp")
if (!requireNamespace("ggplot2")) install.packages("ggplot2")
if (!requireNamespace("gridExtra")) install.packages("gridExtra")
if (!requireNamespace("iml")) install.packages("iml")
if (!requireNamespace("dplyr")) install.packages("dplyr")
if (!requireNamespace("fairmodels")) install.packages("fairmodels")
if (!requireNamespace("DALEX")) install.packages("DALEX")
if (!requireNamespace("caret")) install.packages("caret")
if (!requireNamespace("tidyr")) install.packages("tidyr")

# Cal que el directori de treball sigui el mateix que el de l'script
# i que contingui els fitxers següents

# Fitxers requerits
if (!file.exists("HousingData.csv")) stop("Error: HousingData.csv requerit")
if (!file.exists("loan_sanction_train.csv")) stop("Error: loan_sanction_train.csv requerit")
if (!file.exists("loan_sanction_test.csv")) stop("Error: loan_sanction_test.csv requerit")


# ----------


# PREGUNTA 1

library(car)
library(randomForest)
library(pdp)
library(ggplot2)
library(gridExtra)
library(iml)

# Dades
housing_df <- read.csv("HousingData.csv")

# Anàlisi exploratòria
str(housing_df)
summary(housing_df)

# Diagrames de dispersió amb la variable objectiu
correlations <- sapply(
    housing_df[-which(names(housing_df) == "MEDV")], 
    function(col) cor(housing_df$MEDV, col, use = "complete.obs")
)
for (col in names(sort(correlations))) {
    plot(housing_df$MEDV, housing_df[[col]], 
         xlab = "MEDV", ylab = col,
         main = paste("Correlació:", round(correlations[col], 2)))
}

# Histogrames de les variables
for (col in names(housing_df)) {
    hist(housing_df[[col]], main = col)
}

# Recomptes de valors buits
sapply(housing_df, function(col) sum(is.na(col)))

# Files amb més d'un valor buit
housing_df[apply(housing_df, 1, function(row) sum(is.na(row)) > 1), ]

# Imputa els valors buits usant la mediana
impute_median <- function(col) {
    if (is.numeric(col)) {
        col[is.na(col)] <- median(col, na.rm = TRUE)
    }
    return(col)
}
housing_df <- as.data.frame(lapply(housing_df, impute_median))

# Divideix les dades en conjunts d'entrenament i test
set.seed(687)
n <- nrow(housing_df)
split <- sample(seq_len(n), size = 0.75 * n)
housing_train <- housing_df[split, ]
housing_test <- housing_df[-split, ]

# Entrena el model de regressió lineal
housing_lm <- lm(MEDV ~ ., housing_train)
summary(housing_lm)

# Test d'autocorrelació
durbinWatsonTest(housing_lm)

# Test d'homoscedasticitat
plot(housing_lm$fitted.values, abs(housing_lm$residuals),
     xlab = "Prediccions", ylab = "Valor absolut dels residus")

# Test de normalitat
shapiro.test(housing_lm$residuals)

# Entrena el model de random forest
set.seed(687)
housing_rf <- randomForest(MEDV ~ ., housing_train, importance = TRUE)
print(housing_rf)

# Importància de les variables
importance_df <- as.data.frame(importance(housing_rf))
importance_df[order(-importance_df$`%IncMSE`), ]

# Partial Dependence Plots
pdp <- lapply(rownames(importance_df), function(var) {
    pd <- partial(housing_rf, pred.var = var, train = housing_train)
    autoplot(pd) + xlab(var) + ylab("MEDV predit")
})
do.call(grid.arrange, c(pdp, top = "Partial Dependence Plots"))

# SHAP
predictor <- Predictor$new(housing_rf, housing_train, y = housing_train$MEDV)
for (i in 1:10) {
    shap <- Shapley$new(predictor, x.interest = housing_train[i, ])
    print(plot(shap))
}


# ----------


# PREGUNTA 2

library(dplyr)
library(fairmodels)
library(DALEX)

# Dades
loan_train <- read.csv("loan_sanction_train.csv")
loan_test <- read.csv("loan_sanction_test.csv")

# Anàlisi exploratòria
str(loan_train)
summary(loan_train)

# Grups dels atributs sensibles
sensible <- c("Gender", "Married", "Education", "Property_Area")
privileged <- c("Male",   "Yes",    "Graduate",     "Urban")
for (col in sensible) {
    print(col)
    print(table(loan_train[[col]]))
}

# Anomena els grups sense nom de les variables sensibles
loan_train[loan_train == ""] <- "Unknown"
loan_train[loan_train == ""] <- "Unknown"

# Recomptes de valors buits
sapply(loan_train, function(col) sum(is.na(col)))

# Histogrames de les variables amb valors buits
for (col in c("LoanAmount", "Loan_Amount_Term", "Credit_History")) {
    hist(loan_train[[col]], main = col)
}

# Imputa valors buits usant la mediana dels grups interseccionals sensibles
impute_by_group <- function(df, sensible) {
    df %>%
    group_by(across(all_of(sensible))) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm=T), .))) %>%
    ungroup()
}
loan_train <- impute_by_group(loan_train, sensible)
loan_test <- impute_by_group(loan_test, sensible)
# Excepció per un cas que no té prou dades per calcular la mediana
loan_train <- impute_by_group(loan_train, setdiff(sensible, "Married")) 

# Binaritza la variable objectiu
loan_train$Loan_Status <- as.numeric(ifelse(loan_train$Loan_Status == "Y", 1, 0))

# Elimina l'identificador
loan_train <- loan_train %>% select(-Loan_ID)
loan_test <- loan_test %>% select(-Loan_ID)

# Transforma les variables categòriques en factors
loan_train[] <- lapply(loan_train, function(col)
    if (is.character(col)) as.factor(col) else col)
loan_test[] <- lapply(loan_test, function(col)
    if (is.character(col)) as.factor(col) else col)

# Diagrames d'equitat de les variables sensibles
plot_fairness <- function(protected, privileged, cutoffs) {
    loan_lr <- glm(Loan_Status ~ ., binomial(), 
                   select(loan_train, -all_of(protected)))
    lr_explainer <- explain(loan_lr, loan_train, loan_train$Loan_Status)
    fobject <- fairness_check(lr_explainer, cutoff = cutoffs,
        protected = loan_train[[protected]], privileged = privileged)
    plot(fobject)
}
plot_fairness("Gender", "Male", list(Female=0.5, Unknown=0.5))
plot_fairness("Married", "Yes", list(No=0.5, Unknown=0.5))
plot_fairness("Education", "Graduate", list("Not Graduate"=0.5))
plot_fairness("Property_Area", "Urban", list(Semiurban=0.5, Rural=0.5))

# Cutoff manipulation
loan_lr <- glm(Loan_Status ~ ., binomial(), loan_train)
lr_explainer <- explain(loan_lr, loan_train, loan_train$Loan_Status)
fobject <- fairness_check(
    lr_explainer, cutoff = list(Female = 0.5, Unknown = 0.5),
    protected = loan_train$Gender, privileged = "Male")
plot(ceteris_paribus_cutoff(fobject, subgroup = c("Female")))


# ----------


# PREGUNTA 3

library(randomForest)
library(caret)
library(ggplot2)
library(tidyr)

# Data red
url_red <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv"
data_red <- read.csv(url_red, sep=';')

# Exploratory analysis red
str(data_red)

# Preprocessing red
X_red <- data_red[, -which(names(data_red) == "quality")]
y_red <- as.factor(data_red$quality)

# Training and test datasets red
set.seed(687)
trainIndex <- createDataPartition(y_red, p = 0.7, list = FALSE)
X_train_red <- X_red[trainIndex, ]
y_train_red <- y_red[trainIndex]
X_test_red <- X_red[-trainIndex, ]
y_test_red <- y_red[-trainIndex]

# Random Forest Training red
rf_model <- randomForest(X_train_red, y_train_red, importance = TRUE)
print(rf_model)

# Prediction red
y_pred_red <- predict(rf_model, X_test_red)

# Evaluation red
y_pred_red <- factor(y_pred_red, levels = c(levels(y_pred_red), "9"))
y_test_red <- factor(y_test_red, levels = c(levels(y_test_red), "9"))
conf_matrix_red <- confusionMatrix(y_pred_red, y_test_red)
print(conf_matrix_red)

# Metrics red
accuracy_red <- conf_matrix_red$overall["Accuracy"]
mean_precision_red <- mean(conf_matrix_red$byClass[, "Precision"], na.rm = T)
mean_recall_red <- mean(conf_matrix_red$byClass[, "Recall"], na.rm = T)
cat("Accuracy:", accuracy_red, "\n")
cat("Mean precision:", mean_precision_red, "\n")
cat("Mean recall:", mean_recall_red, "\n")

# Confusion matrix red
conf_matrix_table_red <- as.table(conf_matrix_red)
conf_matrix_percent_red <- prop.table(conf_matrix_table_red) * 100
ggplot(as.data.frame(conf_matrix_percent_red), aes(Reference, Prediction, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "mediumpurple") +
    labs(x = "Reals", y = "Prediccions", title = "Matriu de confusió - Vi negre") +
    geom_text(aes(label = ifelse(Freq == 0, "0", paste0(ceiling(Freq), "%"))), size = 3) +
    theme(plot.title = element_text(size = 11))

# Data white
url_white <- "https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv"
data_white <- read.csv(url_white, sep=';')

# Exploratory analysis white
str(data_white)

# Preprocessing white
X_white <- data_white[, -which(names(data_white) == "quality")]
y_white <- as.factor(data_white$quality)

# Prediction
y_pred_white <- predict(rf_model, X_white)

# Evaluation white
conf_matrix_white <- confusionMatrix(y_pred_white, y_white)
print(conf_matrix_white)

# Metrics white
accuracy_white <- conf_matrix_white$overall["Accuracy"]
mean_precision_white <- mean(conf_matrix_white$byClass[, "Precision"], na.rm = T)
mean_recall_white <- mean(conf_matrix_white$byClass[, "Recall"], na.rm = T)
cat("Accuracy:", accuracy_white, "\n")
cat("Mean precision:", mean_precision_white, "\n")
cat("Mean recall:", mean_recall_white, "\n")

# Confusion matrix white
conf_matrix_table_white <- as.table(conf_matrix_white)
conf_matrix_percent_white <- prop.table(conf_matrix_table_white) * 100
ggplot(as.data.frame(conf_matrix_percent_white), aes(Reference, Prediction, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "goldenrod") +
    labs(x = "Reals", y = "Prediccions", title = "Matriu de confusió - Vi blanc") +
    geom_text(aes(label = ifelse(Freq == 0, "0", paste0(ceiling(Freq), "%"))), size = 3) +
    theme(plot.title = element_text(size = 11))

# Confusion matrix difference
conf_matrix_diff <- conf_matrix_percent_white - conf_matrix_percent_red
ggplot(as.data.frame(as.table(conf_matrix_diff)), aes(Reference, Prediction, fill = Freq)) +
    geom_tile() +
    scale_fill_gradient2(low = "#8EE5EE", mid = "white", high = "#76EEC6") +
    labs(x = "Reals", y = "Prediccions", title = "Diferència: Blanc - Negre") +
    geom_text(aes(label = ifelse(Freq == 0, "0", sprintf("%+d%%", ceiling(Freq)))), size = 3) +
    theme(plot.title = element_text(size = 11))

# Differences in metrics
cat("Accuracy difference:", accuracy_white - accuracy_red, "\n")
cat("Mean precision difference:", mean_precision_white - mean_precision_red, "\n")
cat("Mean recall difference:", mean_recall_white - mean_recall_red, "\n")
accuracy_drop <- ((accuracy_red - accuracy_white) / accuracy_red) * 100
precision_drop <- ((mean_precision_red - mean_precision_white) / mean_precision_red) * 100
recall_drop <- ((mean_recall_red - mean_recall_white) / mean_recall_red) * 100
cat("Drop in accuracy:", round(accuracy_drop, 2), "%\n")
cat("Drop in precision:", round(precision_drop, 2), "%\n")
cat("Drop in recall:", round(recall_drop, 2), "%\n")

# Covariate drift test
sapply(names(X_test_red), function(feature) {
    ks.test(X_test_red[[feature]], X_white[[feature]])$p.value
})

# Combine the datasets to compare distributions
X_white <- subset(data_white, select = -quality)
X_train_red$dataset <- "Negre"
X_white$dataset <- "Blanc"
combined <- rbind(X_train_red, X_white)

# Standardize variables for comparison
numeric_cols <- sapply(combined, is.numeric)
combined[numeric_cols] <- scale(combined[numeric_cols])

# Reshape the data and draw box plots to compare distributions
combined_long <- pivot_longer(
    combined, cols = -dataset, names_to = "variable", values_to = "value"
)
ggplot(combined_long, aes(x = variable, y = value, fill = dataset)) +
    geom_boxplot() +
    labs(title = "Comparació de distribucions", y = "Valor estandarditzat", fill = "Vi") +
    scale_fill_manual(values = c(Negre = "mediumpurple", Blanc = "goldenrod")) +
    theme(axis.text.x = element_text(angle = 30, hjust = 1))


