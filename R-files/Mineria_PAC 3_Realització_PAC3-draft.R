# MINERIA DE DADES - PAC 3

library(ggplot2)
library(dplyr)
library(caret)

# Dades
setwd("/Users/terenci/Desktop/UOC/2 Mineria/PAC 3/Realització")
credit <- read.csv("credit.csv")

# EDA
str(credit)


# Correlations

numerical_vars <- credit[, c("months_loan_duration", "amount", "installment_rate", 
                               "residence_history", "age", "existing_credits", "dependents")]
correlation_matrix <- cor(numerical_vars, use = "complete.obs")
corrplot(correlation_matrix, method = "circle", type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, addCoef.col = TRUE, number.cex = 0.9)


categorical_vars <- c("checking_balance", "credit_history", "purpose", "savings_balance", 
                      "employment_length", "personal_status", "other_debtors", "property", 
                      "installment_plan", "housing", "telephone", "foreign_worker", "job")
cramers_v_results <- matrix(NA, nrow = length(categorical_vars), ncol = length(categorical_vars))
rownames(cramers_v_results) <- colnames(cramers_v_results) <- categorical_vars
for (i in 1:length(categorical_vars)) {
    for (j in 1:length(categorical_vars)) {
        if (i != j) {
            cramers_v_results[i, j] <- CramerV(credit[[categorical_vars[i]]], credit[[categorical_vars[j]]])
        }
    }
}
print(cramers_v_results)


numeric_vars <- credit[c(
    "months_loan_duration", "amount", "installment_rate", 
    "residence_history", "age", "existing_credits", "dependents"
)]
sapply(numeric_vars, function(x) cor(x, credit$default))

library(DescTools)
categorical_vars <- c("checking_balance", "credit_history", "purpose", "savings_balance", 
                      "employment_length", "personal_status", "other_debtors", "property", 
                      "installment_plan", "housing", "telephone", "foreign_worker", "job")
cramers_v_results <- list()
for (var in categorical_vars) {
    cramers_v_results[[var]] <- CramerV(credit[[var]], credit$default)
}
cramers_v_results



# Calculate correlation between AMOUNT and $credit_history and $savings_balance
# to justify their removal

library(DescTools)
credit <- read.csv("credit.csv")
# Bin the numerical variables
credit$months_loan_duration <- cut(credit$months_loan_duration, breaks = 4) # Adjust the number of bins as needed
credit$amount <- cut(credit$amount, breaks = 4) # Adjust the number of bins as needed
credit$age <- cut(credit$age, breaks = 4) # Adjust the number of bins as needed
# Get all variable names
var_names <- names(credit)
# Initialize a matrix to store CramerV values
cramerV_matrix <- matrix(NA, ncol = length(var_names), nrow = length(var_names))
colnames(cramerV_matrix) <- var_names
rownames(cramerV_matrix) <- var_names
# Calculate CramerV for each combination of variables
for (i in 1:length(var_names)) {
    for (j in 1:length(var_names)) {
        cramerV_matrix[i, j] <- CramerV(credit[[var_names[i]]], credit[[var_names[j]]])
    }
}
# View the CramerV matrix
cramerV_matrix

library(reshape2)
cramerV_long <- melt(cramerV_matrix, value.name = "CramerV")

ggplot(cramerV_long, aes(Var1, Var2, fill = CramerV)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", CramerV)), size = 2) +
    scale_fill_gradient2(low = "white", high = "black", mid = "blue4", 
                         midpoint = 0.55, limit = c(0,0.7), name="Cramer's V") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_blank())



sorted_values <- sort(cramerV_matrix["default", ], decreasing = TRUE)
sorted_cols <- names(sorted_values)
for (i in seq_along(sorted_cols)) {
    cat(paste(round(sorted_values[i], 2), sorted_cols[i], "\n"))
}



# Preparació

credit <- read.csv("credit.csv")
categories_to_aggregate <- c("repairs", "others", "domestic appliances", "retraining")
credit$purpose[credit$purpose %in% categories_to_aggregate] <- "others"
credit$credit_history[credit$credit_history %in% c("fully repaid this bank", "fully repaid")] <- "fully repaid"
columns_to_keep <- c(
    "default",
    "checking_balance",
    "credit_history",
    "savings_balance",
    "purpose",
    #"months_loan_duration",
    "amount",
    "property",
    "employment_length",
    #"housing",
    "age"
)
credit <- credit[, columns_to_keep]
credit_subset <- credit %>%
    mutate_if(is.character, as.factor) %>%
    mutate(default = as.factor(default))

# Separa els predictors (x) de la variable objectiu (y)
x <- credit_subset[, !(names(credit_subset) %in% "default")]
y <- credit_subset[, "default"]

# Mostra aleatòria d'índexs
set.seed(615)
indexes = sample(1:nrow(credit_subset), size = nrow(credit_subset) * 2/3)

# Conjunt d'entrenament
trainx <- x[indexes, ]
trainy <- y[indexes]

# Conjunt de prova
testx <- x[-indexes, ]
testy <- y[-indexes]

summary(trainx)
summary(testx)
table(trainy)
table(testy)

# Decision tree
library(C50)
library(grid)
model <- C5.0(trainx, trainy, rules = TRUE)
model2 <- C5.0(trainx, trainy)
summary(model)
summary(model2)
plot(model2, gp = gpar(fontsize = 5))



# test
filtered_df <- subset(credit, checking_balance == "1 - 200 DM" & amount > 9572)
# Count the frequencies of each category in the "default" variable
default_counts <- table(filtered_df$default)

# Calculate the proportions
default_proportions <- prop.table(default_counts)

# Print the counts and proportions
print(default_counts)
print(default_proportions)

