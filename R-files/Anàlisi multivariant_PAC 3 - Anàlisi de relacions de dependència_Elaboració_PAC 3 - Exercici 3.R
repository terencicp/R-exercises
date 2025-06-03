# Anàlisi multivariant - PAC 3

# Exercici 3

library(dplyr)
library(ggplot2)
library(olsrr)

# Console output width
options(width=180)

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac3/"
file_path <- paste(project_path, "Ej3.csv", sep = "")
csv <- read.table(file_path, sep = "," , header = TRUE)

# Variable selection
data <- csv %>%
  select(salary_in_usd, remote_ratio, work_year, employment_type, company_size, experience_level)

# Dummy variables
salaries <- data
salaries$remote_ratio <- factor(salaries$remote_ratio)
salaries$work_year <- factor(salaries$work_year)

# Box plots by categories
ggplot(salaries, aes(x=salary_in_usd, y=remote_ratio)) + geom_boxplot()
ggplot(salaries, aes(x=salary_in_usd, y=work_year)) + geom_boxplot()
ggplot(salaries, aes(x=salary_in_usd, y=employment_type)) + geom_boxplot()
ggplot(salaries, aes(x=salary_in_usd, y=company_size)) + geom_boxplot()
ggplot(salaries, aes(x=salary_in_usd, y=experience_level)) + geom_boxplot()

# Levels
for (i in 2:ncol(data)) { print(unique(data[i])) }

# Group means
salaries %>% group_by(experience_level) %>%
  summarise_at(vars(salary_in_usd), list(name = mean))
salaries %>% group_by(company_size) %>%
  summarise_at(vars(salary_in_usd), list(name = mean))
salaries %>% group_by(remote_ratio) %>%
  summarise_at(vars(salary_in_usd), list(name = mean))
salaries %>% group_by(employment_type) %>%
  summarise_at(vars(salary_in_usd), list(name = mean))

# Manual model selection
# Step 1
for (i in 2:6) {
  model <- lm(salary_in_usd ~ salaries[[i]], data=salaries)
  print(paste(colnames(salaries)[i], round(summary(model)$adj.r.squared, 4)))
}
model1 <- lm(salary_in_usd ~ experience_level, data=salaries)
confint(model1, level=0.95)
# Step 2
for (i in 2:5) {
  model <- lm(salary_in_usd ~ experience_level + salaries[[i]], data=salaries)
  print(paste(colnames(salaries)[i], round(summary(model)$adj.r.squared, 4)))
}
model2 <- lm(salary_in_usd ~ experience_level + company_size, data=salaries)
confint(model2, level=0.95)
anova(model1, model2)["Pr(>F)"][2,]
# ...

# Automatic variable selection
mlr <- lm(salary_in_usd ~ remote_ratio + work_year + employment_type + company_size + experience_level, data=salaries)
ols_step_forward_p(mlr)

# Chosen model 
model <- lm(salary_in_usd ~ experience_level + company_size + remote_ratio + employment_type, data=salaries)
coefficients(model)
