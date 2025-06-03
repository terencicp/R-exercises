# Anàlisi multivariant - PAC 3

# Exercici 1

library(dplyr)
library(ggplot2)

# Console output width
options(width=180)

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac3/"
file_path <- paste(project_path, "Ej1.csv", sep = "")
csv <- read.table(file_path, sep = "," , header = TRUE)

# Variable selection
scores <- csv %>%
  select(math.score, parental.level.of.education, lunch, test.preparation.course)

# Levels
for (i in 2:ncol(scores)) { print(unique(scores[i])) }

# Cell counts (Unbalanced data)
scores %>% count(parental.level.of.education, lunch, test.preparation.course)

# Math score distribution
x <- scores$math.score
h<-hist(x, breaks=10, col="red") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)

# Box plots by categories
ggplot(scores, aes(x=math.score, y=parental.level.of.education)) + geom_boxplot()
ggplot(scores, aes(x=math.score, y=lunch)) + geom_boxplot()
ggplot(scores, aes(x=math.score, y=test.preparation.course)) + geom_boxplot()

# Plot means
scores$parental.level.of.education <- factor(scores$parental.level.of.education, levels = c("some high school", "high school", "some college", "associate's degree", "bachelor's degree", "master's degree"))
scores$test.preparation.course <- factor(scores$test.preparation.course, levels = c("none", "completed"))
library(dplyr) 
library(ggplot2)
groups <- group_by(scores, parental.level.of.education, lunch, test.preparation.course)
plot_data <- summarise(groups, mean = mean(math.score))
ggplot(plot_data, aes(x=parental.level.of.education, y=mean, fill=test.preparation.course )) +
  geom_bar(stat="identity", position=position_dodge()) +
  facet_wrap(~lunch)

# Group normality
library(beeswarm)
beeswarm(
  math.score ~  parental.level.of.education + lunch + test.preparation.course,
  data=scores,
  col = c("blue", "red", "darkgreen", "orange")
)

# Skewness & Kurtosis (normality)
library(moments); library(dplyr)
groups <- group_split(scores, parental.level.of.education, lunch, test.preparation.course)
skew <- vector("numeric", 24); kurt <- vector("numeric", 24)
for (i in 1:24) {
  skew[i] <- round(skewness(groups[[i]]$math.score), 2)
  kurt[i] <- round(kurtosis(groups[[i]]$math.score), 2) }
cat("Skewness:", skew, "\n"); cat("Kurtosis:", kurt)

## Q-Q plot (normality)
library(dplyr)
par(mfrow=c(6,4))
groups <- group_split(scores, parental.level.of.education, lunch, test.preparation.course)
for (group in groups) {
  title = paste(group$parental.level.of.education[[1]], group$test.preparation.course[[1]], group$lunch[[1]])
  qqnorm(group$math.score, main=title, cex.main=0.65)
  qqline(group$math.score, col = "blue") }

# Levene test (homogeneity of variance)
car::leveneTest(math.score ~ parental.level.of.education*lunch*test.preparation.course, data=scores)


# Type-II Factorial ANOVA
model <- aov(
  math.score ~  parental.level.of.education + lunch + test.preparation.course,
  data=scores)
summary(model)
car::Anova(model, type=2)

# Contrastos per parells
pairwise.t.test(scores$math.score, scores$parental.level.of.education, p.adjust.method="bonferroni")
# Creuats
scores$group <- paste(scores$test.preparation.course, "-", scores$lunch)
pairwise.t.test(scores$math.score, scores$group, p.adjust.method="bonferroni")

