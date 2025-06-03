# Console output width
options(width=280)

# Data
setwd("~/Desktop/UOC/Data/")
project_path <- "2 Scripting/practica/"
file_path <- paste(project_path, "googleplay_clean.csv", sep = "")
csv <- read.csv(file_path, sep = "," , header = TRUE)

# Cleaning
csv <- csv[csv$Type != "", ]

# Nom dels camps:
# 1 App, 2 Category, 3 Rating, 4 Reviews, 5 Size, 6 Installs, 7 Type, 8 Price,
# 9 Content Rating, 10 Genres, 11 Last Updated, 12 Current Ver, 13 Android Ver

# App
head(csv$App, 10)
# Category
unique(csv$Category)
# Rating
paste(min(csv$Rating), max(csv$Rating), sep=", ")
# Reviews
paste(min(csv$Reviews), max(csv$Reviews), sep=", ")
# Size
paste(min(csv$Size), max(csv$Size), sep=", ")
csv[csv$Size > , ]
# Installs
unique(csv$Installs)
# Type
unique(csv$Type)
# Price
unique(csv$Price)
# Content.Rating
unique(csv$Content.Rating)
# Last Updated
paste(min(csv$Last.Updated), max(csv$Last.Updated), sep=", ")
# Current Ver
unique(csv$Current.Ver)
rows <- grep("[[:digit:]]", csv$"Current.Ver", invert = TRUE)
csv[rows, "Current.Ver"]
# Android Ver
unique(csv$Android.Ver)


# ANALYSIS

# Ratings by categories
ratings_cat <- aggregate(csv$Rating, by=list(Category=csv$Category), FUN=mean)
ratings_cat <- ratings_cat[order(ratings_cat$x, decreasing = TRUE),]
ratings_cat

# App size distribution
hist(csv$Size)

# Content Rating distribution
table(csv$Content.Rating)

# Versioning
ver_types <- gsub("[0-9]+", "#", csv$Current.Ver)
ver_types <- gsub("[a-zA-Z]+", "a", ver_types)
head(sort(table(ver_types), decreasing = TRUE), 20)


# -------

# Type %
table(csv$Type) / nrow(csv) * 100

# Months since last update
months_past <- numeric(length(csv$Last.Updated))
for (i in 1:length(csv$Last.Updated)) {
  date <- csv$Last.Updated[i]
  month <- as.numeric(substr(date, 6, 7))
  year <- as.numeric(substr(date, 1, 4))
  years_past <- 2018 - year
  months_past[i] <- if(year==2018) 8-month else (years_past*12)-month+8
}
hist(months_past)

