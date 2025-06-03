setwd("/Users/terenci/Desktop/")

library("dplyr")
library("tidyverse")
library("ggplot2")
library("tidyr")
library("corrplot")
library("clustMixType")

accident_csv <- read.csv('accident.CSV', row.names=NULL, na.strings="")

structure = str(accident_csv)

ggplot(accident_csv, aes(x = factor(STATENAME))) +
    geom_bar(fill = "darkblue") + coord_flip() +
    scale_x_discrete(limits = names(sort(table(accident_csv$STATENAME)))) +
    ggtitle("Nombre d'accidents mortals per estat") +
    xlab("") + ylab("Nombre d'accidents mortals")

ordered_days <- c(
    "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"
)
ggplot(accident_csv,
       aes(x = factor(DAY_WEEKNAME, levels = ordered_days),
           fill = factor(DAY_WEEKNAME, levels = ordered_days))
) +
    geom_bar() +
    ggtitle("Nombre d'accidents mortals segons el dia de la setmana") +
    xlab("Dia de la setmana") +
    ylab("Nombre d'accidents mortals") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    scale_fill_discrete(guide = FALSE)

ggplot(data = accident_csv,
       aes(x = reorder(LGT_CONDNAME, -table(LGT_CONDNAME)[LGT_CONDNAME]),
           fill = LGT_CONDNAME)
) +
    geom_bar() +
    ggtitle("Nombre d'accidents mortals segons les condicions lumíniques") +
    xlab("Condició lumínica") +
    ylab("Nombre d'accidents mortals") + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_fill_discrete(guide = FALSE)

accident_clean <- accident_csv

apply(accident_clean, 2, function(x) sum(is.na(x)))

for (i in 1:nrow(accident_clean)) {
    if (!is.na(accident_clean$TWAY_ID2[i])) {
        accident_clean$TWAY_ID[i] <- paste(
            sort(c(accident_clean$TWAY_ID[i], accident_clean$TWAY_ID2[i])),
            collapse = " & "
        )
    }
}

accident_clean$TWAY_INTERSECTION <- ifelse(is.na(accident_clean$TWAY_ID2), 0, 1)

names(accident_clean)[names(accident_clean) == "TWAY_ID"] <- "TWAY_ID_WITH_INTERSECTIONS"
accident_clean <- subset(accident_clean, select = -TWAY_ID2)

head(accident_clean$TWAY_ID_WITH_INTERSECTIONS, 20)
head(accident_clean$TWAY_INTERSECTION, 20)

any(is.na(accident_clean))

sum(duplicated(accident_clean$ST_CASE))

concatenated_rows <- apply(
    subset(accident_clean, select = -ST_CASE), 1,
    function(row) paste(row, collapse = '')
)
sum(duplicated(concatenated_rows))

numeric_cols <- names(accident_clean)[sapply(accident_clean, is.numeric)]
for (col in numeric_cols) {
    min_val <- min(accident_clean[[col]])
    max_val <- max(accident_clean[[col]])
    cat(paste0(col, " - Min: ", min_val, ", Max: ", max_val, "\n"))
}

original_row_count <- nrow(accident_clean)
accident_clean <- accident_clean %>% filter(
    !(HOUR == 99 | 
          MINUTE == 99 | 
          LATITUDE %in% c(77.7777000, 88.8888000, 99.9999000) | 
          LONGITUD %in% c(777.7777000, 888.8888000, 999.9999000))
)
nrow(accident_clean) / original_row_count *  100

non_numerical_columns <- names(accident_clean)[sapply(accident_clean, is.character)]
unique_single_occurrence_values <- list()
for (col in non_numerical_columns) {
    value_counts <- table(accident_clean[[col]])
    unique_values <- names(value_counts[value_counts == 1])
    if (length(unique_values) > 0) {
        unique_single_occurrence_values[[col]] <- unique_values
    }
}
lapply(unique_single_occurrence_values, function(x) head(x, 10))

accident_clean$PERTOTAL <- accident_clean$PERMVIT + accident_clean$PERNOTMVIT

ggplot(accident_clean, aes(x=PERTOTAL)) + 
    geom_histogram(binwidth=1, fill="darkblue") + 
    ggtitle("Distribució del nou atribut PERTOTAL") +
    xlab("Total de persones involucrades") +
    ylab("Freqüència")

binned <- NA
binned[accident_clean$PERTOTAL <= 3] <- accident_clean$PERTOTAL[accident_clean$PERTOTAL <= 3]
binned[accident_clean$PERTOTAL > 3] <- 4
accident_clean$PERTOTAL_BINNED <- binned
accident_clean %>%
    group_by(PERTOTAL_BINNED) %>%
    summarise(
        minimum = min(PERTOTAL),
        maximum = max(PERTOTAL),
        frequency = n()
    )

accident_clean$PERTOTAL <- NULL

discrete_attributes <- c(
    "VE_TOTAL", "VE_FORMS", "PVH_INVL", "PEDS", "PERSONS", "PERMVIT",
    "PERNOTMVIT", "FATALS", "DRUNK_DR"
)
other_attributes <- c(
    "DAY", "MONTH", "DAY_WEEK", "HOUR", "LATITUDE", "LONGITUD", "PERTOTAL_BINNED"
)
normalized_attributes <- c(discrete_attributes, other_attributes)
accident_clean <- accident_clean %>%
    mutate(across(all_of(normalized_attributes), function(x) {
        return ((x - min(x)) / (max(x) - min(x)))
    }))
head(accident_clean[normalized_attributes], 10)

accident_clean <- accident_clean %>%
    rename(TWAY_ID_WITH_INTERSECTIONS_NAME = TWAY_ID_WITH_INTERSECTIONS) %>%
    mutate(TWAY_ID_WITH_INTERSECTIONS = as.integer(as.factor(TWAY_ID_WITH_INTERSECTIONS_NAME)))

accident_clean %>%
    group_by(TWAY_ID_WITH_INTERSECTIONS) %>%
    summarize(Frequency = n()) %>%
    arrange(-Frequency) %>%
    head(20) %>%
    ggplot(aes(x=reorder(TWAY_ID_WITH_INTERSECTIONS, Frequency), y=Frequency)) +
    geom_bar(stat="identity", fill="darkblue") +
    coord_flip() +
    ggtitle("Carrers i interseccions amb més accidents") +
    xlab("Codi") + ylab("Freqüència")

accident_clean <- accident_clean %>%
    mutate(RAIL = as.integer(as.factor(RAIL)))

accident_numeric <- accident_clean %>%
    select_if(is.numeric)
accident_numeric$DAYNAME <- NULL
accident_numeric$MINUTE <- NULL

str(accident_numeric)

accident_numeric <- accident_numeric %>%
    select(-c("ST_CASE", "NOT_HOUR", "NOT_MIN", "ARR_HOUR", "ARR_MIN", "HOSP_HR", "HOSP_MN"))
ncol(accident_numeric)

accident_condensed <- read.csv('accident_condensed.csv', row.names=NULL)

# Torna a convertir els factors a enters
for(name in names(accident_condensed)) {
    if(name %in% normalized_attributes) next
    accident_condensed[[name]] <- as.integer(accident_condensed[[name]])
}

# Nou dataframe que conté les dades per als diagrames
boxplot_data <- bind_rows(
    accident_numeric %>% mutate(dataset = "Originals"),
    accident_condensed %>% mutate(dataset = "Condensades")
) %>%
    pivot_longer(
        cols = -dataset,
        names_to = "variable",
        values_to = "value"
    ) %>%
    filter(
        variable %in% c("VE_FORMS", "PVH_INVL", "PERMVIT", "PERNOTMVIT", "FATALS", "DRUNK_DR")
    )

# Diagrames de caixa
ggplot(boxplot_data, aes(x = dataset, y = value, fill = dataset)) +
    geom_boxplot() +
    xlab("Dades") + ylab("") +
    ggtitle("Variació en la distribució dels atributs") +
    facet_wrap(~variable) +
    scale_fill_manual(values = c("Condensades" = "red", "Originals" = "blue")) +
    theme(legend.position = "none")

sapply(accident_condensed, sd)

accident_numeric$YEAR <- NULL
accident_condensed$YEAR <- NULL

accident_corr <- accident_numeric[,discrete_attributes]
corrplot(cor(accident_corr), addCoef.col = "black", tl.col = "black")

accident_numeric$PERSONS <- NULL
accident_numeric$PEDS <- NULL
accident_numeric$VE_TOTAL <- NULL
accident_condensed$PERSONS <- NULL
accident_condensed$PEDS <- NULL
accident_condensed$VE_TOTAL <- NULL

normalized_attributes <- c(
    "VE_FORMS", "PVH_INVL", "PERMVIT", "PERNOTMVIT", "FATALS", "DRUNK_DR",
    "DAY", "MONTH", "DAY_WEEK", "HOUR", "LATITUDE", "LONGITUD", "PERTOTAL_BINNED"
)
accident_condensed[, normalized_attributes] <- round(
    accident_condensed[, normalized_attributes], 4
)

str(accident_condensed[normalized_attributes])

# One-hot
onehot_attributes <- !(names(accident_condensed) %in% normalized_attributes)
onehot_names <- names(accident_condensed[, onehot_attributes])
accident_onehot <- accident_condensed[, normalized_attributes, drop=FALSE]
for (attribute_name in onehot_names) {
    accident_condensed[[attribute_name]] <- as.factor(accident_condensed[[attribute_name]])
    onehot_matrix <- model.matrix(~ . - 1, data=accident_condensed[, attribute_name, drop=FALSE])
    for (i in 1:ncol(onehot_matrix)) {
        new_name <- paste(attribute_name, colnames(onehot_matrix)[i], sep="_")
        accident_onehot[[new_name]] <- onehot_matrix[, i]
    }
}
paste("Dimensions originals:", paste(dim(accident_condensed), collapse="x"))
paste("Dimensions one-hot:", paste(dim(accident_onehot), collapse="x"))


discrete_attributes <- c(
    "VE_FORMS", "PVH_INVL", "PERMVIT", "PERNOTMVIT", "FATALS", "DRUNK_DR"
)
pairs(accident_condensed[discrete_attributes])

summary(accident_clean$PERTOTAL_BINNED)

ggplot(accident_condensed, aes(y=VE_TOTAL)) +
    geom_boxplot(fill="blue", alpha=0.7) +
    ggtitle("Box Plot of Normalized VE_TOTAL") +
    xlab("") + ylab("VE_TOTAL (Normalized)")

summary(accident_condensed$LONGITUD)







student <- data.frame("ID alumne" = c(1, 2, 3),
                      "Edat" = c(20, 21, 22),
                      "Genere" = c('M', 'F', 'M'),
                      "Adreça" = c('Carrer 1', 'Carrer 2', 'Carrer 3'))

grades <- data.frame("ID alumne" = c(1, 2, 3),
                     "Any inici" = c(2018, 2019, 2020),
                     "Nota mitjana" = c(7.5, 8.0, 8.5))

joined_data <- student %>%
    inner_join(grades, by = "ID alumne")


