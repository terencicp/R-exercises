# 22.426 - Aplicacions per a la presa de decisions
# Repte 2
# Terenci Claramunt Porta

# ------------------------------------------------------------------------------

# INSTRUCCIONS D'EXECUCIÓ

# Requereix 'medical_insurance.csv' i 'HRDataset_v14.csv' a la mateixa carpeta

# ------------------------------------------------------------------------------

# CONFIGURACIÓ INICIAL

# Instal·lació de les llibreries usades:
if (!require("igraph")) install.packages("igraph", dependencies = TRUE)
if (!require("TSP")) install.packages("TSP", dependencies = TRUE)
if (!require("ggplot2")) install.packages("ggplot2", dependencies = TRUE)
if (!require("corrplot")) install.packages("corrplot", dependencies = TRUE)
if (!require("randomForest")) install.packages("randomForest", dependencies = TRUE)
if (!require("dplyr")) install.packages("dplyr", dependencies = TRUE)
if (!require("lubridate")) install.packages("lubridate", dependencies = TRUE)
if (!require("stringr")) install.packages("stringr", dependencies = TRUE)
if (!require("reshape2")) install.packages("reshape2", dependencies = TRUE)
if (!require("rcompanion")) install.packages("rcompanion", dependencies = TRUE)

# Reinicia l'entorn
rm(list = ls())

# ------------------------------------------------------------------------------

# EXERCICI 1

# 1.A - TSP: Models heurístics

# Crea el graf
g <- graph_from_data_frame(data.frame(
    from =  c('A', 'A', 'A', 'A', 'E', 'E', 'B', 'B', 'D', 'D'),
    to =    c('B', 'E', 'C', 'D', 'B', 'C', 'D', 'C', 'E', 'C'),
    weight = c(7,  10,  13,   2,   5,   4,   9,   3,   11,  8)
),  directed = FALSE)

# Visualitza el graf
plot(g, edge.label = E(g)$weight, edge.width = 2)

# Matriu de distàncies
m <- as.matrix(distances(g, weights = E(g)$weight))
tsp <- TSP(m)

# Funció que calcula la distància total d'un recorregut
tour_distance <- function(tour, distance_matrix) {
    tour_segments <- cbind(tour, c(tour[-1], tour[1]))
    return(sum(distance_matrix[tour_segments]))
}

# Implementació de Nearest Neighbour
nn_tsp <- function(dist_matrix, start) {
    # Inicialització de les variables
    unvisited <- setdiff(colnames(dist_matrix), start) # Nodes a visitar
    visited <- c(start) # Nodes visitats
    current <- start # Node actual
    # Mentre quedin nodes per visitar
    while (length(unvisited) > 0) {
        # Busca el node més proper al node actual
        distances <- setNames(dist_matrix[current, unvisited], unvisited)
        nearest_node <- names(which.min(distances))
        # Va al node més proper (actualitza les variables)
        visited <- c(visited, nearest_node)
        current <- nearest_node
        unvisited <- setdiff(unvisited, nearest_node)
    }
    return(visited)
}

# Implementació de Nearest Insert
ni_tsp <- function(dist_matrix, start) {
    # Nodes a visitar
    unvisited <- setdiff(rownames(dist_matrix), start) 
    # Busca el node més proper al node inicial
    nearest <- unvisited[which.min(dist_matrix[start, unvisited])]
    # Crea un circuit entre el node inicial i el més proper
    visited <- c(start, nearest, start)
    unvisited <- setdiff(unvisited, nearest)
    # Mentre quedin nodes per visitar
    while (length(unvisited) > 0) {
        # Busca el node més proper a qualsevol node del circuit
        distances <- sapply(unvisited, function(node) min(dist_matrix[node, visited]))
        nearest <- unvisited[which.min(distances)]
        # Busca el parell de nodes (a, b) més propers on inserir el node 
        distance_increases <- sapply(1:(length(visited) - 1), function(i) {
            a <- visited[i]
            b <- visited[i + 1]
            dist_matrix[a, nearest] + dist_matrix[nearest, b] - dist_matrix[a, b]
        })
        best_position <- which.min(distance_increases)
        # Insereix el node més proper a la millor posició (entre a i b)
        visited <- append(visited, nearest, best_position)
        unvisited <- setdiff(unvisited, nearest)
    }
    return(visited[-length(visited)]) # Elimina el node final repetit
}

# Funció que genera un cami aleatori ('C', 'B', 'A', 'C')
generate_random_path <- function(n) {
    random_tour <- sample(toupper(letters[1:n]))
    return(c(random_tour, random_tour[1]))
}

# Implementació de 2-opt
two_opt_tsp <- function(distance_matrix, path) {
    # Distància inicial
    best_distance <- tour_distance(path, distance_matrix)
    # Mentre es millori la distància
    improved <- TRUE
    while (improved) {
        improved <- FALSE
        # Per cada parell de nodes (i, j)
        n <- length(path)
        for (i in 2:(n-2)) {
            for (j in (i + 1):(n-1)) {
                # Inverteix el segment entre (i, j)
                new_path <- c(path[1:(i-1)], rev(path[i:j]), path[(j+1):n])
                # Si el circuit és més curt
                new_distance <- tour_distance(new_path, distance_matrix)
                if (new_distance < best_distance) {
                    # Manten el nou circuit
                    path <- new_path
                    best_distance <- new_distance
                    # Torna a explorar tots els parells de nodes
                    improved <- TRUE
                    break # Surt del for interior
                }}
            if (improved) {
                break # Surt del for exterior
            }}}
    return(path[-length(path)]) # Elimina el node final repetit
}

# Mètode heurístic 1: Nearest Neighbour (NN)

# NN usant la llibreria TSP
tour_nn <- solve_TSP(tsp, method = 'nn', start = 1)
labels(tsp)[tour_nn]
tour_length(tsp, tour_nn)

# NN usant la implementació pròpia
tour_nn <- nn_tsp(m, start = 'A')
tour_nn
tour_distance(tour_nn, m)

# Mètode heurístic 2: Nearest Insertion (NI)

# NI usant la llibreria TSP
tour_ni <- solve_TSP(tsp, method = 'nearest_insertion', start = 1)
labels(tsp)[tour_ni]
tour_length(tsp, tour_ni)

# NI usant la implementació pròpia
tour_ni <- ni_tsp(m, start = 'A')
tour_ni
tour_distance(tour_ni, m)

# Mètode heurístic 3: 2-opt usant la implementació pròpia

# Circuit inicial aleatori
random_path <- generate_random_path(5)
random_path[1:5]
tour_distance(random_path, m)

# Circuit optimitzat usant 2-opt
optimal_path <- two_opt_tsp(m, random_path)
optimal_path
tour_distance(optimal_path, m)
  

# ------------------------------------------------------------------------------

# EXERCICI 2

# 2.A - Importació de les dades
df <- read.csv2('medical_insurance.csv')

# Verificació de les variables
str(df)

# 2.B - Anàlisi descriptiva univariant

# Recompte la valors buits per variable
colSums(is.na(df))

# Estadístics descriptius de les variables numèriques
summary(df[sapply(df, is.numeric)])

# Frequències de les variables categòriques
lapply(df[sapply(df, is.character)], table)

# 2.C - Correlació entre d'edat, BMI i cost mèdic
r_age <- round(cor(df$age, df$charges), 2)
r_bmi <- round(cor(df$bmi, df$charges), 2)

title = 'Correlació: Edat i cost mèdic.'
ggplot(df, aes(x = age, y = charges)) +
    geom_point(color = 'deepskyblue4', alpha = 0.3) +
    geom_smooth(method = 'lm', color = 'black') +
    labs(x = 'Edat', y = 'Cost mèdic', title = paste(title, 'r =', r_age))

title = 'Correlació: BMI i cost mèdic.'
ggplot(df, aes(x = bmi, y = charges)) +
    geom_point(color = 'deeppink3', alpha = 0.3) +
    geom_smooth(method = 'lm', color = 'black') +
    labs(x = 'BMI', y = 'Cost mèdic', title = paste(title, 'r =', r_bmi))

# 2.D - Regressió lineal de la variable cost
summary(lm(charges ~ age + sex + bmi + children + smoker + region, df))


# ------------------------------------------------------------------------------
  
# EXERCICI 3
  
library(ggplot2)
library(randomForest)

# Carrega les dades
df <- read.csv2('medical_insurance.csv')

# Correlació entre fumador i cost mèdic
df$smoker <- ifelse(df$smoker == "yes", 1, 0)
round(cor(df$smoker, df$charges), 2)

# Crea una variable binària d'obesitat
df$obesity <- ifelse(df$bmi >= 30, 'Obès', 'No obès')

# Diagrama de caixes de les despeses mèdiques segons si és fumador i obès
ggplot(df, aes(x = smoker, y = charges, fill = obesity)) +
    geom_boxplot() +
    labs(x = 'Fumador', y = 'Despeses mèdiques', fill = 'Obesitat') +
    scale_fill_manual(values = c('No obès' = 'deepskyblue3', 'Obès' = 'tan3'))

# Model de regressió lineal amb interacció
summary(lm(charges ~ age + bmi * smoker, df))

# Model de Random Forest
set.seed(476)
randomForest(charges ~ age + bmi + smoker, df)
  

# ------------------------------------------------------------------------------

# EXERCICI 4

# 4.A - Importa les dades
original_df <- read.csv('HRDataset_v14.csv')

# Comprova les dades
str(df)

# Nivells ordenats de la variable 'PerformanceScore'
levels <- c('PIP', 'Needs Improvement', 'Fully Meets', 'Exceeds')

# Transformació de variables
df <- original_df %>%
    # Reanomena 'Employee_Name' per 'Name'
    mutate(Name = str_trim(Employee_Name)) %>%
    # Nova variable: 'Age', l'edat de l'empleat
    mutate(Age = 2020 - (1900 + as.integer(str_sub(DOB, -2, -1)))) %>%
    # Reanomena 'Termd' per 'Terminated' i converteix a factor
    mutate(Terminated = factor(ifelse(Termd == 1, 'Yes', 'No'))) %>%
    # Reanomena 'PerformanceScore' per 'Performance' i converteix en factor ordenat
    mutate(Performance = factor(PerformanceScore, levels = levels, ordered = TRUE)) %>%
    # Reanomena 'CitizenDesc' i binaritza en 'US Citizen', o 'Non-Citizen'
    mutate(Citizen = factor(ifelse(CitizenDesc == 'US Citizen', 'Yes', 'No'))) %>%
    # Reanomena 'HispanicLatino' per 'Hispanic' i converteix en factor
    mutate(Hispanic = factor(tolower(HispanicLatino))) %>%
    # Reanomena les variables i les converteix en factors
    mutate(across(c( MaritalDesc, RaceDesc), factor)) %>%
    rename(Marital = MaritalDesc, Race = RaceDesc) %>%
    # Converteix les variables categòriques en factors
    mutate(Sex = factor(str_trim(Sex)), Department = factor(str_trim(Department))) %>%
    mutate(across(c(Position, Zip, TermReason, EmploymentStatus,), factor)) %>%
    mutate(across(c(State, ManagerName, RecruitmentSource), factor)) %>%
    # Nova variable: 'DaysEmployed', els dies que l'empleat ha estat a l'empresa
    mutate(
        LastDay = coalesce(mdy(DateofTermination), as.Date('2019-03-01')),
        DaysEmployed = as.integer(difftime(LastDay, mdy(DateofHire), units = 'days'))
    ) %>%
    # Selecciona les variables d'interès
    select(
        Name, Age, Sex, Marital, Salary, Position, Department, ManagerName,
        EmploymentStatus, Terminated, TermReason, DaysEmployed, 
        Performance, EngagementSurvey, EmpSatisfaction,
        SpecialProjectsCount, DaysLateLast30, Absences,
        Citizen, Race, Hispanic, State, Zip, RecruitmentSource
    )

# Estructura de les dades transformades
str(df)

# 4.B - Anàlisi exploratòria

# Histograma de l'edat
ggplot(df, aes(x = Age)) +
    geom_histogram(binwidth = 5, fill = '#3997cd') +
    labs(title = "Distribució de l'edat", x = "Edat", y = "Treballadors")

# Diagrama de barres del gènere
ggplot(df, aes(x = Sex)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per gènere",
         x = "Gènere", y = "Treballadors")

# Diagrama de barres de l'estat civil
ggplot(df, aes(x = Marital)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per estat civil",
         x = "Estat Civil", y = "Treballadors")

# Histograma del salari
ggplot(df, aes(x = Salary)) +
    geom_histogram(binwidth = 10000, fill = '#3997cd') +
    labs(title = "Distribució del salari", x = "Salari", y = "Treballadors")

# Diagrama de barres del rol
ggplot(df, aes(x = reorder(Position, Position, length))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per rol", x = "Càrrec", y = "Treballadors") +
    coord_flip()

# Diagrama de barres del departament
ggplot(df, aes(x = Department)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per departament", y = "Treballadors") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Diagrama de barres dels managers
ggplot(df, aes(x = reorder(ManagerName, ManagerName, length))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per manager",
         x = "Manager", y = "Treballadors") +
    theme(axis.text.x = element_text(angle = 45, hjust = 0.8, vjust = 0.8))

# Diagrama de barres dels treballadors en actiu
ggplot(df, aes(x = EmploymentStatus)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors en actiu",
         x = "Estat", y = "Treballadors") +
    coord_flip()

# Proporcions dels treballadors en actiu
prop.table(table(df$EmploymentStatus))

# Proporcions dels treballadors no actius
df_not_active <- subset(df, EmploymentStatus != "Active")
prop.table(table(df_not_active$EmploymentStatus))

# Diagrama de barres del motiu d'acomiadament
ggplot(df, aes(x = reorder(TermReason, TermReason, length))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per motiu d'acomiadament",
         x = "Motiu", y = "Treballadors") +
    coord_flip()

# Resum del motiu d'acomiadament
table(df$TermReason)

# Histograma de l'antiguetat
ggplot(df, aes(x = DaysEmployed)) +
    geom_histogram(binwidth = 500, fill = '#3997cd') +
    labs(title = "Distribució de l'antiguetat", x = "Dies", y = "Treballadors")

# Diagrama de barres del rendiment
ggplot(df, aes(x = Performance)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per rendiment",
         x = "Rendiment", y = "Treballadors")

# Histograma de l'enquesta de compromís
ggplot(df, aes(x = EngagementSurvey)) +
    geom_histogram(binwidth = 0.5, fill = '#3997cd') +
    labs(title = "Distribució del compromís",
         x = "Puntuació de l'enquesta", y = "Treballadors")

# Diagrama de barres de satisfacció
ggplot(df, aes(x = EmpSatisfaction)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Treballadors per puntuació de satisfacció",
         x = "Satisfacció laboral", y = "Treballadors")

# Diagrama de barres de projectes especials
ggplot(df, aes(x = SpecialProjectsCount)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Treballadors per nombre de projectes especials",
         x = "Projectes especials", y = "Treballadors")

# Diagrama de barres de dies de retard l'últim mes
ggplot(df, aes(x = factor(DaysLateLast30))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per dies de retard l'últim mes",
         x = "Dies de retard", y = "Treballadors")

# Histograma de les absències
ggplot(df, aes(x = Absences)) +
    geom_histogram(binwidth = 5, fill = '#3997cd') +
    labs(title = "Distribució de les absències", x = "Absències", y = "Treballadors")

# Diagrama de barres de ciutadania
ggplot(df, aes(x = Citizen)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per ciutadania",
         x = "El treballador és ciutadà?", y = "Treballadors")

# Diagrama de barres de l'orígen
ggplot(df, aes(x = reorder(Race, Race, length))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per orígen",
         x = "Race", y = "Treballadors") +
    coord_flip()

# Diagrama de barres d'origen llatinoamericà
ggplot(df, aes(x = Hispanic)) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Nombre de treballadors per origen llatinoamericà",
         x = "El treballador és llatinoamericà?", y = "Treballadors")

# Diagrama de barres de la procedència
ggplot(df, aes(x = reorder(RecruitmentSource, RecruitmentSource, length))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Treballadors per procedència",
         x = "Procedència", y = "Treballadors") +
    coord_flip()

# Diagrama de barres per estats
ggplot(df, aes(x = reorder(State, State, length))) +
    geom_bar(color = '#994c2b', fill = '#cd6539') +
    labs(title = "Treballadors per estat",
         x = "Estat", y = "Treballadors") +
    coord_flip()

# 4.C - Estadístics de les variables numèriques
summary(df[sapply(df, is.numeric)])

# Funció de substitució d'outliers per la mediana
outlierKD <- function(dt, var) {
    var_name <- eval(substitute(var),eval(dt))
    tot <- sum(!is.na(var_name))
    na1 <- sum(is.na(var_name))
    par(mfrow=c(2, 2), oma=c(0,0,3,0))
    boxplot(var_name, main="With outliers")
    hist(var_name, main="With outliers", xlab=NA, ylab=NA)
    outlier <- boxplot.stats(var_name)$out
    var_name <- ifelse(var_name %in% outlier, NA, var_name)
    boxplot(var_name, main="Without outliers")
    hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
    title(paste("Outliers in", deparse(substitute(var))), outer=TRUE)
    na2 <- sum(is.na(var_name))
    message("Outliers identified: ", na2 - na1, " from ", tot, " observations")
    message("Proportion (%) of outliers: ", (na2 - na1) / tot*100)
    response <- readline(prompt="Replace outliers with the median? [yes/no]: ")
    if(response == "yes"){
        # Substitueix els outliers per la mediana
        var_fixed <- ifelse(is.na(var_name), median(var_name), var_name)
        dt[[deparse(substitute(var))]] <- var_fixed
        message('Valors extrems substituïts per la mediana')
        return(invisible(dt))
    }
}

# Imputa els outliers de 'Salary'
df_imputed = outlierKD(df, Salary)

# 4.D - Relacions entre variables

# Còpia el dataframe sense la variable 'Name'
df_cat <- df
df_cat$Name <- NULL

# Categoritza les variables contínues usant quantils
for (col in c('Age', 'Salary', 'DaysEmployed', 'EngagementSurvey', 'Absences')) {
    var <- df_cat[[col]]
    breaks = quantile(var, seq(0, 1, by = 0.2))
    df_cat[[col]] <- cut(var, breaks, include.lowest = TRUE)
}

# Converteix la resta de variables numèriques a factors
df_cat <- as.data.frame(lapply(df_cat, as.factor))

# Crea una matriu buida per guardar la V de Cramér
vars <- names(df_cat)
n = length(vars)
m_cramer <- matrix(1, n, n, dimnames = list(vars, vars))

# Calcula la V de Cramér per cada parell de variables
for (i in 1:(length(vars) - 1)) {
    for (j in (i + 1):length(vars)) {
        var1 <- df_cat[[vars[i]]]
        var2 <- df_cat[[vars[j]]]
        m_cramer[i, j] <- cramerV(var1, var2, bias.correct = TRUE)
        m_cramer[j, i] <- m_cramer[i, j]
    }
}

# Converteix la matriu a dataframe per al diagrama
df_cramer <- melt(m_cramer)
names(df_cramer) <- c('var1', 'var2', 'V')

# Diagrama de la V de Cramér per cada parell de variables
ggplot(df_cramer, aes(x = var1, y = var2, fill = V)) +
    geom_tile() +
    scale_fill_gradient2(midpoint = 0.5, low = 'white', mid = 'turquoise3', high = 'black') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = 'V de Cramér (totes les variables)')

# Diagrama del nombre de retards segons el rendiment
ggplot(df, aes(x = Performance, y = DaysLateLast30)) +
    geom_boxplot() +
    labs(title = 'Nombre de retards segons el rendiment',
         x = 'Rendiment', y = 'Retards els últims 30 dies')

# Diagrama de la mitjana de projectes per rol
ggplot(aggregate(SpecialProjectsCount ~ Position, df, mean),
       aes(x = SpecialProjectsCount, y = reorder(Position, SpecialProjectsCount))) +
    geom_bar(stat = 'identity', fill = 'seagreen3') +
    labs(title = 'Mitjana de projectes per rol', x = 'Mitjana de projectes', y = '')

# Diagrama de l'antiguitat dels treballadors segons el manager
ggplot(df[df$Terminated == "No", ],
       aes(x = DaysEmployed, y = reorder(ManagerName, DaysEmployed, median))) +
    geom_boxplot() +
    labs(title = 'Antiguitat dels treballadors segons el manager',
         x = 'Dies', y = 'Manager')

# 4.E - Pregunta: Hi ha problemes de paritat salarial per gèneres?

# Recompte d'homes i dones
table(df$Sex)

# Estadístics del salari per gènere
lapply(split(df$Salary, df$Sex), summary)

# Diagrama de violí del salari per gènere
ggplot(df, aes(x = Sex, y = Salary, fill = Sex)) +
    geom_violin() +
    labs(title = 'Distribució del Salari per Gènere', x = 'Gènere', y = 'Salari')

# Test de la difèrencia entre distribucions del salari per gènere
wilcox.test(Salary ~ Sex, df)

# Diagrama de dispersió del salari per gènere i departament
ggplot(df, aes(x = Department, y = Salary, color = Sex)) +
    geom_jitter(width = 0.3, alpha = 0.5) +
    labs(title = 'Salari per Gènere i Departament', y = 'Salari') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Diagrama de dispersió del salari per gènere i càrrec
ggplot(df, aes(x = Position, y = Salary, color = Sex)) +
    geom_jitter(width = 0.3, alpha = 0.5) +
    labs(title = 'Salari per Gènere i Càrrec', y = 'Salari') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Pregunta: Hi ha departaments salarialment penalitzats?

# Recompte d'empleats per departament
table(df$Department)

# Estadístics del salari per departament
lapply(split(df$Salary, df$Department), summary)

# Diagrames de caixa del salari per gènere
ggplot(df, aes(x = reorder(Department, Salary, median), y = Salary)) +
    geom_boxplot() +
    labs(title = 'Distribució del Salari per Departament', y = 'Salari', x = '')

# Funció que aplica el test Mann-Whitney als salaris de dos departaments
wilcox_test_departments <- function(dept1, dept2) {
    return(wilcox.test(
        Salary ~ Department,
        df[df$Department %in% c(dept1, dept2), ]
    ))
}

# Tests de la difèrencia entre distribucions del salari per departaments
wilcox_test_departments('Production', 'Admin Offices')
wilcox_test_departments('Production', 'Sales')
wilcox_test_departments('Admin Offices', 'Sales')
wilcox_test_departments('IT/IS', 'Software Engineering')

# 4.F - Regressió lineal de 'Salary'

# Simplifica la variable State en dos grups
df$State <- as.factor(ifelse(df$State == 'MA', 'MA', 'Other'))

# Transforma Performance en factor no ordenat
df$Performance <- factor(df$Performance, ordered = FALSE)

# Model lineal amb totes les variables
full_model <- lm(
    Salary ~ . - Name - Terminated - TermReason - Position - ManagerName - Zip,
    df
)

# Model amb selecció de variables
selected_model <- step(full_model, direction = 'backward')
summary(selected_model)

# Elimina salaris més elevats
df <- df[df$Salary <= 125000, ]

# Model lineal amb totes les variables (sense outliers)
no_outliers_model <- lm(
    Salary ~ . - Name - Terminated - TermReason - Position - ManagerName - Zip,
    df
)
summary(no_outliers_model)


# ------------------------------------------------------------------------------
  
# EXERCICI 5

# 5.A - Nombre i proporció d'empleats insatisfets
sum(df$EmpSatisfaction < 4)
mean(df$EmpSatisfaction < 4)
