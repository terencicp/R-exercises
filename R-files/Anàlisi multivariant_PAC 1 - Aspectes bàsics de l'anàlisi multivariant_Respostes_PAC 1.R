# PAC 1 - Aspectes bàsics d'anàlisi multivariant

setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/pac1/"


# 3. California Test Score Data

file_path <- paste(project_path, "CASchools.csv", sep = "")
CASchools <- read.table(file_path, sep = "," , header = TRUE)

# a) la relació entre el nombre de professors i el nombre d’ordinadors

# La correlació entre les dues variables és 0.93, per tant, podem afirmar
# que hi ha una correlació positiva molt forta entre les variables

cor(CASchools$teachers, CASchools$computer)

# A partir del diagrama de dispersió i de la linia de regressió -en color blau-,
# podem observar que hi ha una relació
# lineal positiva entre les dues variables: quant més professors té un districte,
# més ordinadors té. Cal remarcar que encara que hi ha una associació entre les dues 
# variables no hi ha necessariament una relació de causalitat; probablement hi ha una
# variable de confusió, el nombre d'alumnes, que està associat amb el nombre de professors i 
# és la causa real del nombre d'ordinadors.

plot(CASchools$teachers, CASchools$computer,
     main = "Diagrama de dispersió",
     xlab = "Teachers", ylab = "Computer")
abline(lm(computer ~ teachers, data = CASchools), col = "blue")

# b) la descripció de les dues variables anteriors

# Les dues variables contenen 420 observacions.

# La majoria de districtes són petits
# ja que la mediana de professors es de 48.56. Tot i això les dades tenen una cua
# cap a la dreta, que es fa evident en la mitjana de 129.07, molt més alta que la mediana,
# per tant hi ha districtes que tenen molts més professors que la majoria.
# Aquesta cua també es pot observar al diagrama de caixa, on la majoria de valors estan
# concentrats a la part inferior del diagrama i conté molts valors atípics representats
# en forma de punts a la part superior.

length(CASchools$teachers)
summary(CASchools$teachers)
boxplot(CASchools$teachers, main = "Diagrama de caixa", xlab = "Teachers")

# Com és d'esperar ja que les dues variables tenen una correlació molt alta,
# la distribució de la variable computer és molt similar a la de teachers,
# amb una mediana de 117.5 i una mitjana de 303.4. També és interessant observar
# que hi ha escoles sense ordinadors, ja que el mínim de la variable és 0.

length(CASchools$computer)
summary(CASchools$computer)
boxplot(CASchools$computer, main = "Diagrama de caixa", xlab = "Computer")


# 4. Grunfeld's Investment Data
# a) la relació entre el capital i l’empresa (firma)

# Podem visualitzar les dades mitjançant un gràfic de barres que mostri
# el capital de les empreses ordenades segons el capital

# Carrega les dades en un data frame
file_path <- paste(project_path, "Grunfeld.csv", sep = "")
Grunfeld <- read.table(file_path, sep = ",", header = TRUE)
# Elimina el separador dels milers
Grunfeld$capital <- Grunfeld$capital * 1000

# Carrega les llibreries necessàries pel gràfic
library(ggplot2)
library(dplyr)

# Evita la notació científica al gràfic
options(scipen=10000)
# A partir del data frame
Grunfeld %>%
  # Agrupa les dades per empresa
  group_by(firm) %>%
  # Calcula la mitjana del capital durant el periode d'estudi
  summarise(mean = mean(capital), n = n()) %>%
  # Crea un gràfic de barres horitzontal i defineix els detalls
  ggplot(aes(x = reorder(firm, mean), y = mean)) +
  geom_bar(stat="identity", fill="darkblue", width=.5) +
  coord_flip() +
  ggtitle("Mitjana del capital entre 1935 i 1954") +
  xlab("") +
  ylab("capital") +
  theme_bw()





