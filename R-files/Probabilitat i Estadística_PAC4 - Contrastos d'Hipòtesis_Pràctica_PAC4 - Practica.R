# PAC 4 INFÈRENCIA ESTADÍSTICA II: CONTRAST D’HIPÒTESIS

# Exercici 2

# Dades
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac4/practica/"
file_path <- paste(project_path, "demora.csv", sep = "")
dades <- read.table(file_path, sep = "," , header = TRUE)

# a) Hem llegit que de mitjana els individus triguen 5 hores (300 minuts)
# des de l’inici dels símptomes fins arribar a l’hospital. Feu un
# test d’hipòtesis amb la nostra mostra per decidir si aquesta informació
# és certa amb una confiança del 95%.

demora <- dades$demora
t.test(demora, alternative = 'two.sided', mu = 300, conf.level = .95)

# b) Feu un test, amb una confiança del 95%, per decidir si la demora és diferent
# segons si l’aparició dels símptomes es produeixen durant el dia o durant la nit.

demora_split_noche <- split(dades, dades$noche)
demora_dia <- demora_split_noche[[1]]
demora_nit <- demora_split_noche[[2]]
t.test(demora_dia, demora_nit, var.equal = TRUE, mu = 0, conf.level = 0.95)

# c) Feu un test, amb una confiança del 95%, per decidir si la demora és diferent
# segons el sexe del pacient.

demora_split_sexo <- split(dades, dades$sexo)
demora_home <- demora_split_sexo[[1]]
demora_dona <- demora_split_sexo[[2]]
t.test(demora_home, demora_dona, var.equal = TRUE, mu = 0, conf.level = 0.95)