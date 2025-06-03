# Anàlisi multivariant - Pràctica - Exercici 1

# Console output width
options(width=380)

# Font de les dades
# https://vincentarelbundock.github.io/Rdatasets/datasets.html

# Each variable is a summary of various questions about:

# esupport - extent of emotional support sources
# psupport - extent of practical support sources
# supsources - extent of social support sources

# tangible - availability of tangible support (money)
# tangiblesat - satisfaction with tangible support

# emotional - emotional support availability (psych)
# emotionalsat - emotional support satisfaction

# affect - availability of affectionate support sources (initimate rel.?)
# affectsat - satisfaction with affectionate support sources

# psi - availability of positive social interaction
# psisat - satisfaction with positive social interaction

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "2 Anàlisi multivariant/practica/"
file_path <- paste(project_path, "socsupport.csv", sep = "")
csv <- read.csv(file_path, sep = "," , header = TRUE)

# Data subset + Remove NAs
df <- na.omit(csv[,10:20])

# Correlation (show barlett's test of sphericity)
library(psych)
cortest.bartlett(cor(df), n = nrow(df))

# FA, Choose n of factors using SS Loadings based on Kaiser's rule >1 (Show SS loadings)
fa(df, nfactors = ncol(df) - 1, rotate = "oblimin")

# FA, 5 factors (show diagram)
fa_5 <- fa(df, nfactors = 5, rotate = "oblimin")
fa.diagram(fa_5)

