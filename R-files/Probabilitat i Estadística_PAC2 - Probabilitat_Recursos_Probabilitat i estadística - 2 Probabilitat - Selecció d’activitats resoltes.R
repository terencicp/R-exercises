# Probabilitat - Selecció d'activitats resoltes

# Data
setwd("~/Desktop/UOC/Data")
project_path <- "1 Estadística/pac2/exercicis/"
# tcpu
file_path_tcpu <- paste(project_path, "ActR01TCPU.csv", sep="")
tcpu <- read.table(file_path_tcpu, skip=3, sep=";", header=TRUE)
# ticm
file_path_ticm <- paste(project_path, "ActR01TICM.csv", sep="")
ticm <- read.table(file_path_ticm, skip=4, dec=",", sep=";", header=TRUE)
# ham
file_path_ham <- paste(project_path, "ActR03HAM.csv", sep="")
ham <- read.table(file_path_ham, skip=0, sep=";", header=TRUE)
# tc
file_path_tc <- paste(project_path, "ActR03TC.csv", sep="")
tc <- read.table(file_path_tc, skip=0, sep=";", header=TRUE)
# test
file_path_test <- paste(project_path, "ActR03TEST.csv", sep="")
test <- read.table(file_path_test, skip=0, sep=";", header=TRUE)


# ACTIVITAT 1

# New variable (Even = 0, Odd = 1)
SISOP <- round(100*tc$OBS, 0) %% 2
# New variable (Grouped: Continuous to categorical)
TEMPAGRUP <- cut(tc$OBS,
               breaks=c(4.51, 4.51+0.231, 4.51+2*0.231, 4.51+3*0.231),
               labels=c("TBA", "TME", "TALT"), right=FALSE)
# Data Frame with new variables
DADES1 <- data.frame(tc$OBS, SISOP, TEMPAGRUP)
# Contingeny table + Row and column totals
TA <- table(SISOP, TEMPAGRUP)
TAM <- addmargins(TA)
TAM # Not displayed properly in RStudio Data Viewer


# ACTIVITAT 2

# New variables (Grouped: Continuous to categorical)
NIVBA <- cut(ticm$PBA, breaks=c(0,30,40,50,100), right=FALSE,
             labels=c("1_baix", "2_suf", "3_med", "4_bue"))
NIVEMAIL <- cut(ticm$PEMAIL, breaks=c(0,30,40,50,100), right=FALSE,
              labels=c("1_baix", "2_suf", "3_med", "4_bue"))
# Data Frame with new variables
DADES2 <- with(ticm, data.frame(PBA, PEMAIL, NIVBA, NIVEMAIL))
# Contingeny table + Row and column totals
TABAEMAIL <- with(ticm, addmargins(table(NIVBA,NIVEMAIL)))
TABAEMAIL # Not displayed properly in RStudio Data Viewer


# ACTIVITAT 3

# Dades
SPAM <-     c(0, 1, 2,3,4,5,6,7)
EMPLEATS <- c(7,11,10,7,1,2,1,1)
# Repeat SPAM values EMPLEATS times
OBSSPAM <- rep(SPAM,EMPLEATS)
# New variable (Grouped: Discrete to categorical)
NUMSPAMS <- cut(OBSSPAM, breaks=c(-1, 2.5,5.5, 7.5), right=FALSE,
                labels=c("A","B","C"))
# New variable (Grouped + Same label over multiple intervals: "SI")
FINESTRES3 <- cut(OBSSPAM, breaks=c(0, 0.5 ,6.5, 8), right=FALSE,
                labels=c("SI","NO","SI2"))
FINESTRES <- droplevels(replace(FINESTRES3, FINESTRES3=="SI2", "SI"))
# Data frame from new variables
DADES3 <- data.frame(OBSSPAM, NUMSPAMS, FINESTRES)
# Contingency table
TAVAN <- addmargins(table(NUMSPAMS, FINESTRES))
TAVAN # Not displayed properly in RStudio Data Viewer


# ACTIVITAT 4

# New variable(Grouped: Continuous to categorical)
CLASTEMP <- cut(tcpu$TCPU, breaks=,c(47,81,114,150), right=FALSE,
                labels=c("T_curt", "T_mitjà", "T_llarg"))
# New variable (Grouped using quartiles)
CLASCODI <- with(tcpu,cut(LCOD, quantile(LCOD,
                 probs = c(0,0.25, 0.5, 0.75,1)), include.lowest=TRUE,
                 labels=c("Csupercurt","Ccurt","Cllarg","Csuperllarg")))
# Data frame
DADES4 <- data.frame(tcpu$TCPU, tcpu$LCOD, CLASTEMP, CLASCODI)
# Contingency table
TAULATEMPSCODI <- addmargins(table(CLASTEMP, CLASCODI))
TAULATEMPSCODI # Not displayed properly in RStudio Data Viewer


# ACTIVITAT 5

# Variable sums
S1 <- sum(test$TEST1)
S2 <- sum(test$TEST2)
S3 <- sum(test$TEST3)
# New Variable: 1 when TEST1 and TEST2 are 1, else 0
TEST12 <- test$TEST1 * test$TEST2
S12 <- sum(TEST12)
# Same as before, for TEST3
TEST13 <- Act5$TEST1 * Act5$TEST3
TEST23 <- Act5$TEST2 * Act5$TEST3
TEST123 <- Act5$TEST1 * Act5$TEST2 * Act5$TEST3
S13 <- sum(TEST13)
S23 <- sum(TEST23)
S123 <- sum(TEST123)


# ACTIVITAT 6

# Dades
Java <- c(50.27,50.51,49.58,50.09,50.27,50.72,49.22,50.09,51.37,49.93,50.58, 48.92,50.21,49.81,50.69,48.50,48.77,48.56,49.39,50.79,48.67,51.03, 49.73,52.20,50.32)
Especialitat <- c("Gestió","Sistemes","Gestió","Sistemes","Gestió","Sistemes", "Sistemes","Sistemes","Gestió","Gestió","Gestió","Gestió", "Gestió","Gestió","Gestió","Gestió","Gestió","Gestió", "Gestió","Gestió","Sistemes","Sistemes","Sistemes","Sistemes", "Sistemes")
# Maximum value
SJ <- summary(Java)
# New variable
NJ <- cut(Java,breaks=,c(0,50,52,55), right=FALSE,
        labels=c("baix","mitjà","alt"))
# Contingency table
TEJ <- addmargins(table(Especialitat,NJ))
TEJ # Not displayed properly in RStudio Data Viewer


# ACTIVITAT 7

# Dades
NCOR<-c(2, 1, 0, 0, 1, 1, 1, 2, 0, 1, 0, 0, 0, 0, 0, 1, 2, 0, 1, 2, 0, 0, 0, 2,
        0, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 2, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0)
# New var using repetition
TIPUS <- rep(c("Nit","Matí","Tarda"), c(30,10,10))
# Contingency table
TCD <- addmargins(table(NCOR,TIPUS))
TCD # Not displayed properly in RStudio Data Viewer


# ACTIVITAT 8

# Contingency table
TED <- addmargins(table(ham$ENTRA, ham$DETECTA))



