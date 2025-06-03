# Transforma l'element pivot en 1 (pivot_row, pivot_col)
# i la resta d'elements de la columna pivot en 0 (pivot_col)
pivot <- function(M, pivot_row, pivot_col) {
    if (M[pivot_row, pivot_col] == 0) {
        non_zero_row <- which(M[, pivot_col] != 0)[1]
        non_zero_value <- M[non_zero_row, pivot_col]
        M[pivot_row,] <- M[pivot_row,] + (M[non_zero_row,] / non_zero_value)
        return(M)
    }
    M[pivot_row,] <- M[pivot_row,] / M[pivot_row, pivot_col]
    for(i in 1:nrow(M)) {
        if (i != pivot_row) {
            M[i,] <- M[i,] - M[i, pivot_col] * M[pivot_row,]
        }}
    return(M)
}

# Genera la taula inicial
setup_tab <- function(FO, R, TI) {
    M <- matrix(0, nrow=nrow(R)+1, ncol=ncol(R)+nrow(R)+1)
    M[1, 1:length(FO)] <- FO
    M[2:(nrow(R)+1), 1:ncol(R)] <- R
    M[2:(nrow(R)+1), (ncol(R)+1):(ncol(R)+nrow(R))] <- diag(nrow(R))
    M[2:(nrow(R)+1), ncol(M)] <- TI
    return(M)
}

# Desa les files de les variables de la base en un diccionari
save_tab_as_dict <- function(tab, basis, entering_var, leaving_var) {
    tab_dict <- list()
    for (j in 2:nrow(tab)) {
        tab_dict[[basis[j-1]]] <- tab[j,]
    }
    tab_dict[[entering_var]] <- tab_dict[[leaving_var]]
    return(tab_dict)
}

# Coeficients de la funció objectiu
FO <- c(-0.09, -0.04, -0.1, -0.05, -0.05)
# Restriccions i termes independents de les restriccions
R <- matrix(c(
    1, 1, 1, 1, 1,
    1, 0, 1, 0, 0,
    -1, 0, 0, 0, 0,
    0, -1, 0, 0, 0,
    0, 0, -1, 0, 0,
    0, 0, 0, -1, 0,
    0, 0, 0, 0, -1
), nrow=7, byrow=TRUE)
TI <- c(100, 50, -10, -25, -10, -10, -10)
# Vèrtex inicial
initial_vertex <- c(40, 25, 10, 10, 15)

# Taula que conté FO, R i TI
tab <- setup_tab(FO, R, TI)

# Indexs de les variables que formen la base inicial
slack_basis <- which((R %*% initial_vertex) != TI)
basis <- c(1:length(FO), slack_basis + length(FO))

# Recalcula la taula segons les variables que formen part de la base
while (!sum(tab[, basis]) == length(basis)) {
    for (i in 1:length(basis)) {
        tab <- pivot(tab, i+1, basis[i])
    }
}

# Iteració de l'algoritme simplex
optimum = FALSE
while (!optimum) {
    
    # Índex de la variable que entra a la base
    entering_candidates <- tab[1, 1:(ncol(tab)-1)]
    entering_candidates[basis] <- 0
    entering_var <- which.min(entering_candidates)
    
    # Condició de finalització
    if (min(entering_candidates) >= 0) {
        optimum = TRUE
        cat("z:", tab[1, ncol(tab)], "\n")
        cat("Solució:", tab[,ncol(tab)][1:length(FO)+1])
    }
    
    # Índex de la variable que surt de la base
    leaving_candidates <- tab[-1, ncol(tab)] / tab[-1, entering_var]
    positive_candidates <- ifelse(leaving_candidates <= 0, Inf, leaving_candidates)
    leaving_var <- basis[which.min(positive_candidates)]
    
    # Desa les files de les variables de la base en un diccionari
    tab_dict <- save_tab_as_dict(tab, basis, entering_var, leaving_var)
    
    # Actualitza els índexs de les variables de la base
    basis <- sort(c(basis[basis != leaving_var], entering_var))
    
    # Actualitza la taula amb la nova variable que entra
    updated_tab = matrix(tab[1,], nrow=1, ncol=ncol(tab))
    for (b in basis) {
        updated_tab = rbind(updated_tab, tab_dict[[b]])
    }
    entering_row_index <- which(basis == entering_var) + 1
    tab <- pivot(updated_tab, entering_row_index, entering_var)
}