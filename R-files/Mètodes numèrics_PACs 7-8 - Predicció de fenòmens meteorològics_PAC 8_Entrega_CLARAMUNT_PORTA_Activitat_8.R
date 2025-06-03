# Clear the workspace
rm(list = ls())

# Sets current directory as working directory from RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


####### Funcions #######

# Realitza un ajust lineal polinòmic de grau "n". Retorna els coeficients.
mypolyfit = function(x, y, degree){
  Phi = matrix(1, length(x), degree+1)
  for (i in 1:degree) {
    Phi[, i+1] = x**i
  }
	PhiT = t(Phi)
	c = solve(PhiT %*% Phi, PhiT %*% y)
	return(c)
}

# Avalua un ajust polinòmic definit pels coeficients "c" en els punts "x".
mypolyeval = function(x, coeffs){
	y = 0
	for (i in 1:length(coeffs)) {
	  y = y + coeffs[i] * x**(i-1)
	}
	return(y)
}

# Calcula els coeficients de la sèrie de Fourier
fs_coeffs = function(x, y, n) {
  M = length(x) - 1
  coeffs = c()
  for (k in seq(-n,n)) {
    c_coeff = sum(y * exp(-1i * k * x)) / (M + 1)
    coeffs = append(coeffs, c_coeff)
  }
  return(coeffs)
}

# Punts de la sèrie de Fourier per representar-la gràficament
fs_y = function(x, coeffs) {
  y = 0
  j = -trunc(length(coeffs) / 2)
  for (k in 1:length(coeffs)) {
    y = y + (coeffs[k] * exp(1i * j * x))
    j = j + 1
  }
  return(Re(y))
}

# Calcula el coeficient de determinació, R^2
coeff_det = function(y, y_fit) {
  Sr = sum((y - y_fit) ^ 2)
  y_mean = mean(y)
  St = sum((y - y_mean) ^ 2)
  return((St - Sr) / St)
}

# Realitza un ajust lineal multiple. Retorna els coeficients.
mymultifit = function(y, x_list){
  Phi = matrix(1, length(x_list[[1]]), length(x_list)+1)
  for (i in 1:length(x_list)) {
    Phi[,i+1] = x_list[[i]]
  }
  PhiT = t(Phi)
  return(solve(PhiT %*% Phi, PhiT %*% y))
}

# Avalua un ajust multiple definit pels coeficients "c" en els punts "x".
mymultieval = function(x_list, coeffs){
  y = rep(coeffs[1], length(x_list[[1]]))
  for (k in 2:length(coeffs)) {
    for (i in 1:length(x_list[[k-1]])) {
      y[i] = y[i] + (coeffs[k] * x_list[[k-1]][i]) 
    }
  }
  return(y)
}


####### Lectura de dades #######

myReadData_byDate = function(file, date_ini, date_fin, column){
  df = read.csv(file, sep = ';', row.names = 1)
  idx_Dates = as.character(seq(as.Date(date_ini, format = '%d/%m/%Y'), as.Date(date_fin, format = '%d/%m/%Y'), 'days'))
  return( na.omit(df[idx_Dates, column]) )
}
file = '0200E-19200101-20181231.csv'


####### Exercici 1 - Tendència de temperatures #######

### Dades
start = '01/01/1926'
end = '31/12/2018'
max_y = myReadData_byDate(file, start, end, 'TMAX')
avg_y = myReadData_byDate(file, start, end, 'TMEDIA')
min_y = myReadData_byDate(file, start, end, 'TMIN')

### Tasca 1.1

# Anàlisi exploratori - Gràfics de les temperatures anuals
par(mfrow=c(1,3))
temp = list(MAX=max_y, AVG=avg_y, MIN=min_y)
for (i in 1:3) {
  groups = split(temp[[i]], ceiling(seq_along(temp[[i]]) / 365.25))
  sums = sapply(groups, sum) / 365.25
  plot(1:length(sums), sums, type="l", main=names(temp)[i],
       ylab="Temperatura mitjana", xlab="Anys", xaxt = "n")
  axis(1, at=seq(0, 80, by=20), labels=seq(1926, 2018, by=20))
}

## Regressió lineal polinòmica
# Temperatura màxima
max_x = 1:length(max_y)
max_lm = lm(max_y ~ max_x + I(max_x^2))
# Temperatura mitjana
avg_x = 1:length(avg_y)
avg_lm = lm(avg_y ~ avg_x + I(avg_x^2))
# Temperatura mínima
min_x = 1:length(min_y)
min_lm = lm(min_y ~ min_x + I(min_x^2))

## Representació gràfica
par(mfrow=c(1,1))
title = "Regressió polinòmica de la temperatura màxima, mitjana i mínima"
plot(avg_x, avg_y, pch='.', main=title, xlab="Dia", ylab="Temperatura", ylim=c(0,30), xaxs="i")
# Temperatura màxima
max_fit = mypolyeval(max_x, max_lm$coefficients)
lines(max_x, max_fit, col="red", lwd=3)
# Temperatura mitjana
avg_fit = mypolyeval(avg_x, avg_lm$coefficients)
lines(avg_x, avg_fit, col="green4", lwd=3)
# Temperatura mínima
min_fit = mypolyeval(min_x, min_lm$coefficients)
lines(min_x, min_fit, col="blue", lwd=3)
# Etiquetes, llegenda i eixos
lm_labels = c("Màxima", "Mitjana", "Mínima")
legend("bottom", lm_labels, lty=1, lwd=3, col = c("red", "green4", "blue"))
y_ticks <- seq(-5, 30, by = 5)
axis(side = 2, at = y_ticks)
axis(side = 4, at = y_ticks)

### Tasca 1.2
# Diferència en la temperatura màxima
max_bounds = predict(max_lm, data.frame(max_x=c(1, length(max_x))))
max_bounds[2] - max_bounds[1]
# Diferència en la temperatura mitjana
avg_bounds = predict(avg_lm, data.frame(avg_x=c(1, length(avg_x))))
avg_bounds[2] - avg_bounds[1]
# Diferència en la temperatura mínima
min_bounds = predict(min_lm, data.frame(min_x=c(1, length(min_x))))
min_bounds[2] - min_bounds[1]

### Tasca 1.3
Jan_1_2100 = 63554
# Predicció de la temperatura màxima del 2100
pred_bounds = predict(max_lm, data.frame(max_x=c(Jan_1_2100)))
# Predicció de la temperatura mitjana del 2100
predict(avg_lm, data.frame(avg_x=c(Jan_1_2100)))
# Predicció de la temperatura mínima del 2100
predict(min_lm, data.frame(min_x=c(Jan_1_2100)))


####### Exercici 2 - Estacionalitat #######

### Dades
temp_max_y = myReadData_byDate(file, '01/01/2018', '31/12/2018', 'TMAX')
temp_max_x = 1:length(temp_max_y)

### Tasca 2.1
# Model 1: Regressió lineal polinòmica
coeffs_polynom = mypolyfit(temp_max_x, temp_max_y, degree=2)
# Model 2: Polinòmis trigonomètrics - Sèrie de Fourier
temp_max_x_transf = temp_max_x * 2 * pi / max(temp_max_x)
coeffs_fourier = fs_coeffs(temp_max_x_transf, temp_max_y, n=2)

### Tasca 2.2
# Gràfic dels dos models sobre les dades
plot(temp_max_x, temp_max_y, xlab="Dia", ylab="Temperatura màxima")
# Model 1: Regressió lineal polinòmica
m1_fit = mypolyeval(temp_max_x, coeffs_polynom)
lines(temp_max_x, m1_fit, col="red", lwd=3)
# Model 2: Polinòmis trigonomètrics - Sèrie de Fourier
m2_fit = fs_y(temp_max_x_transf, coeffs_fourier)
lines(temp_max_x, m2_fit, col="blue", lwd=3)

### Tasca 2.3
# Coeficient de determinació del Model 1: Regressió lineal polinòmica
coeff_det(temp_max_y, mypolyeval(temp_max_x, coeffs_polynom))
# Coeficient de determinació del Model 2: Polinòmis trigonomètrics - Sèrie de Fourier
coeff_det(temp_max_y, fs_y(temp_max_x_transf, coeffs_fourier))


####### Exercici 3 - Influència d'altres factors #######

### Lectura de dades
start = '01/01/2018'
end = '31/12/2018'
temp = myReadData_byDate(file, start, end, 'TMEDIA')
h_sol = myReadData_byDate(file, start, end, 'SOL')
precip_mm = myReadData_byDate(file, start, end, 'PRECIPITACION')

### Tasca 2
coeffs_multi = mymultifit(temp, list(h_sol, precip_mm))
print(coeffs_multi)

### Tasca 3
h_sol_pred = c(6)
precip_mm_pred = c(20)
mymultieval(list(h_sol_pred, precip_mm_pred), coeffs_multi)

