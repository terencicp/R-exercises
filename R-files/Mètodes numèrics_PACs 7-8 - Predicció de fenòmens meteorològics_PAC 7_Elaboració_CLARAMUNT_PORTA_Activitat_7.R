# Activitat 7 - Mètodes numèrics
# Nom: Terenci Claramunt Porta

# Sets current directory as working directory from RStudio
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Funció per llegir les dades
myReadData_byDate = function(file, date_ini, date_fin, column){
	df = read.csv(file, sep = ';', row.names = 1)
	idx_Dates = as.character( seq(as.Date(date_ini, format = '%d/%m/%Y'), as.Date(date_fin, format = '%d/%m/%Y'), 'days') )
	return( na.omit(df[idx_Dates, column]) )
}
file = '0200E-19200101-20181231.csv'


# Exercici 1
avg_temp = myReadData_byDate(file, '01/01/1920', '31/12/2018', 'TMEDIA')
avg_temp_diff = avg_temp[2:length(avg_temp)] - avg_temp[-length(avg_temp)]
print(tail(avg_temp_diff, 10))


# Exercici 2

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

# Probabilitats agregades de cada diferència de temperatures
histogram = hist(avg_temp_diff, prob = TRUE)
breaks = length(histogram$breaks)
t = (histogram$breaks[-breaks] + histogram$breaks[-1]) / 2
y = histogram$density

# Canvi de variable
max_abs_t = max(abs(t))
var_change = pi / max_abs_t
x = t * var_change

# Càlcul dels coefficients amb n=2
fs_coeffs(x, y, n=2)


# Exercici 3

# Punts de la sèrie per representar-la gràficament
fs_y = function(x, coeffs) {
  y = 0
  j = -trunc(length(coeffs) / 2)
  for (k in 1:length(coeffs)) {
    y = y + (coeffs[k] * exp(1i * j * x))
    j = j + 1
  }
  return(Re(y))
}

# Histograma amb 3 aproximacions: n=2, n=5, n=10
t_plot = seq(min(histogram$breaks), max(histogram$breaks), 0.1)
x_plot = t_plot * var_change
fs_dens_2 = fs_y(x_plot, fs_coeffs(x, y, n=2))
fs_dens_5 = fs_y(x_plot, fs_coeffs(x, y, n=5))
fs_dens_10 = fs_y(x_plot, fs_coeffs(x, y, n=10))
hist(avg_temp_diff, prob = TRUE, ylim=c(0, 0.3))
lines(t_plot, fs_dens_2, col = "green4", lwd = 2)
lines(t_plot, fs_dens_5, col = "blue", lwd = 2)
lines(t_plot, fs_dens_10, col = "red", lwd = 2)


# Exercici 4

# Integració usant la fòrmula de Simpson
fs_dist = function(f, b, t, coeffs, var_change) {
  # Fixa el nombre d'intervals d'integració
  m = 20
  # Adapta els límits d'integració a la funció f
  a = min(t) * var_change
  b = b * var_change
  # Calcula els extrems dels intervals
  x.ends = seq(a, b, length.out=m+1)
  y.ends = f(x.ends, coeffs)
  x.mids = (x.ends[2:(m+1)] - x.ends[1:m]) / 2 + x.ends[1:m] 
  y.mids = f(x.mids, coeffs)
  # Transforma els valors de x
  a = a / var_change
  b = b / var_change
  x.ends = x.ends / var_change
  x.mids = x.mids / var_change
  # Calcula l'aproximació de la integral
  p.area = sum(y.ends[2:(m+1)] + 4 * y.mids[1:m] + y.ends[1:m])
  p.area = p.area * abs((b - a) / (2 * m)) / 3
  return(p.area)
}

# Comprovació de l'aproximació
coeffs = fs_coeffs(x, y, n=10)
p_total = fs_dist(fs_y, max_abs_t, t, fs_coeffs(x, y, n=10), var_change)

# Correcció de la funció de densitat
fs_y_corrected = function(x, coeffs) {
  correction = 1 / fs_dist(fs_y, max_abs_t, t, coeffs, var_change)
  return (fs_y(x, coeffs) * correction)
}

# Probabilitat que disminueixi almenys 3 graus
fs_dist(fs_y_corrected, -3, t, coeffs, var_change)
# Probabilitat que varii menys de 4 graus
p_pos_4 = fs_dist(fs_y_corrected, 4, t, coeffs, var_change)
p_neg_4 = fs_dist(fs_y_corrected, -4, t, coeffs, var_change)
p_pos_4 - p_neg_4
# Probabilitat que sigui 2 graus major
1 - fs_dist(fs_y_corrected, 2, t, coeffs, var_change)




