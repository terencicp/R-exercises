# Mètodes numèrics - PAC 5
# Terenci Claramunt Porta


# parametres
S0 = 110
K = 110
r = 0.03
sigma = 0.25
T = 1


# 1.1

# d1
d = (log(S0 / K) + (r + 1/2 * sigma^2) * T) / (sigma * sqrt(T))

# valor exacte de delta
vd = pnorm(d)

# valor exacte de gamma
vg = dnorm(d) / (sigma * sqrt(T) * S0)


# 1.2

# model de Black-Scholes
BS = function(S) {
  d1 = ((log(S / K) + (r + 1/2 * sigma^2) * T)) / (sigma * sqrt(T))
  d2 = d1 - (sigma * sqrt(T))
  return((S * pnorm(d1)) - (exp(-r * T) * K * pnorm(d2)))
}

# primera derivada numerica (delta)
findiff = function(f, x, h) {
  return((f(x) - f(x - h)) / h)
}

# segona derivada numerica (gamma)
findiff2 = function(f, x, h) {
  return((f(x + h) - 2 * f(x) + f(x - h)) / (h^2))
}


# valor aproximat de la delta per a h=0.1 i errors
vdapprox1 = findiff(BS, S0, 0.1)
abserrdh1 = abs(vd - vdapprox1)
relerrdh1 = abserrdh1 / abs(vd)

# valor aproximat de la delta para h=0.001 i errors
vdapprox2 = findiff(BS, S0, 0.001)
abserrdh2 = abs(vd - vdapprox2)
relerrdh2 = abserrdh2 / abs(vd)

# valor aproximat de la gamma per a h=0.1 i errors
vgapprox1 = findiff2(BS, S0, 0.1)
abserrgh1 = abs(vg - vgapprox1)
relerrgh1 = abserrgh1 / abs(vg)

# valor aproximat de la gamma per a h=0.01 i errors
vgapprox2 = findiff2(BS, S0, 0.001)
abserrgh2 = abs(vg - vgapprox2)
relerrgh2 = abserrgh2 / abs(vg)


# representacio error absolut per a delta
vec_h = c(0.1, 0.01, 0.001, 0.0001)

# Calcula els errors absoluts per cada h
errors = vector("numeric", 4)
for (i in 1:4) {
  errors[i] = abs(vd - findiff(BS, S0, vec_h[i]))
}

# Mostra el gràfic amb etiquetes als eixos
plot(1:4, errors, type = "b", xaxt="n", ylab="Error absolut", xlab="h")
axis(1, at = seq(1, 4, by = 1), labels = vec_h)


