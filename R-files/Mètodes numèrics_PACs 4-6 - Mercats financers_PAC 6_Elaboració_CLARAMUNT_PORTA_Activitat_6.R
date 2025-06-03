#parametres opcio
S=110
K=110
r=0.04
sigma=0.4
T=0.5
  
d1 = (log(S/K)+(r+0.5*sigma^2)*(T)) / (sigma*sqrt(T))
d2 = d1 - sigma*sqrt(T)

phi = function(x) {
  return((1 / sqrt(2 * pi)) * exp(-x^2 / 2))
}


#formula de Hastings
PhiH = function(x) {
  PhiHpos = function(x) {
    a1 = 0.319381530
    a2 = -0.356563782
    a3 = 1.781477937
    a4 = -1.821255978
    a5 = 1.330274429
    alpha = 0.2316419
    t = 1 / (1 + alpha * x)
    Phitilde = 1 - phi(x) * (a1*t + a2*t^2 + a3*t^3 + a4*t^4 + a5*t^5)
    return(Phitilde)
  }
  if(x >= 0) return(PhiHpos(x))
  else return(1 - PhiHpos(-x))
}


# 1.1

#regla dels trapezis
trap = function(f, a, b, m) {
  x = seq(a, b, length.out=m+1)
  y = f(x)
  p.area = sum((y[2:(m+1)] + y[1:m]))
  p.area = p.area * abs((b-a)/m) / 2 # h/2
  return(p.area)
}

PhiT = function(x, m) {
  PhiTpos = function(x) {
    return((1/2) + trap(phi, 0, x, m))
  }
  if(x >= 0) return(PhiTpos(x))
  else return(1 - PhiTpos(-x))
}

PhiT_d1 = PhiT(d1, 4)
PhiT_d2 = PhiT(d2, 4)


# 1.2

#regla de Simpson
simp = function(f, a, b, m) {
  x.ends = seq(a, b, length.out=m+1)
  y.ends = f(x.ends)
  x.mids = (x.ends[2:(m+1)] - x.ends[1:m]) / 2 + x.ends[1:m] 
  y.mids = f(x.mids)
  p.area = sum(y.ends[2:(m+1)] + 4 * y.mids[1:m] + y.ends[1:m])
  p.area = p.area * abs((b - a) / (2 * m)) / 3 # h/3
  return(p.area)
}

PhiS=function(x, m) {
  PhiSpos = function(x) { return((1/2) + simp(phi, 0, x, m)) }
  if(x >= 0) return(PhiSpos(x))
  else return(1 - PhiSpos(-x))
}

PhiS_d1 = PhiS(d1, 4)
PhiS_d2 = PhiS(d2, 4)


# 1.3

#Monte Carlo
mcint = function(f,a,b,m) {
  set.seed(4)
  x = runif(m, min=a, max=b)
  y.hat = f(x)
  area = ((b-a) / m) * sum(y.hat)
  return(area)
}

PhiMC = function(x, m) {
  PhiMCpos=function(x) { return((1/2) + mcint(phi, 0, x, m)) }
  if(x>=0) return(PhiMCpos(x))
  else return(1-PhiMCpos(-x))
}

PhiMC_d1 = PhiMC(d1, 1000)
PhiMC_d2 = PhiMC(d2, 1000)


#preu de la call
BScall = function(FUN, m=NULL) {
  d1 = (log(S/K)+(r+0.5*sigma^2)*(T)) / (sigma*sqrt(T))
  d2 = d1 - sigma*sqrt(T)
  if (is.null(m)) {
    call = (S*FUN(d1)) - (exp(-r*T)*K*FUN(d2))
  } else {
    call = (S*FUN(d1,m)) - (exp(-r*T)*K*FUN(d2,m))
  }
  return(call)
}

vPhiH=BScall(PhiH) #valor amb la Phi de Hastings
vPhiT=BScall(PhiT, 4) #valor amb la Phi de trapezis
vPhiS=BScall(PhiS, 4) #valor amb la Phi de Simpson
vPhiMC=BScall(PhiMC, 1000) #valor amb la Phi de Monte Carlo


# 1.4

#errors
abserrT = abs(vPhiT - vPhiH)
relerrT = abserrT / vPhiH

abserrS = abs(vPhiS - vPhiH)
relerrS = abserrS / vPhiH

abserrMC = abs(vPhiMC - vPhiH)
relerrMC = abserrMC / vPhiH


# Gràfics

# Regla dels trapezis - Diferències
n=10
diff = vector("numeric", n)
for (i in 1:n) {
  first = BScall(PhiT, i)
  second = BScall(PhiT, i+1)
  diff[i] = first - second
}
plot(1:n, diff, type = "b", ylab="diferències")

# Regla de Simpson - Diferències
n=10
diff = vector("numeric", n)
for (i in 2:n) {
  first = BScall(PhiS, i)
  second = BScall(PhiS, i+1)
  diff[i] = first - second
}
plot(1:n, diff, type = "b", ylab="diferències")

# Mètode de Monte Carlo - Valor de l'opció
n = 7
m = 10^(1:n)
va = vector("numeric", n)
for (i in 1:n) {
  va[i] = BScall(PhiMC, m[i])
}
plot(1:n, va, type = "b", xaxt="n", ylab="aproximació de v", xlab="m")
axis(1, at = seq(1, n), labels = m)


# Elimina variables de l'entorn
remove(i)
remove(n)
remove(m)
remove(diff)
remove(va)
remove(first)
remove(second)
