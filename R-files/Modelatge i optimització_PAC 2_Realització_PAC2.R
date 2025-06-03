# EXERCICI 5

gradient_descent <- function(a, epsilon=0.0001, lambda=0.01) {

  # Gradient de la funció
  gradient_f <- function(x, y, a) {
    dfdx <- 4*x * (x^2 - a^2)
    dfdy <- 2*y
    return(list(x=dfdx, y=dfdy))
  }
  
  # Valors inicials de x i y aleatoris dins un interval
  x <- runif(1, -2*a, 4*a)
  y <- runif(1, -1, 1)
  
  # Calcula el gradient al punt inicial
  gradient <- gradient_f(x, y, a)
  
  # Itera fins que es superi la condició d'aturada
  while(max(abs(gradient$x), abs(gradient$y)) > epsilon) {
    
    # Actualitza les coordenades del punt
    x <- x - lambda * gradient$x
    y <- y - lambda * gradient$y
    
    # Calcula el gradient al nou punt
    gradient <- gradient_f(x, y, a)
  }
  
  # Punt mínim trobat
  return(list(x=x, y=y))
}

# Punt mínim per a=1
print(gradient_descent(a=1))


# EXERCICI 6

# Recompte de vegades que cau a cada punt segons el valor d'a
a_count <- list(positive=0, negative=0)

a <- 1
iterations <- 1000

# Bucle de 1000 iteracions
for (i in 1:iterations) {
  # Si x≈a (amb la precisió especificada)
  if (abs(gradient_descent(a)$x - a) < 0.0001) {
    a_count$positive <- a_count$positive + 1
  # Si x≈-a (amb la precisió especificada)
  } else {
    a_count$negative <- a_count$negative + 1
  }
}

# Mostra les proporcions
cat("Proporció de vegades que cau a (a,0):", a_count$positive / iterations)
cat("Proporció de vegades que cau a (-a,0):", a_count$negative / iterations)


# EXERCICI 7

# Afegeix cerca de línia a l'exercici 5
gradient_descent <- function(a, epsilon=0.0001, lambda=0.01) {
  
  # Gradient de la funció
  gradient_f <- function(x, y, a) {
    dfdx <- 4*x * (x^2 - a^2)
    dfdy <- 2*y
    return(list(x=dfdx, y=dfdy))
  }
  
  # Valors inicials de x i y aleatoris dins un interval
  x <- -100
  y <- 100
  
  # Calcula el gradient al punt inicial
  gradient <- gradient_f(x, y, a)
  
  # Itera fins que es superi la condició d'aturada
  while(max(abs(gradient$x), abs(gradient$y)) > epsilon) {
    
    # Funció que es vol optimitzar
    f <- function(x, y, a) (x^2 - a^2)^2 + y^2
    # Bucle de la cerca de línia
    while(
      # Condició de continuació
      f(x - lambda * gradient$x, y - lambda * gradient$y, a)
      >
      f(x, y, a) - 0.25 * lambda * (gradient$x^2 + gradient$y^2)
    ) {
      # Actualitza el valor de lambda
      lambda <- 0.5 * lambda
    }
    
    # Actualitza les coordenades del punt
    x <- x - lambda * gradient$x
    y <- y - lambda * gradient$y
    
    # Calcula el gradient al nou punt
    gradient <- gradient_f(x, y, a)
  }
  
  # Punt mínim trobat
  return(list(x=x, y=y))
}

print(gradient_descent(a=1))
