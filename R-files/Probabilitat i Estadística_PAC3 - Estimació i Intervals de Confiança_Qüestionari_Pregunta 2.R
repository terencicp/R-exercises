uniform <- seq(from = 2, to = 3, length.out = 1000)

m <- 10000
prop <- c()

for (j in 1:m) {
  n <- 100
  means <- c()
  
  for (i in 1:n) {
    samples <- sample(uniform, 100, replace=T)
    means[i] <- sum(samples) / n
  }
  
  higher <- sum(means > 2.53, na.rm = TRUE)
  
  prop[j] <- higher / n
}

sum(prop)/ m