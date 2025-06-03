A <- matrix(c(
  0,0,0,0,9*sqrt(2)/2,
  sqrt(2)/2,0,0,0,0,
  0,0,0,0,9*sqrt(2)/2,
  0,0,0,729*sqrt(2)/2,0,
  sqrt(2)/2,0,0,0,0,
  0,0,6521*sqrt(2)/2,0,0,
  0,0,0,729*sqrt(2)/2,0,
  0,81*sqrt(2)/2,0,0,0,
  0,81*sqrt(2)/2,0,0,0,
  0,0,6521*sqrt(2)/2,0,0
), byrow = TRUE, nrow = 10, ncol = 5)

svd <- svd(A)

Ac <- svd$u[,1:2] %*% diag(svd$d[1:2]) %*% t(svd$v[,1:2])

B <- A-Ac

svd_B <- svd(B)

