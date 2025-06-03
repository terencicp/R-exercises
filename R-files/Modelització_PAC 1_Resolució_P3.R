# a)

5

# b)

49

# c)

f <- function(α) { (1 - exp(-1/α) - 0.1)}
uniroot(f, c(0.1, 100))$root