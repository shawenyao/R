library(MASS)

x <- matrix(
  c(
    10, 0,
    20, 0,
    0, 90,
    0, 50,
    -30, 40
  ),
  ncol = 2,
  byrow = TRUE
)
y <- matrix(
  c(
    8, 
    19,
    70,
    30,
    0
  ), 
  ncol = 1
)

z <- x / abs(x)
z[is.nan(z)] <- 0

1 / (ginv(x) %*% y)

1 / (solve(t(z) %*% x) %*% t(z) %*% y)
