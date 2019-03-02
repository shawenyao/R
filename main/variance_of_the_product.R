N <- 10000

x <- matrix(
  rnorm(N * 3),
  ncol = 3
)

sigma <- matrix(
  c(
    1, 0.5, 0.5,
    0.5, 1, 0.5,
    0.5, 0.5, 1
  ),
  byrow = TRUE,
  ncol = 3
)

x_dash <- x %*% chol(sigma)

# mean & variance of the sum
mean(x_dash[,1] + x_dash[,2] + x_dash[,3])
var(x_dash[,1] + x_dash[,2] + x_dash[,3])
sum(cov(x_dash))

# mean & variance of the product
mean(x_dash[,1] * x_dash[,2] * x_dash[,3])
var(x_dash[,1] * x_dash[,2] * x_dash[,3])

variance <- x_dash %*% cov(x_dash) %*% t(x_dash)
