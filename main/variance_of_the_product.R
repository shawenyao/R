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

# correlated correlated standard normal random variables
x_dash <- x %*% chol(sigma)

# mean & 
mean(x_dash[,1] + x_dash[,2] + x_dash[,3])
# formula for mean of the sum
mean(x_dash[,1]) + mean(x_dash[,2]) + mean(x_dash[,3])

# variance of the sum
var(x_dash[,1] + x_dash[,2] + x_dash[,3])
# formula for variance of the sum
sum(cov(x_dash))


# mean of the product
mean(x_dash[,1] * x_dash[,2] * x_dash[,3])
# variance of the product
var(x_dash[,1] * x_dash[,2] * x_dash[,3])
