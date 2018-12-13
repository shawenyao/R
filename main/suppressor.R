# Can brining in a new variable make a previously non-significant coefficient estimate siginificant?
# Yes!

library(tidyverse)

set.seed(350)

n <- 100
x <- runif(n, 0, 1) * 10
a <- 3
b <- 2
epsilon <- rnorm(n, 0, 1)
y <- a + b * x + epsilon

# introduce a little noise to avoid a perfect fit
epsilon2 <- epsilon + rnorm(n, 0, 0.5)

# insiginificant coefficient estimate on variable epsilon2
lm(y~epsilon2) %>% summary()

# siginificant coefficient estimate on variable epsilon2
lm(y~x + epsilon2) %>% summary()
