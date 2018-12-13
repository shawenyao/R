# y(t) is i.i.d random variable
# what's the R-squared for the regression specified by
# 1) y(t) - y(t-1) ~ y(t)
# 2) y(t) - y(t-1) - y(t-2) ~ y(t)

#==== case 1 ====#
n <- 10000
series <- runif(n+1)

# y(t) - y(t-1) ~ y(t)
y <- series[-1] - series[-length(series)]
x <- series[-length(series)]
model <- lm(y~x)
# RSQ ~ 0.5
summary(model)
plot(x,y)


#==== case 2 ====#
n <- 10000
series <- runif(n+2)

# y(t) - y(t-1) - y(t-2) ~ y(t)
y <- series[-c(1:2)] -
  series[-c(length(series),length(series)-1)] -
  series[-c(length(series),1)]
x <- series[-c(length(series),length(series)-1)]
model <- lm(y~x)
# RSQ ~ 0.33
summary(model)
plot(x,y)
