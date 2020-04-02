# Coins with values 1 through N (inclusive) are placed into a bag. All the coins from the bag are iteratively drawn (without replacement) at random. For the first coin, you are paid the value of the coin. For subsequent coins, you are paid the absolute difference between the drawn coin and the previously drawn coin. For example, if you drew 5,3,2,4,1, your payments would be 5,2,1,2,3 for a total payment of 13.
# 
# Please provide 5 digits of precision for all answers.

library(dplyr)
library(parallel)

set.seed(100)

N <- 10
# N <- 20
no_threads <- 4
iteration <- 2e7

# simulate one path
get_total_payment <- function(i){
  sequence <- c(0, sample(1:N, replace = FALSE))
  payment <- abs(sequence[-length(sequence)] - sequence[-1])
  sum(payment)
}

# multi-core simulation
cl <- makeCluster(no_threads)
clusterExport(cl, varlist = c("N"))
total_payment_distribution <- parLapply(cl, 1:iteration, get_total_payment) %>% 
  unlist()
stopCluster(cl)

# find answers
mean(total_payment_distribution)
sd(total_payment_distribution)
sum(total_payment_distribution >= 45) / iteration
# sum(total_payment_distribution >= 160) / iteration
