n <- 1e7
m <- unlist(lapply(1:n, function(i){sample(1:52)}))
out <- (which(m %in% c(1,2,3,4)) %% 52)[(1:(n/4))*4-1]
print(mean(out))
