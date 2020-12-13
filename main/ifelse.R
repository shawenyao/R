# test if the "ifelse" function evaluates the statements lazily
# (the answer is no)

test1 <- function(v){
  print(paste0("test1 + ", v))
}

test2 <- function(v){
  print(paste0("test2 + ", v))
}

a <- ifelse(c(TRUE, FALSE), test1(1:2), test2(1:2))
