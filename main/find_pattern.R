n <- 2019
m <- matrix(nrow = ceiling(sqrt(n)), ncol = ceiling(sqrt(n)))

for(i in 1:ceiling(sqrt(n))^2){
  if(i == 1){
    x <- 1
    y <- 1
    direction <- 'down'
  }else if(direction == 'right'){
    y <- y + 1
    if(x == 1){
      direction <- 'down'
    }else if(y == x){
      direction <- 'up'
    }
  }else if(direction == 'down'){
    x <- x + 1
    if(x == y){
      direction <- 'left'
    }else if(y == 1){
      direction <- 'right'
    }
  }else if(direction == 'left'){
    y <- y - 1
    if(y == 1){
      direction <- 'down'
    }
  }else if(direction == 'up'){
    x <- x - 1
    if(x == 1){
      direction <- 'right'
    }
  }
  
  m[x, y] <- i
}

View(m)
which(m==n, arr.ind = T)
