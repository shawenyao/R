#' simulate geometric Brownian motion paths
#' @param mu expected return
#' @param sigma volatility
#' @param dW a matrix of rnorms, useful when simulating with correlated processes
#' @param price0 the initial price
generate_paths <- function(mu, sigma, dW, price0, dt){
  
  drift <- (mu - 0.5 * sigma^2) * dt
  diffusion <- sigma * sqrt(dt) * dW
  
  log_price_increment <- cbind(0, drift + diffusion)
  log_price <- log(price0) + t(apply(log_price_increment, 1, cumsum))
  
  exp(log_price)
}


#' simulate the returns of Uniswap liquidity providers
#' based on asset pair A and B
#' @param mu_A the expected return of asset A
#' @param mu_B the expected return of asset B
#' @param sigma_A the volatility of asset A
#' @param sigma_B the volatility of asset B
#' @param rho the correlation between asset A and B's returns
#' @param fee the fee rate
#' @param N the number of paths to simulate
#' @param t the number of iterations per path
#' @param dt the step size
#' @param A0 the initial price of asset A
#' @param B0 the initial price of asset B
#' @param a0 the initial pool size of asset A
#' @param b0 the initial pool size of asset B (a0 * A0 / B0)
simulate_uniswap_returns <- function(
  mu_A, mu_B,
  sigma_A, sigma_B,
  rho,
  fee,
  N, t, dt,
  A0, B0,
  a0, b0 = a0 * A0 / B0
){
  
  # simulate price paths for asset A and B
  dW_A <- matrix(rnorm(t / dt * N), nrow = N, ncol = t)
  dW_B <- rho * dW_A + sqrt(1 - rho^2) * matrix(rnorm(t / dt * N), nrow = N, ncol = t)
  
  A <- generate_paths(
    mu = mu_A, 
    sigma = sigma_A, 
    price0 = A0,
    dW = dW_A,
    dt = dt
  )
  B <- generate_paths(
    mu = mu_B, 
    sigma = sigma_B, 
    price0 = B0,
    dW = dW_B,
    dt = dt
  )
  
  # pool size
  a <- matrix(a0, nrow = N, ncol = t + 1)
  b <- matrix(b0, nrow = N, ncol = t + 1)
  
  # total fee income
  total_f_a <- matrix(0, nrow = N, ncol = t + 1)
  total_f_b <- matrix(0, nrow = N, ncol = t + 1)
  
  # check the correlation in log price
  # sapply(seq_len(N), function(i){cor(log(A[i,]), log(B[i,]))}) %>% summary()
  
  # evolution of pool balances
  for(i in 2:(t+1)){
    
    a_prime <- sqrt(a[,i-1] * b[,i-1] * B[,i] / A[,i])
    b_prime <- sqrt(a[,i-1] * b[,i-1] * A[,i] / B[,i])
    
    f_a <- (a[,i-1] >= a_prime) * (a[,i-1] - a_prime) * fee
    f_b <- (a[,i-1] < a_prime) * (b[,i-1] - b_prime) * fee
    
    a[,i] <- a_prime + f_a
    b[,i] <- b_prime + f_b
    
    total_f_a[,i] <- total_f_a[,i-1] + f_a
    total_f_b[,i] <- total_f_b[,i-1] + f_b
  }
  
  # initial value
  v0 <- unique(a[,1] * A[,1] + b[,1] * B[,1])
  
  # strategy: buy and hold - terminal value
  v_bah <- a[,1] * A[,t] + b[,1] * B[,t]
  
  # strategy: provide liquidity
  #terminal value
  v_liquidity <- a[,t] * A[,t] + b[,t] * B[,t]
  # fee income
  total_f <- total_f_a[,t] * A[,t] + total_f_b[,t] * B[,t]
  
  v <- tibble(
    `Buy and Hold` = v_bah / v0 - 1,
    `Liquidity Provider` = v_liquidity / v0 - 1,
    `Total Fees` = total_f / v0
  )
}
