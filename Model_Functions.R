#################################################
## Black Scholes Function #######################
#################################################

Putcall <- function(putcall, S, K, r, sigma, t_cur, t_mat = 262){  #t_mat length of time series
  t <- (t_mat - t_cur)/ 365

  #functions to compute the d1 and d2 of the closed form BS equation

  D1 <- function(S, K, r, sigma, t){
    denom <- sigma * sqrt(t)
    a <- log(S/K)
    b <- (r + sigma^2 / 2)*t
    return( (a + b) / denom)
  }

  D2 <- function(S, K, r, sigma, t){
    d1 <- D1(S, K, r, sigma, t)
    return(d1 - sigma*sqrt(t))
  }

  d1 <- D1(S, K, r, sigma, t)
  d2 <- D2(S, K, r, sigma, t)

  if(putcall == "CALL"){
    price <- pnorm(-d2) * K * exp(-r*T) - pnorm(-d1)*S
  }
  else if(putcall == "PUT"){
    price <- pnorm(d1)*S - K * exp(-r*T) * pnorm(d2)
  }
  else{
    price <- NA
  }

  return(price)
}
