get_stats_beta <- function(a, b){
  mean <- a/(a+b)
  var <- (a*b)/(((a+b)^2) * (a +b +1))
  cat('Mean:', mean, 'Var:', var, 'sd:', sqrt(var))
}


get_post_norm <- function(b, d_2, sigma_2, x_bar, n){
  tau <- 1/sigma_2
  c <- 1/d_2
  b_post <- (x_bar * n * tau ) + (c * b)
  b_post <- b_post/(c + n*tau)
  
  d_2_post <- 1/(c + n * tau )
  
  cat('(', b_post, ',', d_2_post, ')')
}

