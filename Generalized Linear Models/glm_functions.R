log_reg <- function(model){
    cat('Logistic')
    coefs <- model$coefficients
    n<- length(coefs)
    
    odds_ratio <- c()
    for(i in 1:n){
      odds_ratio[i] <- exp(coefs[i])
    }
    return(odds_ratio)

  }
    
