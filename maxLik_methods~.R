f <- function(x){
  return(exp(x) + exp(-x) - 5 - x)
}

f_deriv <- function(x) {
  return(exp(x) - exp(-x) - 1)
}


x0 <- 2
a <- 's'
while(a == 's'){
  x1 <- x0 - f(x0)/f_deriv(x0)
  x0 <- x1
  print(x1)
  a <- as.character(readline(prompt = 'Continue?: '))
}
