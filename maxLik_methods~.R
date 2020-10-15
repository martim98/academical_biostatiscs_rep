f <- function(x) {
  res <-exp(x) + exp(-x) - 5 - x
  return(res)
}

expr <- expression(exp(x) + exp(-x) - 5 - x)

first_deriv <- deriv(expr, namevec = "x", func = T)
first_deriv(1)[1]


e <- 1
x_i <- 100
iter <- 1

while(e > 0.001 & iter < 100) {
  x_i1 <- x_i - (f(x_i) / first_deriv(x_i)[1])
  e <- abs(x_i1 - x_i)
  cat('iter: ', iter, 'x_i: ', x_i, 'x_i+1: ', x_i1, 'Error: ', e, '\n', '---------------', '\n')
  x_i <- x_i1
  iter <- iter + 1
}

library(maxLik)
maxNR(f, start = 2)
