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

for(i in 1:20) {
  print(rmultinom(1, 1, prob = c(0.1, 0.1, 0.4, 0.8)))
}

0 ^1
log(0)
x <- sample

estimator <- function(x) {
  count_0 <- sum(x == 0)
  count_1 <- sum(x == 1)
  count_2 <- sum(x == 2)
  count_3 <- sum(x == 3)
  count_4 <- sum(x == 4)
  
  like_1 <- 0 ^ count_0 + 0.05 ^count_1 + 0.05 ^ count_2 + 0.8 ^ count_3 + 0.1 ^ count_4
  like_2 <- 0.05 ^ count_0 + 0.05 ^count_1 + 0.8 ^ count_2 + 0.1 ^ count_3 + 0 ^ count_4
  like_3 <- 0.9 ^ count_0 + 0.08 ^count_1 + 0.02 ^ count_2 + 0.0 ^ count_3 * 0.0 ^ count_4
  
  cat(like_1, like_2, like_3)
  }

x <- sample(c(0, 1,2, 3), 100, replace = T)
table(x)
estimator(x)
