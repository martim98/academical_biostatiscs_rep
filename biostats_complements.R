x <- rbinom(20, 100, 0.5)

hist(x, breaks = 10)
plot(density(x))
length(x)


likelihood_theta <- function(data){
  n1 <- sum(x == 1)
  n2 <- sum(x == 2)
  n3 <- sum(x == 3)
  
  expression <- 0.1^n1 * 1.1^n2 * 17.1^n3
  
  if(expression > 1){
    return(1)
  } else {
    return(0)
  }
}

vetor <- c()
for(i in 1:100000) {
  x <- sample(1:3, 20, replace = T)
  likelihood_theta(x)
  vetor[i] <- likelihood_theta(x)
}

tab <- table(vetor)
prop.table(tab)
