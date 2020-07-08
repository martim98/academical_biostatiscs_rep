x <- seq(0, 4, 0.01)
plot(x, (1/(x^2)), type = 'l', ylim = c(0, 1))

n <- 20
x_bar <- 3.8

a_priori  <- 0.04
b_priori <- 0.2


a_post <- a_priori + n
b_post <- b_priori + n*x_bar

x <- seq(0.01, 0.7, 0.001)

plot(x, dgamma(x, a_post, b_post), type = 'l')
abline(v = 0.1366023)
abline(v = 0.4399279)


pgamma(0.3, 20.04, 76)

library(TeachingDemos)

qgamma(0.005, 20.04, 76)
qgamma(0.995, 20.04, 76)


sum(x)
sum(x^2)
152 - 30

122 / 8
8  ^2

sum(x^2)


16193 - 22.5 * 30
15518/29




(30/25) *22.5 + 20/100


27.2 /(1/100 + 30/25)

1/(1/100 + 30/25)

mu_post <- 22.47934
var_post <-  0.8264463

x <- seq(18, 27, 0.01)

pnorm(24, mu_post, sqrt(var_post), lower.tail =  F)

plot(x, dnorm(x, mu_post, sqrt(var_post)), type = 'l')
lines(x, dnorm(x, mu_post, sqrt(var_post + 25/1)))

library(TeachingDemos)


qnorm(0.025, mu_post, var_post)
qnorm(0.975, mu_post, var_post)

hpd(qnorm, mean =  mu_post, sd =var_post)

pnorm(25, mu_post, sqrt(var_post + 25/1), lower.tail = F)

N <- 5000
count <- 0
for(i in 1:N){
  mu_pred <- rnorm(1, mu_post, sqrt(var_post))
  mu_f <- rnorm(1, mu_pred, 5)
  if(mu_f > 25){
    count <- count + 1
  }
}

var_pred <- var_post + 25/10


sd <- sqrt(var_pred)

qnorm(0.25, mu_post, var_pred)
qnorm(0.5, mu_post, var_pred)
qnorm(0.75, mu_post, var_pred)

x <-c(1, 2, 1, 6, 4, 3, 6, 7)
var(x)


sum((x - mean(x))^2) /7
b <- sum(x^2) - (mean(x) * 8)
b/7

sum(x^2) - sum(x)
sqrt(122)/2
var(x)
16/(16+6)



get_stats_beta(16, 6)


library(TeachingDemos)

hpd(qbeta, shape1 = 16, shape2 = 6, conf = 0.99)

qbeta(0.005, 16, 6)
qbeta(0.995, 16, 6)


pbeta(0.5, 16, 6, lower.tail = F)



pbeta(0.5, 2, 2, lower.tail = T)


x <- seq(0.01, 0.99, 0.01)

plot(x, dgamma(x, 1, 1), type = 'l')

rep(0.5, 10)

qgamma()
hpd(qgamma, shape = 24, rate = 8, conf = 0.99)
1/2
count <- 0
N <- 10000
for(i in 1:N){
  lambda <- rgamma(1, 24, 8)
  x <- rpois(1, lambda)
  if(x == 4){
    count <- count + 1
  }
}

count/N

beta(3, 19)

get_stats_beta(3, 37)
get_stats_beta(3, 19)
get_stats_beta(1, 19)

  
qbeta(0.05, 1, 19)
qbeta(0.95, 1, 19)


qbeta(0.05, 3, 37)
qbeta(0.95,3, 37)


prior <- function(theta){
  y <- theta^(-1/2) + (1-theta)^(-1/2)
  return(y)
}

x <- seq(0.001, 0.999, 0.001)  
plot(x, prior(x), type = 'l')


plot(x, dbeta(x, 1, 19), type = 'l', col = 'red')
lines(x, dbeta(x, 1/2, 1/2), col = 'green')
lines(x, dbeta(x, 3, 39 ), col = 'blue')
lines(x, dbeta(x, 3,19), col = 'orange')


qbeta(0.05, 3, 19)
qbeta(0.95,3, 19)

a <- c(2.99,3.03,3.04,3.01,3.12,2.98,3.03,2.98,3.07,3.10)
mean(a)

get_post_norm(2.5, 0.25, 0.0025, 3.035, 10)

x <- seq(0, 300, 0.1)

plot(x, dnorm(x, 3.034466, sqrt(0.0002497502)), type = 'l')


exp(hpd(qnorm, mean = 3.034466, sd = sqrt(0.0002497502)))


plot(x, dnorm(x, mean = exp(3.034466), sd = sqrt(exp(0.0002497502))), type = 'l')

p <- 0.5
(p^4 * ((1-p)^6)) / beta(5, 7)

x <- seq(0.001, 0.999, 0.01)

plot(x, dbeta(x, 5, 7))

N <- 5000
prob <- 0
for(i in 1:N){
  p <- rbeta(1, 5, 7)
  prob <- prob + rbern(1, p)
}

prob / 5000

library(Rlab)

rbern()



     