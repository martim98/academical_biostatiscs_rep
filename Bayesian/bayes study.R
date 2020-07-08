## theta

x <- seq(0.001, 0.9999, 0.001)


plot(x, dbeta(x, 13.5, 3.5), type = 'l', ylab = 'density',
xlab = 'theta')
lines(x, dbeta(x, 14, 4), col = 'red')
lines(x, dbeta(x, 15, 5), col = 'green')
abline(v = 0.5443469, )
abline(v = 0.9085342)
legend('topleft',
       legend = c('Beta(13.5, 3.5)', 
                            'Beta(14, 4)', 
                            'Beta(15, 5)'),
       lty = 1,
       lwd = 2,
       col =c('black', 'red', 'green'))

qbeta(0.025, 15, 5)
qbeta(0.975, 15, 5)
pbeta(0.6, 15, 5, lower.tail = F)
pbeta(0.6, 1, 1, lower.tail = F)
b <- 0.4/(1-0.4)
21.72727/b

x <- seq(0, 10000)
plot(x, pgeom(x, 0.000001), type = 'l')

pgeom(10, 0.5)



## geometrica

theta <- 0.2

1/0.2

0.2 * 5


## predict
dados <- c(24, 25, 31, 31, 22, 21, 26, 20, 16, 22)
n <- length(dados)

x <- seq(0, 50, 0.1)
a <- 101 
b <- 5 
a_post <- a + sum(dados)
b_post <- b + n
plot(x, dgamma(x, a, b), type = 'l', ylim = c(0, 0.4))
lines(x, dgamma(x, a_post, b_post), col ='red')
lines(x, dnorm(x, esp, var), col = 'green')

esp <- a_post/b_post
var <- a_post/b_post^2
plot(x, dnorm(x, esp, var), type = 'l')
qnorm(0.025, esp, var)
qnorm(0.975, esp, var)

sigma <- 4.51
b <- 0
d_2 <- 10^2
n <- 32
x_bar <- 2.46
c <- 1/d_2
tau <- 1/sigma^2

mu <- (n*tau*x_bar + c*b)/(c + n*tau)
var_p <- 1/(c + n*tau)
mu; var_p







0.63 + 4.51^2

qnorm(0.025, mu, var_p)
qnorm(0.975, mu, var_p)


x <- seq(-10, 30, 0.1)
plot(x, dnorm(x, 2.44, sqrt(0.63)), type = 'l')
lines(x, dnorm(x, 2.44, sqrt(0.63 + 4.51^2)))



N <- 50000 
dados <- c()

for(j in 1:100){
  count <- 0
for(i in 1:N){
  mu_pred <- rnorm(1, mu, sqrt(var_p))
  pred <- rnorm(1, mu_pred, 4.51)
  if(pred > 0){
    count <- count + 1
  }
}

  p_true <- pnorm(0, 2.44, sqrt(0.63 + 4.51^2), lower.tail = F)
  p_iter <- count/N

  dados[j] <- p_true - p_iter
  cat(j/100)
}




plot(density(dados_1), ylim = c(0, 200))
lines(density(dados_2))
lines(density(dados_3))
lines(density(dados))


dados_1 <- dados
dados_2 <- dados
dados_3 <- dados


m <- rmultinom(100, 1, c(0.1, 0.5, 0.4))
m <- as.matrix(m)
t(m)
