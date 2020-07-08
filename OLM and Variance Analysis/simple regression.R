#### data ####
data <- read.csv('fake_diabetes_data.csv')
x <- data[, 'age']
Y <- data[, 'sbp']

plot(x, Y)

mod_compare <- lm(Y ~ x)
anova(mod_compare)
summary(mod_compare)


## funções analíticas ## 
beta_estimate <- function(x, Y){
  num <- sum(x - mean(x)) * sum(Y)
  den <- sum((x - mean(x)) ^ 2)
  beta_1 <- num/den
  beta_0 <- mean(Y) - beta_1 * mean(x)
  
  return(c(beta_0, beta_1))
}

## função variâncias ##
get_vars <- function(x, Y){
  n <- length(x)
  betas <- beta_estimate(x, Y)
  SSE <- sum((Y - (betas[1] + betas[2]*x))^2)
  MSE <- SSE/(n-length(betas))
  return(c(SSE, MSE))
}

## função erros betas ##
beta_error <- function(x, Y){
  n <- length(x)
  betas <- beta_estimate(x, Y)
  var <- get_vars(x, Y)
  beta0_var <- var[2] * (1/n + (mean(x)^2)/(sum((x - mean(x)) ^2)))
  beta1_var <- var[2]/(sum((x - mean(x)) ^2))
  return(c(beta0_var, beta1_var))
}

## fitted, predicted
fit_pred <- function(x, Y){
  alpha <- 0.05
  
  n <- length(x)
  betas <- beta_estimate(x, Y)
  var <- get_vars(x, Y)
  
  fitted <- betas[1] + betas[2]*x
  predic_low <- c()
  predic_high <- c()
  fitted_v <- c() 
  fit_low <- c()
  fit_high <- c()
  q <-qt(1-alpha/2, n-2)
  s_xx <- sum((x-mean(x))^2)
  for(i in 1:n){
    fitted_v[i] <- betas[1] + betas[2] * x[i]
    
    predic_low[i] <- fitted_v[i] - q * sqrt(var[2] * (1/n + (((x[i] - mean(x))^2)/(s_xx))))
    predic_high[i] <- fitted_v[i] + q * sqrt(var[2] * (1/n + (((x[i] - mean(x))^2)/(s_xx))))
    
    
    fit_low[i] <- fitted_v[i] - q * sqrt(1 + var[2] * (1/n + (((x[i] - mean(x))^2)/(s_xx))))
    fit_high[i] <- fitted_v[i] +  q * sqrt(1 + var[2] * (1/n + (((x[i] - mean(x))^2)/(s_xx))))
    
  }
  return(list(fitted_v, predic_low, predic_high, fit_low, fit_high))
}

obj <- fit_pred(x, Y)


plot(x, obj[[1]], type = 'l')
points(x, obj[[2]])
points(x, obj[[3]])

## intervalos beta ##
get_IC_beta <- function(x, Y){
  alpha <- 0.05
  n <- length(x)
  betas <- beta_estimate(x, Y)
  sd <- sqrt(beta_error(x, Y))
  amp_beta0 <- qt(1-alpha/2, n-2) * sd[1]
  amp_beta1 <- qt(1-alpha/2, n-2) * sd[2]
  cat('Beta 0 (', betas[1]-amp_beta0, ',', betas[1] + amp_beta0, ')', '\n')
  cat('Beta 1 (', betas[2]-amp_beta1, ',', betas[2] + amp_beta1, ')')
}

## wald test ##
wald_test <- function(x, Y){
  n <- length(x)
  betas <- beta_estimate(x, Y)
  sd <- sqrt(beta_error(x, Y))
  
  beta0_stat <- (betas[1] - 0)/sd[1]
  beta1_stat <- (betas[2] - 0)/sd[2]
  
  p0 <- pt(abs(beta0_stat), n-2, lower.tail = F)
  p1 <- pt(abs(beta1_stat), n-2, lower.tail = F) 
  
  return(c(round(beta0_stat, 2), round(beta1_stat, 2)))
}

get_IC_beta(x, Y)
wald_test(x, Y)
confint(mod_compare)


betas <- beta_estimate(x, Y)
var <- get_vars(x, Y)

# y = a + bx
plot(x, Y)
abline(a = betas[1], b = betas[2], col = 'red', lty = 2, lwd = 3)
abline(h = mean(Y), lty  = 2, lwd = 3)
