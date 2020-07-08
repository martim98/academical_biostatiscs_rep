library(ggplot2)
library(data.table)
library(car)


data <- fread('fake_diabetes_data.csv')
head(data)
str(data)
summary(data)
data$diabetes <- factor(data$diabetes)

mod <- lm(chol ~age + diabetes, data = data)
null <- lm(chol ~age, data = data)
summary(null)
0.42^2cor(data$chol, data$age) ^2
summary(mod)
anova(null, mod)

1.735 ^2
  -0.311 ^2

cov(data[,.(chol, age)])
var(data[,.(chol, age)])
vcov(mod)
beta_1 <- -11.80191 / (var(data$age))



beta_estimate(data$age, data$chol)

data$id <- seq(1, 300)
plot(density(data$minutes_exercice))

data[minutes_exercice > mean(minutes_exercice),
     exercice:= "high"][minutes_exercice <= mean(minutes_exercice),
                        exercice:= "low"]
data$minutes_exercice <- NULL

data$gym <- factor(data$gym)
data$diabetes <- factor(data$diabetes)
data$sugar <- factor(data$sugar, levels = c('low', 'moderate', 'high'))
data$exercice <- factor(data$exercice, levels = c('low', 'high'))


str(data)


step(lm(sbp ~., data = data[, !"id"]), direction = 'backward')  
step(lm(sbp ~1, data = data[, !"id"]), direction = 'forward',
     scope = formula(lm(sbp ~., data = data[,!"id"]))) 


best_mod <- lm(formula = sbp ~ diabetes + gym + sugar, data = data)
summary(best_mod)

456.47 / 3




mod_compare <- lm(sbp ~ age + minutes_exercice + sleep_h, data = data)
null_mod <- lm(sbp ~ 1, data = data)
summary(mod_compare)



n <- 300
p <- 2

x <- data$age
x_2 <- data$minutes_exercice
y <- data$sbp

X <- model.matrix(y ~ x + x_2)
X

betas <- solve(t(X) %*% X) %*% t(X) %*% y
fitted <- X %*% solve(t(X) %*% X) %*% t(X) %*% y
fitted



SSE <- sum((y - fitted)^2)
MSE <- SSE/(n - (p + 1))

cov_mat <- MSE* solve(t(X) %*% X)

sqrt(abs(cov_mat))

hat_matrix <- X %*% solve(t(X) %*% X) %*% t(X)
hat_values <- diag(hat_matrix)
hat_values
hatvalues(mod_compare)


SSR <- sum((fitted - mean(y)) ^ 2)
SSR + SSE

# stats #
#F = MSR/MSE
MSR <- SSR/2
MSR/MSE

anova(null_mod, mod_compare)


res  <- fitted - y
std_res <- scale(res)
par(mfrow = c(1, 2))
plot(fitted, res)
plot(fitted, std_res)

cook_d <- cooks.distance(mod_compare)
cook_d



par(mfrow = c(1, 2))
plot(hat_values, std_res)
plot(hat_values, cook_d)
lines(cook_d)

data$fitted <- fitted
data$hat <- hat_values
data$cook <- cook_d
data$std_res <- std_res


ggplot(data) + 
  geom_point(aes(fitted, std_res)) +
  geom_smooth(aes(fitted, std_res), formula = y ~ x, method = "loess", se = F)

ggplot(data) + 
  geom_point(aes(hat, std_res))
  
ggplot(data, (aes( x = (hat/(1- hat)), y = std_res))) + 
  geom_point() 


ggplot(data, (aes( x = hat, y = std_res))) + 
  geom_point() +
  geom_text(aes(label=id),hjust=0, vjust=0)


ggplot(data, (aes( x = hat, y = cook))) + 
  geom_point() +
  geom_text(aes(label=id),hjust=0, vjust=0)

vif(mod_compare)

### interação ###
mod_inter <- lm(sbp ~ age + exercice + age:exercice, data)
summary(mod_inter)
effect <- 0.19184 
ggplot(data, aes(age, sbp, colour = exercice)) + 
  geom_point() + 
  geom_smooth(data = data[exercice == 'high'], aes(age, sbp), formula = y ~ x, 
              method = 'lm', se = F) + 
  geom_smooth(data = data[exercice == 'low'], aes(age, sbp), formula = y ~ x, 
              method = 'lm', se = F) +
  geom_abline(slope = 0.01768, intercept = 119.22527, linetype = 'dashed') +
  geom_abline(slope = 0.01768 + effect, intercept = 119.22527 -5.93010 , linetype = 'dashed')












