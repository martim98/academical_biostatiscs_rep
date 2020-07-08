library(ggplot2)
library(data.table)
library(car)


data <- fread('fake_diabetes_data.csv')
head(data)
str(data)
summary(data)

data$id <- seq(1, 300)

data[minutes_exercice > mean(minutes_exercice),
     exercice:= "high"][minutes_exercice <= mean(minutes_exercice),
                        exercice:= "low"]
data$minutes_exercice <- NULL

data$gym <- factor(data$gym)
data$diabetes <- factor(data$diabetes)
data$sugar <- factor(data$sugar, levels = c('low', 'moderate', 'high'))
data$exercice <- factor(data$exercice, levels = c('low', 'high'))

model.matrix(chol ~ sugar, data = data)

ggplot(data, aes(sugar, chol)) + 
  geom_boxplot()

vetor_medias <- data[, mean(chol), by = sugar][, V1]
vetor_medias
data <- data[sugar == 'low',cod_sugar:= 1][sugar == 'moderate', cod_sugar:= 2][sugar == 'high', cod_sugar:=3]

summary(data$sugar)

SSE <- 0
for (i in 1:3){
  n <- length(data[cod_sugar == i, chol])
  sub_data <- data[cod_sugar == i]
  for(j in 1:n){
    obs <- (sub_data[j, chol] - mean(sub_data[, chol]))^2
    SSE <- SSE + obs
  }
} 

SSF <- 0
for (i in 1:3){
  n <- length(data[cod_sugar == i, chol])
  sub_data <- data[cod_sugar == i]
  for(j in 1:n){
    obs <- (mean(sub_data[, chol] - mean(data[, chol])))^2
    SSF <- SSF + obs
  }
} 


mod <- aov(chol ~ sugar, data=data)
anova(mod)
n <- 300
k <- 3
### MSE/MSF
MSE <- SSE / (n - k)
MSF <- SSF /(k - 1)

MSF / MSE







