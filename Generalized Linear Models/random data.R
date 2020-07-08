library(data.table)
library(ggplot2)
library(Rlab)

N <- 300

diabetes <- c()
set.seed(300)
diabetes <- rbern(300, 0.12)

## adding sbp
x <- c()
for(i in 1:300){
  if(diabetes[i] == 1){
    x[i] <- rnorm(1, 130, 6)
  } else{
    x[i] <- rnorm(1, 120, 5)
    
  }
}


## adding number of hours sleep

hours_sleep <- c()
for(i in 1:300){
  if(diabetes[i] == 1){
    hours_sleep[i] <- rpois(1, 5)
  } else{
    hours_sleep[i] <- rpois(1, 7)

  }
}

## adding minutes of exercice per day
exercice <- c()
for(i in 1:300){
  if(diabetes[i] == 1){
    exercice[i] <- rpois(1, 20)
  } else{
    exercice[i] <- rpois(1, 60)
    
  }
}

## goes to gym?
gym <- c()

for(i in 1:300){
  if(diabetes[i] == 1){
    b <- rbern(1, 0.3)
    gym[i] <- 0 + b
  } else{
    gym[i] <- 1 - b
    
  }
}

## age
age<- c()
for(i in 1:300){

  if(diabetes[i] == 1){
    age[i] <-sample(50:80) 
    b <- ceiling(rgamma(1, 10, 1))
    while(age[i] + b < 18 | age[i] + b > 80){
      b <- ceiling(rnorm(1, 0, 10))}
  } else {
    age[i] <- sample(18:80)
  }
}
  
  
## sugar intake
sugar <- c()
opt <- c('low', 'moderate', 'high')
for(i in 1:N){
  if(age[i] > 60){
    sugar[i] <- sample(opt, 1, prob = c(0.5, 0.3, 0.2))
  } else if( age[i] > 40) {
    sugar[i] <- sample(opt, 1, prob = c(0.3, 0.4, 0.3))
  } else {
    sugar[i] <- sample(opt, 1, prob = c(0.2, 0.4, 0.4))
    }
}
 
## cholestrol   
chol <- c()

for(i in 1:N){
  if(gym[i] == 1 & sugar[i] == 'low'){
    chol[i] <- rnorm(1, 150, 8)
  } else if(gym[i] == 1 & sugar[i] != 'low') {
    chol[i] <- rnorm(1, 190, 20)

  } else if(gym[i] == 0 & sugar[i] == 'high') {
    chol[i] <- rnorm(1, 250, 40)

  } else if(gym[i] == 0 & sugar[i] == 'moderate'){
    chol[i] <- rnorm(1, 220, 40)
  } else {
    chol[i] <- rnorm(1, 190, 40)
  }
}



data <- data.table(sbp = x, 
                   diabetes = factor(diabetes), 
                   sleep_h = hours_sleep, 
                   minutes_exercice = exercice, 
                   gym = factor(gym),
                   age = age,
                   sugar = factor(sugar), 
                   chol = chol)

ggplot(data) + 
  geom_point(aes(age, chol, colour = sugar))

fwrite(data, 'fake_diabetes_data.csv')
