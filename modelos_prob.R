count <- 0 
count_2 <-0 
stored <- c()
n <-  10000
for(i in 1:n) {
  x_1 <- sample(1:4, 1, replace = T)
  x_2 <- sample(1:x_1, 1, replace = T)
  
  if(x_2 == 1 & x_1 == 2){
    count <- count + 1
  }
  if(x_2 == 1){
    count_2 <- count_2 + 1
  }
}

num <- count /n

den <- count_2 / n

num / den


stored

a <- sum(stored == 2) / n; a

b <- 1 * (1/4) + (1/2) * (1/4) + (1/3) * (1/4) + (1/4) * (1/4)

delta <- abs(a - b); delta

(0.25  * 0.5) / b
