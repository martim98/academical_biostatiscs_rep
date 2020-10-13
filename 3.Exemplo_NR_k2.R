
# -----------------------------
# Newton-Raphson (k=2)
#
# Estimar (alpha,beta), 
# modelo de regressao logistica
# (exemplo - slides 15/18)
# -----------------------------


x<-c(21,24,25,26,28,31,33,34,35,37,43,49,51,55,25,29,43,44,46,46,51,55,56,58)
x
y<-c(rep(1,14),rep(0,10))
y


# -------------
# Implementacao 
# -------------

dif<-matrix(c(2,2),2,1)
param0<-matrix(c(0.1,0),2,1) # param0[1]=alfa e param0[2]=beta 

# Podemos verificar que para convergir tem que ser inicializado em valores muito proximos de zero
iter<-0
cut<-0.01
vetor_params <- c()
 
while(dif[1]>cut | dif[2]>cut)   # "|" significa "or"
{
iter<-iter+1
teta<-exp(param0[1]+param0[2]*x)/(1+exp(param0[1]+param0[2]*x))

dln<-c(sum(y-teta),sum(x*(y-teta)))   # vetor derivadas de 1.a ordem

a11<- sum((1-teta)*teta)
a21<- sum((1-teta)*x*teta)
a22<- sum((1-teta)*(x^2)*teta)
MH<-matrix(c(a11,a21,a21,a22),2,2)    # matriz derivadas 2.a ordem

param<-param0+solve(MH)%*%dln
dif<-abs(param0-param)
param0<-param
vetor_params[iter] <- param0
}

param
iter

logit_func <- function(x) {
  logit <- exp(param[1] + param[2] * x) / (1 + exp(param[1] + param[2] * x))
  return (logit)
}

logit_func(20)

# --------------------------
# Recorrendo 'a funcao maxNR
# --------------------------

library(maxLik)

# f = log-verosimilhanca de (alfa,beta)
f <- function(para) {
alfa<-para[1]
beta<-para[2]
sum(y*(alfa+beta*x)-log(1+exp(alfa+beta*x)))}

nr<-maxNR(f,start=c(0.1,0))



