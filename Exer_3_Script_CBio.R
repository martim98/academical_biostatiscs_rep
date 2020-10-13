
# ------------------------------
#  Ex 3 - Max Verosimilhança
# ------------------------------

#  b)
# -----


# Uma família - max de log-Verosim
# .................................

# Uma família de 20 membros tem 5 pessoas com a doença
x<-5
n<-20
# Cominações 20, 5 a 5 = 15504

# Logaritmo da função de verosimilhança
logL<-function(teta){
log(15504)+x*log(teta)+(n-x)*log(1-teta)-log((1-(1-teta)^n))
}

seqteta<-seq(0.001,0.99,0.01)
seqteta

plot(seqteta,logL(seqteta),"l")

# o máximo está entre 0.1 e 0.3

# The function optimize searches the interval from lower to upper 
# for a minimum or maximum of the function f with respect to its 
# first argument. 

res<-optimize(logL,interval=c(0.1,0.3),maximum=TRUE)
res

# a estimativa de máxima verosimilhança 
# encontra-se em res$maximum


# Uma família - zero de deriv-log-Verosim
# ........................................

# The function uniroot searches the interval from lower to upper 
# for a root (i.e., zero) of the function f with respect to its # first argument. 

# Derivada do logaritmo da função de verosimilhança
dlogL<-function(teta){
x/teta-(n-x)/(1-teta)-n*(1-teta)^(n-1)/(1-(1-teta)^n)
}
res2<-uniroot(dlogL,interval=c(0.1,0.3))
res2


# mais famílias
# ...............

# 7 famílias com 20 membros cada. x1 contém o n.º de membros, em 20, com a doença.
x1<-c(5,4,8,7,2,9,1)

logL1<-function(teta) {
sum(x1)*log(teta)+(n*length(x1)-sum(x1))*log(1-teta)-length(x1)*log((1-(1-teta)^n))
}

plot(seqteta,logL1(seqteta),"l")

res1<-optimize(logL1,interval=c(0.01,0.6),maximum=TRUE)
res1


#  c)
# -----

# Qual é o número de elementos da família
# que se espera terem a característica genética

nesp<-n*res$maximum/(1-(1-res$maximum)^n)
nesp

nesp1<-n*res1$maximum/(1-(1-res1$maximum)^n)
nesp1



