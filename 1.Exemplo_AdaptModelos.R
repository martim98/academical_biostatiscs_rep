
# ------------------------------
# Exemplo - Taxas de Mortalidade
# ------------------------------


# Dados
# -----

g<-c(1:8)
n<-c(17742,16554,16059,13083,10784,9645,10706,9933)
y<-c(1,5,5,12,25,38,54,65)
ly<-log(y)
ly
ln<-log(n)
ln
ldrr<-ly-ln+log(100000)  # ln(taxa real)
ldrr  # eixo abcissas


# Modelo Adaptado (m=0)
# ---------------------

glm1<-glm(ly ~ 1,offset=ln)   # por defeito: family = gaussian
glm1$fitted.values
summary(glm1)
pchisq(deviance(glm1),df.residual(glm1),lower.tail=F)

ldr1<-glm1$fitted.values-log(n)+log(100000)
ldr1  # eixo ordenadas

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr1,ldrr),max(ldr1,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr1,col="magenta")
lines(g,ldr1,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Modelo Adaptado (m=1)
# ---------------------

glm2<-glm(ly ~ g,offset=ln)
glm2$fitted.values 
summary(glm2)
pchisq(deviance(glm2),df.residual(glm2),lower.tail=F)

ldr2<-glm2$fitted.values-ln+log(100000)
ldr2

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr2,ldrr),max(ldr2,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr2,col="magenta")
lines(g,ldr2,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Modelo Adaptado (m=2)
# ---------------------

g2<-g^2
glm3<-glm(ly ~ g + g2,offset=ln)
glm3$fitted.values
summary(glm3)
pchisq(deviance(glm3),df.residual(glm3),lower.tail=F)

ldr3<-glm3$fitted.values-ln+log(100000)
ldr3
plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr3,ldrr),max(ldr3,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr3,col="magenta")
lines(g,ldr3,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))



# Modelo Adaptado (m=3)
# ---------------------

g3<-g^3
glm4<-glm(ly ~ g + g2 + g3,offset=ln)
glm4$fitted.values 
summary(glm4)
pchisq(deviance(glm4),df.residual(glm4),lower.tail=F)

ldr4<-glm4$fitted.values-ln+log(100000)
ldr4

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr4,ldrr),max(ldr4,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr4,col="magenta")
lines(g,ldr4,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Modelo Adaptado (m=4)
# ---------------------

g4<-g^4
glm5<-glm(ly ~ g + g2 + g3 + g4,offset=ln)
glm5$fitted.values
summary(glm5)
pchisq(deviance(glm5),df.residual(glm5),lower.tail=F)

ldr5<-glm5$fitted.values-ln+log(100000)
ldr5

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr5,ldrr),max(ldr5,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr5,col="magenta")
lines(g,ldr5,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Modelo Adaptado (m=5)
# ---------------------

g5<-g^5
glm6<-glm(ly ~ g + g2 + g3 + g4 + g5,offset=ln)
glm6$fitted.values 
summary(glm6)
pchisq(deviance(glm6),df.residual(glm6),lower.tail=F)

ldr6<-glm6$fitted.values-ln+log(100000)
ldr6

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr6,ldrr),max(ldr6,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr6,col="magenta")
lines(g,ldr6,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Modelo Adaptado (m=6)
# ---------------------

g6<-g^6
glm7<-glm(ly ~ g + g2 + g3 + g4 + g5 + g6,offset=ln)
glm7$fitted.values
summary(glm7)
pchisq(deviance(glm7),df.residual(glm7),lower.tail=F)

ldr7<-glm7$fitted.values-ln+log(100000)
ldr7

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr7,ldrr),max(ldr7,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr7,col="magenta")
lines(g,ldr7,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Modelo Adaptado (m=7)
# ---------------------

g7<-g^7
glm8<-glm(ly ~ g + g2 + g3 + g4 + g5 + g6 + g7,offset=ln)
glm8$fitted.values 
summary(glm8)
pchisq(deviance(glm8),df.residual(glm8),lower.tail=F)

ldr8<-glm8$fitted.values-ln+log(100000)
ldr8

plot(g,ldrr,col="blue",xlim=c(1,8),ylim=c(min(ldr8,ldrr),max(ldr8,ldrr)),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,col="blue")
points(g,ldr8,col="magenta")
lines(g,ldr8,col="magenta")
legend("bottomright",c("reais","estimadas"),lty = 1,col=c("blue","magenta"))


# Todos os Modelos (significativos)
# ---------------------------------

plot.new()
plot(g,ldrr,cex=0.1,xlim=c(1,8),ylim=c(0,8),xlab="Classe etária",ylab="ln(taxa)")
lines(g,ldrr,lwd=2,col="blue")
lines(g,ldr1,col="magenta",lwd=2)
lines(g,ldr2,col="green",lwd=2)
lines(g,ldr3,col="orange",lwd=2)
legend("bottomright",c("reais","estimadas_1","estimadas_2","estimadas_3"),lty = 1,col=c("blue","magenta","green","orange"))


