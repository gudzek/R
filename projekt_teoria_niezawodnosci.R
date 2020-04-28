#####====================================================================================================================
# wyliczanie wartości średniej
#Niezawodność teoretyczna
r <-function(t){
p1 = 1 - pweibull(t,100,3153)
p2 = 1 - pgamma(t,15,11/15)
p3 = 1 - pgamma(t,9, 4/3)
p4 = 1 - pexp(t,1/6)
p5 = 1 - pexp(t,1/6)
p6 = 1 - pexp(t,1/6)
p7 = 1 - pexp(t, 1/8)

p1*p3 + p1*p7 - p1*p3*p7 + p1*p2*p5 + p1*p2*p6 + p1*p2*p4 - p1*p2*p4*p6*p5 - p1*p2*p5*p6 - p1*p2*p4*p5 + p1*p2*p4*p5*p6 - p1*p2*p3*p5 -p1*p2*p3*p6 - p1*p2*p3*p4 + p1*p2*p3*p4*p5*p6 +p1*p2*p3*p5*p6 +p1*p2*p3*p4*p5 - p1*p2*p3*p4*p5*p6 -p1*p2*p5*p7 - p1*p2*p6*p7 -p1*p2*p4*p7 +p1*p2*p4*p5*p6*p7 + p1*p2*p5*p6*p7+ p1*p2*p4*p5*p7 - p1*p2*p4*p5*p6*p7 +p1*p2*p3*p5*p7+ p1*p2*p3*p6*p7 + p1*p2*p3*p4*p7 -p1*p2*p3*p4*p5*p6*p7 -p1*p2*p3*p5*p6*p7 - p1*p2*p3*p4*p5*p7 +p1*p2*p3*p4*p5*p6*p7
}

integrate(r,0,Inf)
#####====================================================================================================================
#Symulacja z1
n1 = 100
z1 = c()
x1 = rweibull(n1, 100, 3153)
x2 = rgamma(n1, 15, 11/15)
x3 = rgamma(n1, 9, 4/3)
x4 = rexp(n1, 1/6)
x5 = rexp(n1, 1/6)
x6 = rexp(n1, 1/6)
x7 = rexp(n1, 1/8)

for(i in 1:n1){
  z1[i] <- min(x1[i],max(x3[i],x7[i],min(x2[i],max(x5[i],x6[i],x4[i]))))
}
integrate(r,0,Inf)
mean(z1)

#Symulacja z2
n2 = 1000
z2 = c()
x1 = rweibull(n2, 100, 3153)
x2 = rgamma(n2, 15, 11/15)
x3 = rgamma(n2, 9, 4/3)
x4 = rexp(n2, 1/6)
x5 = rexp(n2, 1/6)
x6 = rexp(n2, 1/6)
x7 = rexp(n2, 1/8)

for(i in 1:n2){
  z2[i] <- min(x1[i],max(x3[i],x7[i],min(x2[i],max(x5[i],x6[i],x4[i]))))
}
integrate(r,0,Inf)
mean(z2)

#Symulacja z3
n3 = 10000
z3 = c()
x1 = rweibull(n3, 100, 3153)
x2 = rgamma(n3, 15, 11/15)
x3 = rgamma(n3, 9, 4/3)
x4 = rexp(n3, 1/6)
x5 = rexp(n3, 1/6)
x6 = rexp(n3, 1/6)
x7 = rexp(n3, 1/8)


for(i in 1:n3){
  z3[i] <- min(x1[i],max(x3[i],x7[i],min(x2[i],max(x5[i],x6[i],x4[i]))))
}
integrate(r,0,Inf)
mean(z3)

#####====================================================================================================================
#Rozkład prawdopoobieństwa zmiennych 
x <- seq(0,max(z3)+1,by=0.01)
y <- pgamma(x,9, 4/3)
plot(x,y,col ="white", main="Rozkład prawdopodobieństwa zmiennych",ylab=expression(F[n](t)),xlab="t")


lines(x,pweibull(x,100,3153), col=6, lty=1, lwd=1)
lines(x,pgamma(x,15,11/15), col=2, lty=1,lwd=2)
lines(x,pgamma(x,9, 4/3), col=3, lty=1, lwd=2)
lines(x,pexp(x,1/6),col=4, lty=1,lwd=2)
lines(x,pexp(x,1/8),col=5, lty=1,lwd=2)

legend("right", legend=c("Weibull(100,3153)", "Gamma(15,11/15)","Gamma(9, 4/3)","Exp(1/6)","Exp(1/8)"),
       col=c(6, 2,3,4,5),lty=1, cex=0.8)

#####====================================================================================================================
#histogramy czasu życia
hist(z1, main="Histogram czasu pracy życia", ylab="częstotliwość",xlim=c(0,60),xlab="n=100")
x <- seq(0,max(z1)+1,by=0.01)
plot(ecdf(z1), pch=20,cex=0 )
lines(x,1-r(x), col=2, lty=1, lwd=2)


hist(z2, main="Histogram czasu pracy życia", ylab="częstotliwość",xlim=c(0,60),xlab="n=1000")
plot(ecdf(z2),lwd=2)

hist(z3, main="Histogram czasu pracy życia", ylab="częstotliwość",xlim=c(0,60),xlab="n=10000")
plot(ecdf(z3),lwd=2)

#####====================================================================================================================
#Dystrybuanty empiryczne, a teoretyczne 
#n=100
x <- seq(0,max(z1)+1,by=0.01)
plot(ecdf(z1), pch=20,cex=0 ,main="Dystrybuanty dla n = 100",ylab=expression(F[n](x)),xlim=c(0,80))
lines(x,1-r(x), col=2, lty=1, lwd=2)
legend("right", legend=c("Empiryczna", "Teoretyczna"),
       col=c(1,2),lty=1, cex=0.8)

#n=1000
x <- seq(0,max(z2)+1,by=0.01)
plot(ecdf(z2), pch=20,cex=0 ,main="Dystrybuanty dla n = 1 000",ylab=expression(F[n](x)),xlim=c(0,80))
lines(x,1-r(x), col=2, lty=1, lwd=2)
legend("right", legend=c("Empiryczna", "Teoretyczna"),
       col=c(1,2),lty=1, cex=0.8)

#n=10000
x <- seq(0,max(z3)+1,by=0.01)
plot(ecdf(z3), pch=20,cex=0 ,main="Dystrybuanty dla n = 10 000",ylab=expression(F[n](x)), xlim=c(0,80))
lines(x,1-r(x), col=2, lty=1, lwd=2)
legend("right", legend=c("Empiryczna", "Teoretyczna"),
       col=c(1,2),lty=1, cex=0.8)

#####====================================================================================================================
#Dystrybuanta teoretyczna
a1<-ecdf(z1)(x)
a2<-ecdf(z2)(x)
a3<-ecdf(z3)(x)


#dystrybuanta empiryczna
b<-1-r(x)

#Regresja prosta w R
reg1<-lm(a1~b)
reg2<-lm(a2~b)
reg3<-lm(a3~b)
summary(reg1)
summary(reg2)
summary(reg3)

#wykresy
plot(b,a1,type="l",col="2", xlab="Dystrybuanta empiryczna", ylab="Dystrybuanta teoretyczna")
abline(reg1)
legend("bottomright", legend=c("Regresja liniowa", "Dane"),
       col=c(1,2),lty=1, cex=0.8)
plot(b,a2,type="l",col="2", xlab="Dystrybuanta empiryczna", ylab="Dystrybuanta teoretyczna")
abline(reg1)
legend("bottomright", legend=c("Regresja liniowa", "Dane"),
       col=c(1,2),lty=1, cex=0.8)
plot(b,a3,type="l",col="2", xlab="Dystrybuanta empiryczna", ylab="Dystrybuanta teoretyczna")
abline(reg1)
legend("bottomright", legend=c("Regresja liniowa", "Dane"),
       col=c(1,2),lty=1, cex=0.8)

#####====================================================================================================================
#Miary 
T=23

#czas Vesely
czas0=c()
czas1_v=c()
czas2_v=c()
czas3_v=c()

#czas Birnbaum
czas1_b=c()
czas2_b=c()
czas3_b=c()

n=100 #lub 1000 lub 10000
for (i in 1:n){
  czas0[i]=max(min(x1[i],x7[i]),min(x1[i],x2[i],x5[i]), min(x1[i],x2[i],x6[i]),min(x1[i],x2[i],x4[i])
               ,min(x1[i],x3[i]))
  czas1_v[i]=max(min(T,x7[i]),min(T,x2[i],x5[i]), min(T,x2[i],x6[i]),min(T,x2[i],x4[i])
               ,min(T,x3[i]))
  czas2_v[i]=max(min(x1[i],x7[i]),min(x1[i],T,x5[i]), min(x1[i],T,x6[i]),min(x1[i],T,x4[i])
               ,min(x1[i],x3[i]))
  czas3_v[i]=max(min(x1[i],x7[i]),min(x1[i],x2[i],x5[i]), min(x1[i],x2[i],x6[i]),min(x1[i],x2[i],x4[i])
               ,min(x1[i],T))
  czas1_b[i]=max(min(0,x7[i]),min(0,x2[i],x5[i]), min(0,x2[i],x6[i]),min(0,x2[i],x4[i])
               ,min(0,x3[i]))
  czas2_b[i]=max(min(x1[i],x7[i]),min(x1[i],0,x5[i]), min(x1[i],0,x6[i]),min(x1[i],0,x4[i])
               ,min(x1[i],x3[i]))
  czas3_b[i]=max(min(x1[i],x7[i]),min(x1[i],x2[i],x5[i]), min(x1[i],x2[i],x6[i]),min(x1[i],x2[i],x4[i])
               ,min(x1[i],0))
}

P=mean(czas0>=T)
P1_v=mean(czas1_v>=T)
P2_v=mean(czas2_v>=T)
P3_v=mean(czas3_v>=T)

P1_b=mean(czas1_b>=T)
P2_b=mean(czas2_b>=T)
P3_b=mean(czas3_b>=T)

Vesely=c(P1_v-P,P2_v-P,P3_v-P)
Birnbaum=c(P1_v-P1_b,P2_v-P2_b,P3_v-P3_b)
Środek_komunikacji=c('Pieszo','Autobus nr 167','Autobus nr 168')
I_a_przez_I_b=Vesely/Birnbaum
Miary<-data.frame(Środek_komunikacji, Vesely, Birnbaum,I_a_przez_I_b)
Miary

t=10000
p1 = 1 - pweibull(t,100,3153)
p2 = 1 - pgamma(t,15,11/15)
p3 = 1 - pgamma(t,9, 4/3)
p4 = 1 - pexp(t,1/6)
p5 = 1 - pexp(t,1/6)
p6 = 1 - pexp(t,1/6)
p7 = 1 - pexp(t, 1/8)

f_pr1<-function(p1,p2,p3,p4,p5,p6,p7){
  p1*p3 + p1*p7 - p1*p3*p7 + p1*p2*p5 + p1*p2*p6 + p1*p2*p4 - p1*p2*p4*p6*p5 - p1*p2*p5*p6 - p1*p2*p4*p5 + p1*p2*p4*p5*p6 - p1*p2*p3*p5 -p1*p2*p3*p6 - p1*p2*p3*p4 + p1*p2*p3*p4*p5*p6 +p1*p2*p3*p5*p6 +p1*p2*p3*p4*p5 - p1*p2*p3*p4*p5*p6 -p1*p2*p5*p7 - p1*p2*p6*p7 -p1*p2*p4*p7 +p1*p2*p4*p5*p6*p7 + p1*p2*p5*p6*p7+ p1*p2*p4*p5*p7 - p1*p2*p4*p5*p6*p7 +p1*p2*p3*p5*p7+ p1*p2*p3*p6*p7 + p1*p2*p3*p4*p7 -p1*p2*p3*p4*p5*p6*p7 -p1*p2*p3*p5*p6*p7 - p1*p2*p3*p4*p5*p7 +p1*p2*p3*p4*p5*p6*p7
  
}

#Vesely
Ia1<-f_pr1(1,p2,p3,p4,p5,p6,p7)-f_pr1(p1,p2,p3,p4,p5,p6,p7)
Ia2<-f_pr1(p1,1,p3,p4,p5,p6,p7)-f_pr1(p1,p2,p3,p4,p5,p6,p7)
Ia3<-f_pr1(p1,p2,1,p4,p5,p6,p7)-f_pr1(p1,p2,p3,p4,p5,p6,p7)

#Birnbaum
Ib1<-f_pr1(1,p2,p3,p4,p5,p6,p7)-f_pr1(0,p2,p3,p4,p5,p6,p7)
Ib2<-f_pr1(p1,1,p3,p4,p5,p6,p7)-f_pr1(p1,0,p3,p4,p5,p6,p7)
Ib3<-f_pr1(p1,p2,1,p4,p5,p6,p7)-f_pr1(p1,p2,0,p4,p5,p6,p7)

#testy Kolmogorowa
#vesely
ks.test(Ia1,P1_v-P)
ks.test(Ia2,P2_v-P)
ks.test(Ia3,P3_v-P)

#Birnbaum
ks.test(Ib1,P1_v-P1_b)
ks.test(Ib2,P2_v-P2_b)
ks.test(Ib3,P3_v-P3_b)


#####====================================================================================================================
#Obliczanie wartości oczekiwanej i wariancji z funkcji niezawodności.
r2 <-  function(t){2*t*r(t)}
mt <-  integrate(r,0, Inf)$value
mt2 <- integrate(r2, 0, Inf)$value
vt  = mt2  - (mt)^2 

#obliczanie wartości oczekiwanej i wariancji z wygenerowanych rozkładów
mt_z1 <- mean(z1) #n = 100
vt_z1 <- var(z1)

mt_z2 <- mean(z2) #n = 1 000
vt_z2 <- var(z2)

mt_z3 <- mean(z3) #n = 10 000 
vt_z3 <- var(z3)



#Test kołomogorowa-Smirnowa
#dla wartości oczekiwanej
ks.test(mt, z1)
ks.test(mt, z2)
ks.test(mt, z3)

#####====================================================================================================================
#Proces odnowy
n = 10000
z = c()         #czas życia układu 
z_pom = c()     #czas życia układu bez składnika x7
z_odn = c()

for(i in 1:n){
  z[i]     <-  min(x1[i],max(x3[i],x7[i],min(x2[i],max(x5[i],x6[i],x4[i]))))
  z_pom[i] <- min(x1[i],max(x3[i],min(x2[i],max(x5[i],x6[i],x4[i]))))
}

f <-numeric()   #wektor odnowy
for(i in 1:n){
  
  if(x7[i]>z_pom[i]){counter <- 0 }
  
  while(x7[i]<z_pom[i]){
    
    x7[i] <- x7[i] + runif(1, min=0, max=0.1)
    
    if(x7[i]>z_pom[i]){x7[i] <- z_pom[i] 
    i = i + 1}
    
    x7[i] <- x7[i] + rweibull(1,10,4)
    
    counter <- sum(counter,1)
    
  }
  f <- append(f,counter)
  z_odn[i] <- min(x1[i],max(x3[i],x7[i],min(x2[i],max(x5[i],x6[i],x4[i]))))
}

mean(z)
mean(z_odn)
var(z)
var(z_odn)
mean(f)




