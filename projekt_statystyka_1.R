######=========================================== part_0
###Tworzenie w¸asnych funkcji na potrzeby projektu 
#funkcja 1
kurtosis.test <- function (x) {
  m4 <- sum((x-mean(x))^4)/length(x)
  s4 <- var(x)^2
  kurt <- (m4/s4) - 3
  sek <- sqrt(24/length(x))
  totest <- kurt/sek
  pvalue <- pt(totest,(length(x)-1))
  pvalue 
}

#funkcja 2
skew.test <- function (x) {
  m3 <- sum((x-mean(x))^3)/length(x)
  s3 <- sqrt(var(x))^3
  skew <- m3/s3
  ses <- sqrt(6/length(x))
  totest <- skew/ses
  pt(totest,(length(x)-1))
  pval <- pt(totest,(length(x)-1))
  pval
}





######=========================================== part_1
###Generowanie i wst«pnie rozeznanie si« w otrzymanych danych 
#Generujemy wektory losowe
Norm_vector <- rnorm(5000, mean = 0, sd = 1)
Unif_vector <- runif(5000, min = 0, max = 10)
Gamma_vector <-rgamma(5000, shape = 5, scale = 2)
Exp_vector <- rexp(5000, rate = 3)

hist(Norm_vector,col="gray")
hist(Unif_vector,col="gray")
hist(Gamma_vector,col="gray")
hist(Exp_vector,col="gray")

#Sprawdzamy za pomocˆ qq-plota czy rozk¸ad jest normalny
qqnorm(Norm_vector)
qqline(Norm_vector, col=2)
qqnorm(Unif_vector)
qqline(Unif_vector, col=2)
qqnorm(Gamma_vector)
qqline(Gamma_vector, col=2)
qqnorm(Exp_vector)
qqline(Exp_vector, col=2)

#korzystamy z w¸asnych test—w
kurtosis.test(Norm_vector)
kurtosis.test(Unif_vector)
kurtosis.test(Gamma_vector)
kurtosis.test(Exp_vector)

skew.test(Norm_vector)
skew.test(Unif_vector)
skew.test(Gamma_vector)
skew.test(Exp_vector)

#test shapiro-wilka
shapiro.test(Norm_vector)
shapiro.test(Unif_vector)
shapiro.test(Gamma_vector)
shapiro.test(Exp_vector)





######=========================================== part_2
###psujemy rozk¸ady przez dodawanie
#przez dodawanie rozk¸adu Uniform
result <- shapiro.test(Norm_vector)
result$p.value

Norm_vector_broken_add <- Norm_vector + Unif_vector
result_broken <- shapiro.test(Norm_vector_broken_add)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i <- 100
while (result_broken$p.value < 0.05) {
  Norm_vector_broken_add <- Norm_vector + (i/100)*Unif_vector
  result_broken <- shapiro.test(Norm_vector_broken_add)
  print(i)
  i = i-1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_add, col="lightblue")
plot(p1, col="gray")
plot(p2, col="lightblue", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_add),col="blue")




#przez dodawanie rozk¸adu Gamma
result <- shapiro.test(Norm_vector)
result$p.value

Norm_vector_broken_add <- Norm_vector + Gamma_vector
result_broken <- shapiro.test(Norm_vector_broken_add)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i <- 100
while (result_broken$p.value < 0.05) {
  Norm_vector_broken_add <- Norm_vector + (i/100)*Gamma_vector
  result_broken <- shapiro.test(Norm_vector_broken_add)
  print(i)
  i = i-1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_add, col="lightblue")
plot(p1, col="gray")
plot(p2, col="lightblue", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_add),col="blue")







#przez dodawanie rozk¸adu Exp
result <- shapiro.test(Norm_vector)
result$p.value

Norm_vector_broken_add <- Norm_vector + 2*Exp_vector
result_broken <- shapiro.test(Norm_vector_broken_add)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i <- 200
while (result_broken$p.value < 0.05) {
  Norm_vector_broken_add <- Norm_vector + (i/100)*Exp_vector
  result_broken <- shapiro.test(Norm_vector_broken_add)
  print(i)
  i = i-1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_add, col="lightblue")
plot(p1, col="gray")
plot(p2, col="lightblue", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_add),col="blue")




######=========================================== part_3
###psujemy rozk¸ady przez odejmowanie
#rozk¸adu Uniform
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_sub <- Norm_vector - Unif_vector
result_broken <- shapiro.test(Norm_vector_broken_sub)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i <- 100
while (result_broken$p.value < 0.05) {
  Norm_vector_broken_sub <- Norm_vector - (i/100)*Unif_vector
  result_broken <- shapiro.test(Norm_vector_broken_sub)
  print(i)
  i = i-1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_sub, col="red")
plot(p1, col="gray")
plot(p2, col="red", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_sub),col="red")





#odejmowanie rozk¸adu gamma
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_sub <- Norm_vector - Gamma_vector
result_broken <- shapiro.test(Norm_vector_broken_sub)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i <- 100
while (result_broken$p.value < 0.05) {
  Norm_vector_broken_sub <- Norm_vector - (i/100)*Gamma_vector
  result_broken <- shapiro.test(Norm_vector_broken_sub)
  print(i)
  i = i-1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_sub, col="red")
plot(p1, col="gray")
plot(p2, col="red", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_sub),col="red")





#odejmowanie rozk¸adu exp
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_sub <- Norm_vector - 2*Exp_vector
result_broken <- shapiro.test(Norm_vector_broken_sub)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i <- 200
while (result_broken$p.value < 0.05) {
  Norm_vector_broken_sub <- Norm_vector - (i/100)*Exp_vector
  result_broken <- shapiro.test(Norm_vector_broken_sub)
  print(i)
  i = i-1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_sub, col="red")
plot(p1, col="gray")
plot(p2, col="red", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_sub),col="red")





######=========================================== part_4
###psucie rozk¸adu normlanego przez podstawienie wartoæci
#przez zerowanie wartoæci
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_zer <- Norm_vector
result_broken <- shapiro.test(Norm_vector_broken_zer)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i<-1
while(result_broken$p.value > 0.05){
  
  Norm_vector_broken_zer[sample.int(5000,1)] <-0
  result_broken <- shapiro.test(Norm_vector_broken_zer)
  print(result_broken$p.value)
  print(i)
  i <- i+1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_zer, col="green")
plot(p1, col="gray")
plot(p2, col="green", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_zer),col="green")
qqnorm(Norm_vector_broken_zer)
qqline(Norm_vector, col="green")



#przez podstawienie maximum
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_max <- Norm_vector
result_broken <- shapiro.test(Norm_vector_broken_max)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i<-1
while(result_broken$p.value > 0.05){
  
  Norm_vector_broken_max[sample.int(5000,1)] <-max(Norm_vector)
  result_broken <- shapiro.test(Norm_vector_broken_max)
  print(result_broken$p.value)
  print(i)
  i <- i+1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_max, col="green")
plot(p1, col="gray")
plot(p2, col="green", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_max),col="green")
qqnorm(Norm_vector_broken_max)
qqline(Norm_vector, col="green")




#psucie przez podstawienie sredniej
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_mean <- Norm_vector
result_broken <- shapiro.test(Norm_vector_broken_mean)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i<-1
while(result_broken$p.value > 0.05){
  
  Norm_vector_broken_mean[sample.int(5000,1)] <-mean(Norm_vector)
  result_broken <- shapiro.test(Norm_vector_broken_mean)
  print(result_broken$p.value)
  print(i)
  i <- i+1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_mean, col="green")
plot(p1, col="gray")
plot(p2, col="green", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_mean),col="green")
qqnorm(Norm_vector_broken_mean)
qqline(Norm_vector, col="green")




#psucie przez 5 kwantyl
result <- shapiro.test(Norm_vector)
result$p.value
Norm_vector_broken_kw5 <- Norm_vector
result_broken <- shapiro.test(Norm_vector_broken_kw5)
print("Czy p-value jest wi«ksze od 0.05?")
result_broken$p.value > 0.05

i<-1
while(result_broken$p.value > 0.05){
  
  Norm_vector_broken_kw5[sample.int(5000,1)] <-quantile(Norm_vector, 0.05)
  result_broken <- shapiro.test(Norm_vector_broken_kw5)
  print(result_broken$p.value)
  print(i)
  i <- i+1
}

p1 <-hist(Norm_vector)
p2 <-hist(Norm_vector_broken_kw5, col="green")
plot(p1, col="gray")
plot(p2, col="green", add=TRUE )
plot(density(Norm_vector))
lines(density(Norm_vector_broken_kw5),col="green")
qqnorm(Norm_vector_broken_kw5)
qqline(Norm_vector, col="green")





kurtosis.test(Norm_vector)
kurtosis.test(Norm_vector_broken_zer)
kurtosis.test(Norm_vector_broken_max)
kurtosis.test(Norm_vector_broken_kw5)

skew.test(Norm_vector)
skew.test(Norm_vector_broken_zer)
skew.test(Norm_vector_broken_max)
skew.test(Norm_vector_broken_kw5)





#https://stats.stackexchange.com/questions/3136/how-to-perform-a-test-using-r-to-see-if-data-follows-normal-distribution
hist(Norm_vector)
shapiro.test(Norm_vector)