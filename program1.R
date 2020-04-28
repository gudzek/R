# Program rozwiazujacy uklady rownan liniowych metoda eliminacji Gaussa 
# Oraz wyznaczajaca rozklad LU macierzy wspolczynnikow
# Copyright (C) 2018 by Damian Guzek, Emilia Koniszewska and Dawid Drzymalski.
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the GNU General Public License for more details,
# available at <https://www.gnu.org/licenses/>.
# 
# Started on December 11, 2018. Last revision: December 13, 2018.
# 
# 
# Program5 v. 1.00, 12/13/2018. Copyright (C) by Damian Guzek, Emilia Koniszewska and Dawid Drzymalski.
# This is free software. No warranty. See GNU GPL for details.





#####==================================================== part_0
###przykladowe uklady rownan
#dane do macierzy 3x3 sa wprowadzane zaczynajac od lewej strony,
#wprowadzajac kolejno elementy pierwszego wiersza. Po uzupelnieniu
#pierwszego wiersza, przechodzimy do kolejnego wiersza postepujac w 
#analogiczny sposob.

X1 <- c(2,-5,4,1,-2.5,1,1,-4,6)
Y1 <- c(-3,5,10)

X2 <- c(1,-3,2,1,1,-2,2,-1,1)
Y2 <- c(3,1,-1)

X3 <- c(0,2,1,1,-2,-3,-1,1,2)      #program napotyka zero na przekątnej
Y3 <- c(-8,0,3)

X4 <- c(1,-2,-6,2,4,12,1,-4,-12)
Y4 <- c(12,-17,22)






#####==================================================== part_1
###Definiujemy funckje wykorzystywane w naszym programie.
#Funkcja stwierdzajaca czy na przekatenj wystepuje 0
Przekatna <- function(X){
X <- matrix(X,byrow=T,nrow=3,ncol=3)
if(X[1,1]==0 || X[2,2]==0 || X[3,3]==0){
  print('Wystepuje 0 na przekatnej')
}else{
  print('Nie ma 0 na przekatnej')
}
}

#Funkcja stwierdzajaca czy Twierdzenie I zapewnia rozklad LU
#Twierdzenie I
#Jeżeli macierz X posiada wszystkie minory glowne rózne od zera,
#to istnieje rozklad LU macierzy X
Twierdzenie <- function(X){
  X <- matrix(X,byrow=T,nrow=3,ncol=3)
  if(det(X[1:2,1:2])==0 || X[1,1]==0 || det(X)==0){
    print('Twierdzenie I nie zapewnia istnienia rozkladu')
  }else{
    print('Twierdzenie I zapewnia istnienie rozkladu LU ')
  }
}



#Funkcja rozwiazujaca za uklad rownan metoda eliminacji Gaussa
#Oraz znajdujaca rozklad LU macierzy X
GaussLU <- function(X,Y){
  
  Twierdzenie(X)
  Przekatna(X)
  A <- matrix(X,byrow=T,nrow=3,ncol=3)
  b <- matrix(Y,nrow=3,ncol=1)
  p <- nrow(A)
  (U.pls <- cbind(A,b))
  
  U.pls[1,] <- U.pls[1,]/U.pls[1,1]
  
  i <- 2
  while (i < p+1) {
    j <- i
    while (j < p+1) {
      U.pls[j, ] <- U.pls[j, ] - U.pls[i-1, ] * U.pls[j, i-1]
      j <- j+1
    }
    while (U.pls[i,i] == 0) {
      U.pls <- rbind(U.pls[-i,],U.pls[i,])
    }
    U.pls[i,] <- U.pls[i,]/U.pls[i,i]
    i <- i+1
  }
  for (i in p:2){
    for (j in i:2-1) {
      U.pls[j, ] <- U.pls[j, ] - U.pls[i, ] * U.pls[j, i]
    }
  }
  print(U.pls)
  
  
  #Szukanie rozkladu LU
  dim(X) <- c(3, 3)
  library(Matrix)
  expand(lu(X))
}







#####==================================================== part_2
###Testowanie programu.
GaussLU(X1,Y1)
GaussLU(X2,Y2)
GaussLU(X3,Y3)
GaussLU(X4,Y4)



