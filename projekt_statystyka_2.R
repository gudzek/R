#####========================================================================== part_0
### import danych i instalacja potrzebnych pakietów
install.packages("fmsb")
install.packages("devtools")
install.packages("ggpubr")
install.packages("plot3D")
library(fmsb)
library(devtools)
library(ggpubr)
library(plot3D)
mydata <- read.table("/Users/damianguzek/Desktop/Speed Dating Data - Speed Dating Data.csv", header=TRUE,
sep=",")
sample_data<-data.frame(mydata)
#szybki podgląd wczytanych danych
colMeans(sample_data)
#tworzymy dwa podzbiory - zawierający tylko mężczyzn i zawierający tylko kobiety
newdata_f <- subset(mydata, gender==0)
newdata_m <-subset(mydata, gender==1)

#####========================================================================== part_1
##### Średnia ocen otrzymana przez uczestników wydarzenia
attr_both <- mean(sample_data$attr)
sinc_both <- mean(sample_data$sinc)
intel_both<- mean(sample_data$intel)
fun_both <- mean(sample_data$fun)
amb_both <- mean(sample_data$amb)
shar_both <- mean(sample_data$shar)
data_both = as.data.frame(matrix(c(attr_both, sinc_both, intel_both, fun_both, amb_both, shar_both), ncol=6))
colnames(data_both) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
data_both=rbind(rep(10,10) , rep(0,10) , data_both)
radarchart( data_both , axistype=2 ,
#custom polygon
pcol=rgb(0.2,0.2,0.2,0.8) , pfcol=rgb(0.2,0.2,0.2,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title ="Rating of average person "
)
##### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Średnia ocen otrzymana przez mężczyzn
attr_f <- mean(newdata_f$attr)
sinc_f <- mean(newdata_f$sinc)
intel_f<- mean(newdata_f$intel)
fun_f <- mean(newdata_f$fun)
amb_f <- mean(newdata_f$amb)
shar_f <- mean(newdata_f$shar)
data_female = as.data.frame(matrix(c(attr_f, sinc_f, intel_f, fun_f, amb_f, shar_f), ncol=6))
colnames(data_female) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
data_female=rbind(rep(10,10) , rep(0,10) , data_female)
radarchart( data_female , axistype=2 ,
#custom polygon
pcol=rgb(0.2,0.2,0.8,0.8) , pfcol=rgb(0.2,0.2,0.8,0.4) , plwd=4 ,
#custom the grid

cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title ="Rating of average men"
)
##### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### Średnia ocen otrzymana przez kobiety
attr_m <- mean(newdata_m$attr)
sinc_m <- mean(newdata_m$sinc)
intel_m<- mean(newdata_m$intel)
fun_m <- mean(newdata_m$fun)
amb_m <- mean(newdata_m$amb)
shar_m <- mean(newdata_m$shar)
data_male = as.data.frame(matrix(c(attr_m, sinc_m, intel_m, fun_m, amb_m, shar_m), ncol=6))
colnames(data_male) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
data_male=rbind(rep(10,10) , rep(0,10) , data_male)
radarchart( data_male , axistype=2 ,
#custom polygon
pcol=rgb(0.7,0.1,0.7,0.8) , pfcol=rgb(0.7,0.1,0.7,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title ="Rating of average women"
)

#####========================================================================== part_2
##### preferencje, które powininen spełniać partner
pf_attr_both <- mean(sample_data$pf_o_att)
pf_sinc_both <- mean(sample_data$pf_o_sin)
pf_intel_both<- mean(sample_data$pf_o_int)
pf_fun_both <- mean(sample_data$pf_o_fun)
pf_amb_both <- mean(sample_data$pf_o_amb)
pf_shar_both <- mean(sample_data$pf_o_sha)
pf_data_both = as.data.frame(matrix(c(pf_attr_both, pf_sinc_both, pf_intel_both, pf_fun_both, pf_amb_both,
pf_shar_both), ncol=6))
colnames(pf_data_both) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
pf_data_both=rbind(rep(30,10) , rep(0,10) , pf_data_both)
radarchart( pf_data_both , axistype=2 ,
#custom polygon
pcol=rgb(0.2,0.2,0.2,0.8) , pfcol=rgb(0.2,0.2,0.2,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,25,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title="What atributes should have your partner?"
)
##### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### men's expectations from women
pf_attr_f <- mean(newdata_f$pf_o_att)
pf_sinc_f <- mean(newdata_f$pf_o_sin)
pf_intel_f<- mean(newdata_f$pf_o_int)
pf_fun_f <- mean(newdata_f$pf_o_fun)

pf_amb_f <- mean(newdata_f$pf_o_amb)
pf_shar_f <- mean(newdata_f$pf_o_sha)
pf_data_f = as.data.frame(matrix(c(pf_attr_f, pf_sinc_f, pf_intel_f, pf_fun_f, pf_amb_f, pf_shar_f), ncol=6))
colnames(pf_data_f) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
pf_data_f=rbind(rep(30,10) , rep(0,10) , pf_data_f)
radarchart( pf_data_f , axistype=2 ,
#custom polygon
pcol=rgb(0.7,0.1,0.7,0.8) , pfcol=rgb(0.7,0.1,0.7,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,25,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title="men's expectations from women"
)
##### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### women's expectations from men
pf_attr_m <- mean(newdata_m$pf_o_att)
pf_sinc_m <- mean(newdata_m$pf_o_sin)
pf_intel_m<- mean(newdata_m$pf_o_int)
pf_fun_m <- mean(newdata_m$pf_o_fun)
pf_amb_m <- mean(newdata_m$pf_o_amb)
pf_shar_m <- mean(newdata_m$pf_o_sha)
pf_data_m = as.data.frame(matrix(c(pf_attr_m, pf_sinc_m, pf_intel_m, pf_fun_m, pf_amb_m, pf_shar_m), ncol=6))
colnames(pf_data_m) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
pf_data_m=rbind(rep(30,10) , rep(0,10) , pf_data_m)
radarchart( pf_data_m , axistype=2 ,
#custom polygon
pcol=rgb(0.2,0.2,0.8,0.8) , pfcol=rgb(0.2,0.2,0.8,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,25,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title ="women's expectations from men"
)

#####========================================================================== part_4
####race influence on made choices
African_American <-subset(mydata, race==1)
Caucasian <-subset(mydata, race==2)
Latino <-subset(mydata, race==3)
Asian <-subset(mydata, race==4)
Other_race <-subset(mydata, race==6)
mean(African_American$dec_o)
mean(Caucasian$dec_o)
mean(Latino$dec_o)
mean(Asian$dec_o)
mean(Other_race$dec_o)
race_chance = as.data.frame(matrix(c(mean(African_American$dec_o)*100, mean(Caucasian$dec_o)*100,
mean(Latino$dec_o)*100, mean(Asian$dec_o)*100, mean(Other_race$dec_o)*100), ncol=5))
colnames(race_chance) = c("African", "Caucasian", "Latino", "Asian", "Other" )
print(race_chance)
##### Age
hist(mydata$age, col="grey", main=" participant's age", xlab="age", xlim=c(20,40))
hist(newdata_f$age, col="pink", main="women participant's age", xlab="age",xlim=c(20,40))

hist(newdata_m$age, col="lightblue", main="men participant's age", xlab="age",xlim=c(20,40))

#####========================================================================== part_5
##### What is good enough man and woman to date with
winners_f <-subset(newdata_f, dec==1)
winners_m <-subset(newdata_m, dec==1)
##### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### men which won
attr_win_m <- quantile(winners_f$attr,0.25)
sinc_win_m <- quantile(winners_f$sinc,0.25)
intel_win_m<- quantile(winners_f$intel,0.25)
fun_win_m <- quantile(winners_f$fun,0.25)
amb_win_m <- quantile(winners_f$amb,0.25)
shar_win_m <- quantile(winners_f$shar,0.25)
sufficient_man = as.data.frame(matrix(c(attr_win_m, sinc_win_m, intel_win_m, fun_win_m, amb_win_m, shar_win_m),
ncol=6))
colnames(sufficient_man) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
sufficient_man=rbind(rep(10,10) , rep(0,10) , sufficient_man)
radarchart( sufficient_man , axistype=2 ,
#custom polygon
pcol=rgb(0.2,0.2,0.8,0.8) , pfcol=rgb(0.2,0.2,0.8,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title="Great enough man"
)
##### - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
##### women which won
attr_win_f <- quantile(winners_m$attr,0.25)
sinc_win_f <- quantile(winners_m$sinc,0.25)
intel_win_f<- quantile(winners_m$intel,0.25)
fun_win_f <- quantile(winners_m$fun,0.25)
amb_win_f <- quantile(winners_m$amb,0.25)
shar_win_f <- quantile(winners_m$shar,0.25)
sufficient_woman = as.data.frame(matrix(c(attr_win_f, sinc_win_f, intel_win_f, fun_win_f, amb_win_f, shar_win_f),
ncol=6))
colnames(sufficient_woman) = c("Attractive", "Sincere", "Intelligent", "Fun", "Ambitious", "Interest" )
sufficient_woman=rbind(rep(10,10) , rep(0,10) , sufficient_woman)
radarchart( sufficient_woman , axistype=2 ,
#custom polygon
pcol=rgb(0.7,0.1,0.7,0.8) , pfcol=rgb(0.7,0.1,0.7,0.4) , plwd=4 ,
#custom the grid
cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
#custom labels
vlcex=0.8,
title="Great enough woman"
)
#####========================================================================== part_6
##### Badanie korelacji pearsona, kendalla i spearmana.
# - - - - - - - - - - - - - - - - - - - -
#korelacja wieku względem atrakcyjności fizycznej
cor.test(newdata_f$age_o,newdata_f$attr, method="pearson")
cor.test(newdata_f$age_o,newdata_f$attr, method="kendall")
z <- table(newdata_f$age_o, newdata_f$attr)
hist3D(z=z, border="black", xlab="Age", ylab="Attractive", main="Women's perspective about men")

cor.test(newdata_m$age_o,newdata_m$attr, method="pearson")
cor.test(newdata_m$age_o,newdata_m$attr, method="kendall")
z <- table(newdata_m$age_o, newdata_m$attr)
hist3D(z=z, border="black", xlab="Age", ylab="Attractive", main="Men's perspective about women")
# - - - - - - - - - - - - - - - - - - - -
#korelacja fun z atrakcyjnoścą fizyczną
cor.test(newdata_f$fun_o, newdata_f$attr_o, method ="pearson")
cor.test(newdata_f$fun_o, newdata_f$attr_o, method ="kendall")
z <- table(newdata_f$fun_o, newdata_f$attr_o)
image2D(z=z, border="black", xlab="Fun", ylab="Attractive", main="Men's perspective about women")
cor.test(newdata_m$fun_o, newdata_m$attr_o, method ="pearson")
cor.test(newdata_m$fun_o, newdata_m$attr_o, method ="kendall")
z <- table(newdata_m$fun_o, newdata_m$attr_o)
image2D(z=z, border="black", xlab="Fun", ylab="Attractive", main="Women's perspective about men")
# - - - - - - - - - - - - - - - - - - - -
#korelacja atrybutów z szaną na następne spotkanie
aggdata <-aggregate(sample_data, by=list(sample_data$iid), FUN=mean, na.rm=TRUE)
cor.test(aggdata$attr_o, aggdata$dec_o, method="pearson")
cor(aggdata$attr_o, aggdata$dec_o, method="pearson")
cor(aggdata$attr_o, aggdata$dec_o, method="kendall")
plot(aggdata$attr_o, aggdata$dec_o, type="p",pch=16, col="lightgrey", ylab="chances to next date", xlab="attractiveness
score")
lines(lowess(aggdata$attr_o,aggdata$dec_o), col="blue")
cor.test(aggdata$sinc_o, aggdata$dec_o, method="pearson")
cor(aggdata$sinc_o, aggdata$dec_o, method="pearson")
cor(aggdata$sinc_o, aggdata$dec_o, method="kendall")
plot(aggdata$sinc_o, aggdata$dec_o, type="p",pch=16, col="lightgrey", ylab="chances to next date", xlab="sincerity
score")
lines(lowess(aggdata$sinc_o,aggdata$dec_o), col="blue")
cor.test(aggdata$intel_o, aggdata$dec_o, method="pearson")
cor(aggdata$intel_o, aggdata$dec_o, method="pearson")
cor(aggdata$intel_o, aggdata$dec_o, method="kendall")
plot(aggdata$intel_o, aggdata$dec_o, type="p",pch=16, col="lightgrey", ylab="chances to next date", xlab="intelligence
score")
lines(lowess(aggdata$intel_o,aggdata$dec_o), col="blue")
cor.test(aggdata$fun_o, aggdata$dec_o, method="pearson")
cor(aggdata$fun_o, aggdata$dec_o, method="pearson")
cor(aggdata$fun_o, aggdata$dec_o, method="kendall")
plot(aggdata$fun_o, aggdata$dec_o, type="p",pch=16, col="lightgrey", ylab="chances to next date", xlab="fun score")
lines(lowess(aggdata$fun_o,aggdata$dec_o), col="blue")
cor.test(aggdata$amb_o, aggdata$dec_o, method="pearson")
cor(aggdata$amb_o, aggdata$dec_o, method="pearson")
cor(aggdata$amb_o, aggdata$dec_o, method="kendall")
plot(aggdata$amb_o, aggdata$dec_o, type="p",pch=16, col="lightgrey", ylab="chances to next date", xlab="ambitions
score")
lines(lowess(aggdata$amb_o,aggdata$dec_o), col="blue")
cor.test(aggdata$shar_o, aggdata$dec_o, method="pearson")
cor(aggdata$shar_o, aggdata$dec_o, method="pearson")
cor(aggdata$shar_o, aggdata$dec_o, method="kendall")
plot(aggdata$shar_o, aggdata$dec_o, type="p",pch=16, col="lightgrey", ylab="chances to next date", xlab="same interest
score")
lines(lowess(aggdata$shar_o,aggdata$dec_o), col="blue")
#&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&