

setwd("D:\\School\\2.Letnik\\Umetna_inteligenca\\Prva_seminarska_naloga\\")
setwd("C:\\Users\\sebas\\Google Drive\\2.Letnik\\Umetna_inteligenca\\Prva_seminarska_naloga\\")

md <- read.table(file="podatkiSem1.txt", sep=",", header=TRUE) 


#Vizualizacija podatkov
#Pripravite nekaj zanimivih grafov, ki ilustrirajo podane podatke 
#(distribucije vrednosti, soodvisnosti med atributi, ponavljajoče se vzorce in podobno).

#O3 po letih
#par(mar=c(1,1,1,1))
plot(md$Datum , md$O3 , xlab="Leto", ylab="O3", main="Maksimalna dnevna koncentracija ozona")

#povprečna TemperaturaLokacije po letih
plot(md$Datum , md$Temperatura_lokacija_mean , xlab="Leto", ylab="Temperatura", main="Povpre�na temperatura lokacije")

#rm(list=ls())


#install.packages("lubridate")


#require(lubridate)


#sestevek O3 po letih
md$Datum <- as.Date(md$Datum)
leta <- format(md$Datum, "%Y")
o3_po_letih <- aggregate(md$O3, by=list(leta), sum)
barplot(o3_po_letih$x, main="Seštevek maksimalne dnevne koncentracije ozona po letih", xlab="Leto", ylab="O3", names.arg = o3_po_letih$Group.1)


#sestevek O3 po mesecih
meseci <- format(md$Datum, "%m")
o3_po_mesecih <- aggregate(md$O3, by=list(meseci), sum)
barplot(o3_po_mesecih$x, main="Seštevek maksimalne dnevne koncentracije ozona po mesecih", xlab="Mesec", ylab="O3", names.arg = o3_po_mesecih$Group.1)



#vsota dezja po mesecih
dez_po_mesecih <- aggregate(md$Padavine_sum, by=list(meseci), sum)
barplot(dez_po_mesecih$x, main="Seštevek dežja po mesecih", xlab="Mesec", ylab="Dež", names.arg = dez_po_mesecih$Group.1,)


#povprecje temperatur postaj po letih 
temperatura_postaja_leta <- aggregate(md$Temperatura_lokacija_mean, by=list(leta), mean)
plot(temperatura_postaja_leta$Group.1,temperatura_postaja_leta$x , xlab="Leto", ylab="Temperatura", main="Povprečje temperatur postaj zraka po letih", type="l", xaxt="n")
axis(side=1,at=temperatura_postaja_leta$Group.1)




#povprecna temperatura po postaj po mesecih
temperatura_postaja_mesec <- aggregate(md$Temperatura_lokacija_mean, by=list(meseci), mean)

barplot(temperatura_postaja_mesec$x, main="Povprečna temperatura zraka postaj po mesecih", xlab="Mesec", ylab="Temperatura", names.arg = temperatura_postaja_mesec$Group.1)





#Primerjava globalnega sevanja in O3
sevanje_mesec <- aggregate(md$Glob_sevanje_mean, by=list(meseci), mean)
O3_mesec <- aggregate(md$O3, by=list(meseci), mean)

plot(sevanje_mesec$x, type="l", col="blue" , xaxt="n", xlab="Mesec", ylab="Količina", main="Primerjava globalnega sevanja in O3", ylim=c(0, max(O3_mesec$x)+5), yaxt='n')
axis(side=1,at=sevanje_mesec$Group.1)
lines(O3_mesec$x,type="l",col="red")
legend(1, 125, legend=c("O3", "globalno sevanje"),col=c("red", "blue"), lty=1:1, cex=0.8)



#Primerjava PM10 in O3
PM10_mesec <- aggregate(md$PM10, by=list(meseci), mean)
O3_mesec <- aggregate(md$O3, by=list(meseci), mean)


plot(PM10_mesec$x, type="l", col="blue" , xaxt="n", xlab="Mesec", ylab="Količina", main="Primerjava O3 in PM10 po mesecih", ylim=c(0, max(O3_mesec$x)+5), yaxt='n')
axis(side=1,at=sevanje_mesec$Group.1)
lines(O3_mesec$x,type="l",col="red")
legend(1, 125, legend=c("O3", "PM10"),col=c("red", "blue"), lty=1:1, cex=0.8)


#Primerjava temperature  lokacije in O3
temperatura_postaja_mesec <- aggregate(md$Temperatura_lokacija_mean, by=list(meseci), mean)
O3_mesec <- aggregate(md$O3, by=list(meseci), mean)


plot(temperatura_postaja_mesec$x, type="l", col="blue" , xaxt="n", xlab="Mesec", ylab="�tevilo", main="Primerjava povpre�ne temperature zraka postaj in O3", ylim=c(0, max(O3_mesec$x)+5), yaxt='n')
axis(side=1,at=sevanje_mesec$Group.1)
lines(O3_mesec$x,type="l",col="red")
legend(1, 125, legend=c("Temperatura", "O3"),col=c("red", "blue"), lty=1:1, cex=0.8)





rm(list=ls()) 




#2. Ocenjevanje atributov
#Ocenite kvaliteto podanih atributov in konstruirajte nove atribute, ki lahko izboljšajo
#kvaliteto zgrajenih modelov. Namig: datum je v obstoječi obliki relativno neuporaben, iz
#njega pa lahko izpeljemo nove atribute (npr. letni čas, dan v tednu…), ki potencialno
#pomagajo pri napovedovanju onesnaženja zraka.

setwd("D:\\School\\2.Letnik\\Umetna_inteligenca\\Prva_seminarska_naloga\\")
setwd("C:\\Users\\sebas\\Google Drive\\2.Letnik\\Umetna_inteligenca\\Prva_seminarska_naloga\\")



podatki <- read.table(file="podatkiSem1.txt", sep=",", header=TRUE) 
podatki$Datum <- as.Date(podatki$Datum)
#install.packages("lubridate")

#meseci

library(lubridate)
mesec <- month(as.POSIXlt(podatki$Datum, format="%Y-%m-%d"))
podatki <- cbind(podatki, mesec=mesec)


#letni čas
getSeason <- function(DATES) {
  zima <- as.Date("2012-12-21", format = "%Y-%m-%d") # Winter Solstice
  pomlad <- as.Date("2012-3-21",  format = "%Y-%m-%d") # Spring Equinox
  poletje <- as.Date("2012-6-21",  format = "%Y-%m-%d") # Summer Solstice
  jesen <- as.Date("2012-9-23",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(DATES, format="2012-%m-%d"))
  
  ifelse (d >= zima | d < pomlad, "zima",
          ifelse (d >= pomlad & d < poletje, "spomlad",
                  ifelse (d >= poletje & d < jesen, "poletje", "jesen")))
}


letni_casi <- getSeason(podatki$Datum)
podatki <- cbind(podatki, letni_cas=letni_casi)


#dan v tednu
library(lubridate)
dan <- weekdays(as.Date(podatki$Datum))
podatki$dan <- dan


#odstranim datum
podatki$Datum <- NULL

podatki$Postaja <- as.factor(podatki$Postaja)
podatki$letni_cas <- as.factor(podatki$letni_cas)
podatki$dan <- as.factor(podatki$dan)

sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)
learn <- podatki[sel,] #ucna mnozica

#write.table(learn, "ucnaMnozica.txt", sep=",")


#3. Klasifikacija
#Zgradite vsaj tri klasifikacijske modele za napovedovanje:



#  a. maksimalne dnevne koncentracije ozona – možni razredi so: NIZKA (pod 60.0),
#NIZKA (pod 60.0),SREDNJA (med 60.0 in 120.0), VISOKA (med 120.0 in 180.0) in EKSTREMNA (nad 180.0),


podatki$razredO3 <- NA



podatki$razredO3[podatki$O3 < 60.0] = "NIZKA"
podatki$razredO3[podatki$O3 >= 60.0 & podatki$O3 < 120.0] = "SREDNJA"
podatki$razredO3[podatki$O3 >= 120.0 & podatki$O3 < 180.0] = "VISOKA"
podatki$razredO3[podatki$O3 >= 180.0] = "EKSTREMNA"

podatki$razredO3 <- as.factor(podatki$razredO3)


# Lahko napisemo funkcijo za izracun klasifikacijske tocnosti
CA <- function(prave, napovedane)
{
  t <- table(prave, napovedane)
  
  sum(diag(t)) / sum(t)
}

# Funkcija za izracun Brierjeve mere
# az vsako dimenizjo pogleda kolko je failu
#napake kvadriramo in sestejemo
#delimo s stevilo vrstci oz. stevilom testnih primerov
#manjsa verejtnost je boljsa, brier score nam pove kolko je failu
#najslabsa verjetnost je 2, ker lfali za 1 in za 0
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}


#library(dplyr)
#pri vseh odstranim O3
podatki$O3 <- NULL



# Podatke bomo nakljucno razdelili na ucno in testno mnozico v rezmerju 70:30.
#Ukaz set.seed nastavi generator nakljucnih stevil.
# Uporabimo ga takrat, ko zelimo ponovljivo sekvenco generiranih stevil.
set.seed(12345) #da vsi nasi random generatorji delajo na enak nacin -9iste nakljucne vrednosti
#brez dneva v tednu, PM10, 

sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)


learn <- podatki[sel,]#ucna mnozica
test <- podatki[-sel,]#testna





#
#
# VECINSKI KLASIFIKATOR
#
# vedno klasificira v razred z najvec ucnimi primeri

# poglejmo pogostost posameznih razredov
table(learn$razredO3)
# v nasem primeru je vecinski razred "SREDINA"

# Izracunajmo tocnost vecinskega klasifikatorja
# (delez pravilnih napovedi, ce bi vse testne primere klasificirali v vecinski razred)

sum(test$razredO3 == "SREDNJA") / length(test$razredO3)

#1: 0.5806452


#
#
# ODLOCITVENO DREVO
#
#

# gradnja modela s pomocjo knjiznice "rpart"

#remove.packages("dplyr")
#install.packages("dplyr")
library(dplyr)
podatki2 <- select(podatki,
                   Hitrost_vetra_max, Hitrost_vetra_mean,
                   Pritisk_mean, 
                   Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_min, 
                   mesec, razredO3)
set.seed(12345)

sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna



library(rpart)
dt <- rpart(razredO3 ~ ., data = learn)
#plot(dt);text(dt)
# Prave vrednosti testnih primerov
observed <- test$razredO3
predicted <- predict(dt, test, type = "class")  #uporabimo ucen model

CA(observed, predicted)
#1: 0.7419355
#2: 0.75


# Napovedane verjetnosti pripadnosti razredom (odgovor dobimo v obliki matrike)
predMat <- predict(dt, test, type = "prob")

# Prave verjetnosti pripadnosti razredom 
# (dejanski razred ima verjetnost 1.0 ostali pa 0.0)
obsMat <- model.matrix( ~ razredO3-1, test)



# Izracunajmo Brierjevo mero za napovedi nasega drevesa
brier.score(obsMat, predMat)
#1: 0.3932342
#2: 





#install.packages(c("ipred", "prodlim", "CORElearn", "e1071", "randomForest", "kernlab", "nnet"))
#install.packages("kernlab")

#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

podatki2 <- select(podatki, Postaja, Glob_sevanje_max, Glob_sevanje_mean,
                   Hitrost_vetra_max, Hitrost_vetra_mean, Hitrost_vetra_min, 
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, 
                   Padavine_mean, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min, 
                   Vlaga_max, Vlaga_mean, Vlaga_min,
                    Temperatura_lokacija_mean, Temperatura_lokacija_min, 
                   mesec, letni_cas, dan, PM10, razredO3)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.nb <- CoreModel(razredO3 ~ ., data = learn, model="bayes")
observed <- test$razredO3
predicted <- predict(cm.nb, test, type="class")
CA(observed, predicted)
#1: 0.5672043
#2: 0.5954301

predMat <- predict(cm.nb, test, type = "probability")
brier.score(obsMat, predMat)
#1: 0.7353673
#2: 0.6333963

#
#
# K-NAJBLIZJIH SOSEDOV
#
#


podatki2 <- select(podatki, Postaja, Glob_sevanje_max, Glob_sevanje_mean,
                    Hitrost_vetra_mean, Hitrost_vetra_min, 
                    Sunki_vetra_mean, 
                   Padavine_mean, Padavine_sum,
                   Pritisk_mean, Pritisk_min, 
                   Vlaga_min, 
                   Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_min, 
                   mesec, letni_cas, dan, PM10, razredO3)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna

# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.knn <- CoreModel(razredO3 ~ ., data = learn, model="knn", kInNN = 5)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)
#1: 0.8010753
#2: 0.8185484


#8:
predMat <- predict(cm.knn, test, type = "probability")
brier.score(obsMat, predMat)
#1: 0.3108602
#2: 0.2954839

#
#
# NAKLJUCNI GOZD
#
#


podatki2 <- select(podatki, Postaja, Glob_sevanje_max,
                   Hitrost_vetra_max, Hitrost_vetra_mean, 
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, 
                   Padavine_mean, Padavine_sum, Pritisk_max,
                   Pritisk_mean, 
                   Vlaga_max, Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max, Temperatura_Krvavec_mean, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_mean, Temperatura_lokacija_min, 
                   mesec, letni_cas, dan, PM10, razredO3)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna


# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.rf <- CoreModel(razredO3 ~ ., data = learn, model="rf")
predicted <- predict(cm.rf, test, type="class")
CA(observed, predicted)
#1: 0.8172043
#2: 0.8212366



predMat <- predict(cm.rf, test, type = "probability")
brier.score(obsMat, predMat)
#1: 0.2657512
#2: 0.2694497

#
#
# SVM
#
#


podatki2 <- select(podatki, Glob_sevanje_max,
                   Hitrost_vetra_max, Hitrost_vetra_mean, 
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, 
                   Padavine_mean, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min, 
                   Vlaga_max, Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_min, 
                   mesec, dan, letni_cas, PM10, razredO3)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna



#gradnja modela s pomocjo knjiznice "kernlab"

library(kernlab)

model.svm <- ksvm(razredO3 ~ ., data = learn, kernel = "rbfdot")
predicted <- predict(model.svm, test, type = "response")

CA(observed, predicted)
#1: 0.7983871
#2: 0.8172043



model.svm <- ksvm(razredO3 ~ ., data = learn, kernel = "rbfdot", prob.model = T)
predMat <- predict(model.svm, test, type = "prob")
brier.score(obsMat, predMat)
#1: 0.5321138
#2: 0.4959855








#preizkusite razli?ne na?ine kombiniranja modelov strojnega u?enja
#(npr. glasovanje, ute?eno glasovanje, bagging in podobno) 
#in jih primerjajte z osnovnimi modeli.

scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}


set.seed(12345)
sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)


learn <- podatki[sel,]#ucna mnozica
test <- podatki[-sel,]#testna

#install.packages("CORElearn")
library(CORElearn)






#
# Glasovanje
#
#
modelDT <- CoreModel(razredO3 ~ ., learn, model="tree")
modelNB <- CoreModel(razredO3 ~ ., learn, model="bayes")
modelKNN <- CoreModel(razredO3 ~ ., learn, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$razredO3, predDT)
caDT
#tree dobi 0.7674731

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$razredO3, predNB)
caNB
#bayes dobi 0.5672043

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$razredO3, predKNN)
caKNN
#KNN dobi 0.8010753

#iamo tri modele s razli?nimi klasfakcijskimi to?nosmti
#ho?emo potegn zanje s vseh treh, ?eprav je bais kinda useless

# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predDT, predNB, predKNN)

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  factor(res, levels=levels(predictions[,1]))
}

predicted <- voting(pred)
CA(test$razredO3, predicted)
#vse skupaj dobi 0.7755376, kar je slab�e od KNN.




















#b. dnevne koncentracije delcev PM10 – možni razredi so: NIZKA (do 35.0), VISOKA (nad 35.0).

podatki$razredPM10 <- NA



podatki$razredPM10[podatki$PM10 <= 35.0] = "NIZKA"
podatki$razredPM10[podatki$PM10 > 35.0] = "VISOKA"
podatki$razredPM10 <- as.factor(podatki$razredPM10)



# Lahko napisemo funkcijo za izracun klasifikacijske tocnosti
CA <- function(prave, napovedane)
{
  t <- table(prave, napovedane)
  
  sum(diag(t)) / sum(t)
}

# Funkcija za izracun Brierjeve mere
# az vsako dimenizjo pogleda kolko je failu
#napake kvadriramo in sestejemo
#delimo s stevilo vrstci oz. stevilom testnih primerov
#manjsa verejtnost je boljsa, brier score nam pove kolko je failu
#najslabsa verjetnost je 2, ker lfali za 1 in za 0
brier.score <- function(observedMatrix, predictedMatrix)
{
  sum((observedMatrix - predictedMatrix) ^ 2) / nrow(predictedMatrix)
}



#library(dplyr)
#pri vseh odstranim PM10
podatki$PM10 <- NULL



# Podatke bomo nakljucno razdelili na ucno in testno mnozico v rezmerju 70:30.
#Ukaz set.seed nastavi generator nakljucnih stevil.
# Uporabimo ga takrat, ko zelimo ponovljivo sekvenco generiranih stevil.
set.seed(12345) #da vsi nasi random generatorji delajo na enak nacin -9iste nakljucne vrednosti


sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)


learn <- podatki[sel,]#ucna mnozica
test <- podatki[-sel,]#testna



write.table(learn, "podatkiUcnaReg2.txt", sep=",")



#
#
# VECINSKI KLASIFIKATOR
#
# vedno klasificira v razred z najvec ucnimi primeri

# poglejmo pogostost posameznih razredov
table(learn$razredPM10 )
# v nasem primeru je vecinski razred "SREDNJA"

# Izracunajmo tocnost vecinskega klasifikatorja
# (delez pravilnih napovedi, ce bi vse testne primere klasificirali v vecinski razred)

sum(test$razredPM10 == "NIZKA") / length(test$razredPM10)

#1: 0.8655914


#
#
# ODLOCITVENO DREVO
#
#

# gradnja modela s pomocjo knjiznice "rpart"


podatki2 <- select(podatki,
                   Sunki_vetra_max,
                   Pritisk_mean,
                  Temperatura_Krvavec_mean, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_mean, 
                  letni_cas,dan, O3, razredPM10)
set.seed(12345)

sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna

library(rpart)
dt <- rpart(razredPM10 ~ ., data = learn)
plot(dt);text(dt)
# Prave vrednosti testnih primerov
observed <- test$razredPM10
predicted <- predict(dt, test, type = "class")  #uporabimo ucen model

CA(observed, predicted)
#1: 0.8991935
#2: 0.9112903




# Napovedane verjetnosti pripadnosti razredom (odgovor dobimo v obliki matrike)
predMat <- predict(dt, test, type = "prob")

# Prave verjetnosti pripadnosti razredom 
# (dejanski razred ima verjetnost 1.0 ostali pa 0.0)
obsMat <- model.matrix( ~ razredPM10-1, test)



# Izracunajmo Brierjevo mero za napovedi nasega drevesa
brier.score(obsMat, predMat)
#1:  0.1588142
#2: 0.1596201



#install.packages(c("ipred", "prodlim", "CORElearn", "e1071", "randomForest", "kernlab", "nnet"))


#
#
# NAIVNI BAYESOV KLASIFIKATOR
#
#

podatki2 <- select(podatki, Hitrost_vetra_mean, Hitrost_vetra_min,
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, Padavine_mean, Pritisk_max,
                   Pritisk_mean, Pritisk_min,Vlaga_max,
                   Temperatura_lokacija_min, 
                   mesec,letni_cas,dan, O3, razredPM10)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna



# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.nb <- CoreModel(razredPM10 ~ ., data = learn, model="bayes")
observed <- test$razredPM10
predicted <- predict(cm.nb, test, type="class")
CA(observed, predicted)
#1: 0.7903226
#2: 0.8360215



predMat <- predict(cm.nb, test, type = "probability")
brier.score(obsMat, predMat)
#1:  0.3778996
#2:  0.2840897

#
#
# K-NAJBLIZJIH SOSEDOV
#
#


podatki2 <- select(podatki, Glob_sevanje_max, Glob_sevanje_mean, Glob_sevanje_min, Hitrost_vetra_max, Hitrost_vetra_min,
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min,Vlaga_max,
                   Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max,Temperatura_Krvavec_mean, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_mean,Temperatura_lokacija_min, 
                   mesec,letni_cas,dan, O3, razredPM10)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna

# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.knn <- CoreModel(razredPM10 ~ ., data = learn, model="knn", kInNN = 5)
predicted <- predict(cm.knn, test, type="class")
CA(observed, predicted)
#1: 0.9018817
#2: 0.9099462



#8:
predMat <- predict(cm.knn, test, type = "probability")
brier.score(obsMat, predMat)
#1: 0.1483871
#2: 0.1468817

#
#
# NAKLJUCNI GOZD
#
#


podatki2 <- select(podatki, Glob_sevanje_max, Glob_sevanje_mean, Hitrost_vetra_max, Hitrost_vetra_mean, Hitrost_vetra_min,
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, Padavine_mean, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min,Vlaga_max,
                   Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max,Temperatura_Krvavec_mean, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_mean,Temperatura_lokacija_min, 
                   mesec,letni_cas,dan, O3, razredPM10)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna


# gradnja modela s pomocjo knjiznice "CORElearn"

library(CORElearn)
cm.rf <- CoreModel(razredPM10 ~ ., data = learn, model="rf")
predicted <- predict(cm.rf, test, type="class")
CA(observed, predicted)
#1: 0.9112903
#2: 0.9153226



predMat <- predict(cm.rf, test, type = "probability")
brier.score(obsMat, predMat)
#1: 0.133014
#2:0.1313416

#
#
# SVM
#
#


podatki2 <- select(podatki, Glob_sevanje_mean,
                   Sunki_vetra_mean, Sunki_vetra_min, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min,Vlaga_max,
                   Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max,Temperatura_Krvavec_mean, 
                   Temperatura_lokacija_mean,Temperatura_lokacija_min, 
                   mesec,letni_cas,dan, O3, razredPM10)

set.seed(12345)
sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)


learn <- podatki2[sel,]#ucna mnozica
test <- podatki2[-sel,]#testna

#gradnja modela s pomocjo knjiznice "kernlab"

library(kernlab)

model.svm <- ksvm(razredPM10 ~ ., data = learn, kernel = "rbfdot")
predicted <- predict(model.svm, test, type = "response")
CA(observed, predicted)
#1: 0.9045699
#2: 0.9139785



model.svm <- ksvm(razredPM10 ~ ., data = learn, kernel = "rbfdot", prob.model = T)
predMat <- predict(model.svm, test, type = "prob")
brier.score(obsMat, predMat)
#1: 0.145491
#2: 0.1327114






#preizkusite razli?ne na?ine kombiniranja modelov strojnega u?enja
#(npr. glasovanje, ute?eno glasovanje, bagging in podobno) 
#in jih primerjajte z osnovnimi modeli.

scale.data <- function(data)
{
  norm.data <- data
  
  for (i in 1:ncol(data))
  {
    if (!is.factor(data[,i]))
      norm.data[,i] <- scale(data[,i])
  }
  
  norm.data
}


set.seed(12345)
sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)


learn <- podatki[sel,]#ucna mnozica
test <- podatki[-sel,]#testna

#install.packages("CORElearn")
library(CORElearn)






#
# Glasovanje
#
#
modelDT <- CoreModel(razredPM10 ~ ., learn, model="tree")
modelNB <- CoreModel(razredPM10 ~ ., learn, model="bayes")
modelKNN <- CoreModel(razredPM10 ~ ., learn, model="knn", kInNN = 5)

predDT <- predict(modelDT, test, type = "class")
caDT <- CA(test$razredPM10, predDT)
caDT
#tree dobi 0.8884409

predNB <- predict(modelNB, test, type="class")
caNB <- CA(test$razredPM10, predNB)
caNB
#bayes dobi 0.7903226

predKNN <- predict(modelKNN, test, type="class")
caKNN <- CA(test$razredPM10, predKNN)
caKNN
#KNN dobi 0.9018817

#iamo tri modele s razli?nimi klasfakcijskimi to?nosmti
#ho?emo potegn zanje s vseh treh, ?eprav je bais kinda useless

# zdruzimo napovedi posameznih modelov v en podatkovni okvir
pred <- data.frame(predDT, predNB, predKNN)

# testni primer klasificiramo v razred z najvec glasovi
voting <- function(predictions)
{
  res <- vector()
  
  for (i in 1 : nrow(predictions))  	
  {
    vec <- unlist(predictions[i,])
    res[i] <- names(which.max(table(vec)))
  }
  
  factor(res, levels=levels(predictions[,1]))
}

predicted <- voting(pred)
CA(test$razredPM10, predicted)
#vse skupaj dobi 0.8938172, kar je malenkost slabse od KNN, oz. neko pvoprečje













#////////////////////////////////////////////////////////////////////////////////////////////////


#4. Regresija
#Zgradite vsaj tri regresijske modele za napovedovanje:
#a. maksimalne dnevne koncentracije ozona,
#b. dnevne koncentracije delcev PM10.




#install.packages("dplyr")
library(dplyr)








#brez razredO3
#brez temperature
podatki2 <- select(podatki, Postaja, Glob_sevanje_max, Glob_sevanje_mean, Glob_sevanje_min,
                   Hitrost_vetra_max, Hitrost_vetra_mean, Hitrost_vetra_min, Sunki_vetra_max,
                   Sunki_vetra_mean, Sunki_vetra_min, Padavine_mean, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min, Vlaga_max, Vlaga_mean, Vlaga_min, Temperatura_Krvavec_max,
                   Temperatura_Krvavec_mean, Temperatura_Krvavec_min, O3, PM10, mesec,dan,letni_cas)

#  regresijsko drevo  o3        0.5346602   mae= 15.26574
#  nakljucni gozd   O3          0.3958446   mae= 11.30224
# k-najblizjih sosedov    O3    0.5041572   mae= 14.35674






#Zpodatki z naloge 2
#  regresijsko drevo  o3        0.5517902     mae = 14.23796
#  nakljucni gozd   O3          0.4054646     mae = 11.61267
# k-najblizjih sosedov    O3    0.4961864     mae = 14.52968









set.seed(12345)


#sel <- sample(1:nrow(podatki2), size=as.integer(nrow(podatki2) * 0.7), replace=F)
sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)

#md <- podatki2
#md <- podatki3
md <- podatki


train <- md[sel,]
test <- md[-sel,]

lm.model <- lm(O3 ~ ., train)

predicted <- predict(lm.model, test)






mae <- function(observed, predicted)
{
  mean(abs(observed - predicted))
}

rmae <- function(observed, predicted, mean.val) 
{  
  sum(abs(observed - predicted)) / sum(abs(observed - mean.val))
}

mse <- function(observed, predicted)
{
  mean((observed - predicted)^2)
}

rmse <- function(observed, predicted, mean.val) 
{  
  sum((observed - predicted)^2)/sum((observed - mean.val)^2)
}





#
# regresijsko drevo  o3
#


library(rpart)

#///////////////////////////////////////////////////////////////////////////

rt.model <- rpart(O3 ~ ., train)
predicted <- predict(rt.model, test)
rmae(test$O3, predicted, mean(train$O3))
mae(test$O3,predicted)

#
# nakljucni gozd   O3
#

#install.packages("randomForest")
library(randomForest)


rf.model <- randomForest(O3 ~ ., train)
predicted <- predict(rf.model, test)
rmae(test$O3, predicted, mean(train$O3))
mae(test$O3,predicted)



#
# k-najblizjih sosedov    O3
#

#install.packages("kknn")
library(kknn)

knn.model <- kknn(O3 ~ ., train, test, k = 5)
predicted <- fitted(knn.model)
rmae(test$O3, predicted, mean(test$O3))
mae(test$O3,predicted)








#/////////////////////////////////////////////////////////////////////////////////



#brez  razredO3, letni_casi,mesec
#brez sunkov
podatki4 <- select(podatki, Postaja, Glob_sevanje_max, Glob_sevanje_mean, Glob_sevanje_min,
                   Hitrost_vetra_max, Hitrost_vetra_mean, Hitrost_vetra_min, 
                   Sunki_vetra_max, Sunki_vetra_mean, Sunki_vetra_min, 
                   Padavine_mean, Padavine_sum, Pritisk_max,
                   Pritisk_mean, Pritisk_min, 
                   Vlaga_max, Vlaga_mean, Vlaga_min, 
                   Temperatura_Krvavec_max, Temperatura_Krvavec_mean, Temperatura_Krvavec_min, 
                   Temperatura_lokacija_max, Temperatura_lokacija_mean, Temperatura_lokacija_min, 
                   dan,PM10)

#  regresijsko drevo  PM10        0.8205745  mae=7.780349
#  nakljucni gozd   PM10          0.693262   mae=6.492922
# k-najblizjih sosedov    PM10    0.8171118  mae=7.7194




#Zpodatki z naloge 2
#  regresijsko drevo  PM10        0.7304283   mae= 8.030843
#  nakljucni gozd   PM10          0.5651606   mae= 6.000241
# k-najblizjih sosedov    PM10    0.7310188   mae= 6.721682



#sel <- sample(1:nrow(podatki4), size=as.integer(nrow(podatki4) * 0.7), replace=F)
sel <- sample(1:nrow(podatki), size=as.integer(nrow(podatki) * 0.7), replace=F)



#md <- podatki4
#md <- podatki5
md <- podatki


train <- md[sel,]
test <- md[-sel,]

lm.model <- lm(PM10 ~ ., train)

predicted <- predict(lm.model, test)




#///////////////////////////////////////////////////////////////////////////////////////

library(rpart)
#
# regresijsko drevo  PM10
#


rt.model <- rpart(PM10 ~ ., train)
predicted <- predict(rt.model, test)
rmae(test$PM10, predicted, mean(train$PM10))
mae(test$PM10,predicted)






#
# nakljucni gozd  PM10
#

#install.packages("randomForest")
library(randomForest)


rf.model <- randomForest(PM10 ~ ., train)
predicted <- predict(rf.model, test)
rmae(test$PM10, predicted, mean(train$PM10))
mae(test$PM10,predicted)



#
# k-najblizjih sosedov  PM10
#

#install.packages("kknn")
library(kknn)

knn.model <- kknn(PM10 ~ ., train, test, k = 5)
predicted <- fitted(knn.model)
rmae(test$PM10, predicted, mean(test$PM10))
mae(test$PM10,predicted)


#////////////////////////////////////////////////////////////////////////////















