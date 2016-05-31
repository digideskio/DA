library(foreign)
setwd(paste(getwd(),"/daten",sep=""))

#read bank2
bank2 <- read.spss("bank2.sav",to.data.frame = TRUE)
real_bank2 <- bank2[1:100,]
forged_bank2 <- bank2[101,200]

#a are they continuous and metric
unique(bank2)
typeof(bank2)
is.data.frame(bank2)
is.numeric(bank2$WIDTH)
is.numeric(bank2$LEFT)
is.numeric(bank2$RIGHT)
is.numeric(bank2$UPPER)
is.numeric(bank2$LOWER)
is.numeric(bank2$DIAGONAL)

#b
#cREATE TABLES
tab1 <- table(bank2$WIDTH)
tab2 <- table(bank2$LEFT)
tab3 <- table(bank2$RIGHT)
tab4 <- table(bank2$UPPER)
tab5 <- table(bank2$LOWER)
tab6 <- table(bank2$DIAGONAL)


#VISUALIZE DATA FOR COMPLETE DATA SET
barplot(tab1)
barplot(tab2)
barplot(tab3)
barplot(tab4)
barplot(tab5)
barplot(tab6)

stripchart(bank2$WIDTH, method='j')
#was kann man erkennen - immer gleicher Abstand zwischen den Merkmalsausprägungen (diskrete Abstände)
# könnte damit zusammenhängen wie die Variable gemessen wird




#VISUALIZE DATA FOR REAL BANKNOTES

#VISUALIZE DATA FOR FORGED BANKNOTES



#TASK 2 - READ IN ALLBUS DATASET
allbus <- read.spss("ALLBUS2014.sav",to.data.frame = TRUE)
tab_albus <- table(allbus$V417)
barplot(tab_albus) 
hist(allbus$V417, breaks = "FD") #angemessendste Visualisierung zur Ermittlung der verteilung, REGEL= Friedman und conis
#Darstellung is noch etwas unpraktisch

#observation --> left tails --> outliers
boxplot(allbus$V417)

#test if normal distribution
qqnorm(allbus$V417)
qqline(allbus$V417)
boxplot(allbus$V417)
boxplot(allbus$V417,outline = FALSE)

#outlier entfernen
income <- subset(allbus$V417,allbus$V417 <15000)
qqnorm(income)
qqline(income)


#Aufgabe 6
#a - welcher parameter beschreibt  die verteilung am beste --> Modus
# Frage ob Nadel oder Histgram -- etwas beides die gleiche Information
