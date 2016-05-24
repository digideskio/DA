library("foreign")
setwd(paste(getwd(),"/daten",sep=""))
Albus <- read.spss(file="ALLBUS2014.sav",to.data.frame = TRUE)

#2.1
ctab <- table(Albus$V868,Albus$V418)
ctab
chisq.test(ctab)

#data:  ctab
#X-squared = 353.28, df = 336, p-value = 0.2479
# p-value is indicating independence

chisq.test(ctab, simulate.p.value = T)

#	Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
#data:  ctab
#X-squared = 353.28, df = NA, p-value = 0.2704
# --> Improvement - Einkommen ist unabhängig von Nullhypothese

#2.2
ctab2 <- table (Albus$V11, Albus$V10)
ctab2
chisq.test(ctab2)
#Pearson's Chi-squared test
#data:  ctab2
#X-squared = 374.32, df = 16, p-value < 2.2e-16

chisq.test(ctab2,simulate.p.value = TRUE)

#	Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
#data:  ctab2
#X-squared = 374.32, df = NA, p-value = 0.0004998

#2.3
index <- sample(nrow(Albus),size = 0.5*nrow(Albus))
ctab3 <- table(Albus$V10[index],Albus$V11[index])
chisq.test(ctab3)

#	Pearson's Chi-squared test
#data:  ctab3
#X-squared = 179.05, df = 16, p-value < 2.2e-16

chisq.test(ctab3,simulate.p.value = TRUE)

#	Pearson's Chi-squared test with simulated p-value (based on 2000 replicates)
#data:  ctab3
#X-squared = 179.05, df = NA, p-value = 0.0004998

index <- sample(nrow(Albus), size = 0.1*nrow(Albus))
ctab4 <- table(Albus$V10[index], Albus$V11[index])
chisq.test(ctab4)

#	Pearson's Chi-squared test
#data:  ctab4
#X-squared = 67.267, df = 16, p-value = 2.989e-08
chisq.test(ctab4, simulate.p.value = TRUE)

#data:  ctab4
#X-squared = 67.267, df = NA, p-value = 0.01199

#-->quadratische Kontigenz nimmt ab. Weil: kritischer Wert ist zwar immer der gleiche,
#aber Kontingenz (X-squared) geht runter, weil mit abnehmendem n das abnimmt

#2.4. Cohens w -> Zur Messung von Effektstärken
w <- chisq.test(ctab2)$statistic
cohen <- sqrt(w/sum(ctab2))
cohen

w2 <- chisq.test(ctab4)$statistic
cohen2 <- sqrt(w/sum(ctab4))
cohen2
#Frage: wann gibt es ein Problem mit den Effektstärken:
# aufgezeichnete Matrix "H0","H1"/"niedrig","hoch"
# --> Problem bei Effektstärke: niedrig, "H1" $ "hoch", "H0" (weil Stichprobe zu klein ist)


#A2
#a - Frage nach Skalenniveaus
# Metrische Skalenniveaus - stetige Variable
      