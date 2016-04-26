setwd("Z:/daten")

library("foreign")
ctry <- read.spss("COUNTRY.SAV" , to.data.frame=T)
dim(ctry)

library(psych)
describe(ctry$URBAN)

plot(density(ctry$URBAN))


table(is.na(ctry$URBAN))

boxplot(ctry$URBAN)

Dctry<-subset(ctry,ctry$DEVELOP=="Developed country")
NDctry<-subset(ctry,ctry$DEVELOP=="Developing country")

t.test(Dctry$GDP,NDctry$GDP)

mean(ctry$LIFEEXPM)
mean(ctry$LIFEEXPF)

wilcox.test(ctry$LIFEEXPF,ctry$LIFEEXPM)

kruskal.test(ctry(life$))

