setwd("Z:/daten")
x <- read.spss("COUNTRY.SAV", to.data.frame=TRUE)
#2a
View(x)
table(x$DEVELOP)
#2b
hist(x$RADIO)
boxplot(x$RADIO)
table(x$RADIO)
radiodens <- density(x$RADIO)
plot(radiodens)
#2c
table(x$RADIO == "NA")
stem(x$RADIO)
install.packages("robustbase")
out <- adjOutlyingness(x$RADIO)
hist(out$adjout)
View(out)
#2d
devu <- x$URBAN[x$DEVELOP == "Developing country"]
nondevu <- x$URBAN[x$DEVELOP =! "Developing country"]
t.test(devu, nondevu)
wilcox.test(devu, nondevu)
#2e
mean(x$LIFEEXPF)
mean(x$LIFEEXPM)
t.test(x$LIFEEXPF, x$LIFEEXPM, paired=T)
#2f 
tapply(x$DOCS, x$REGION, mean)
boxplot(x$DOCS ~ x$REGION)

levene.test(x$DOCS, x$REGION)



1.5 
par(mfrow=c(1,2))
barplot(x$RADIO)
hist(x$RADIO)
barchart(x$RADIO)
