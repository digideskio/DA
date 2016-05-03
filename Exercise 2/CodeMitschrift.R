#Exercise 2
#(a) Enter the data


child <- c(1:25)
Cereal <- as.factor(c("Ghostly Shadows","Ghostly Shadows", "Ghostly Shadows", "Canary Crunch","Turtle Treats", "Turtle Treats","Turtle Treats","Tutle Treats","Ghostly Shadows","Turtle Treats","Turtle Treats","Ghostly Shadows", "Turtle Treats","Turtle Treats","Ghostly Shadows","Canary Crunch","Turtle Treats","Turtle Treats","Ghostly Shadows","Turtle Treats","Canary Crunch","Turtle Treats","Ghostly Shadows", "Turtle Treats", "Turtle Treats"))
Like <- as.factor(c("crazy", "like","not part","not part","crazy","crazy","crazy","like","crazy","not part","crazy","like","crazy","like","crazy","don't know","crazy","like","like","crazy","like","crazy","like","like","crazy"))
Gift <- as.factor(c("squirt gun","squirt gun","squirt gun","ring","squirt gun","ring","squirt gun","ring","ring","squirt gun","squirt gun","ring","squirt gun","ring","whistle","ring","whistle","ring","squirt gun","can't decide", "ring","squirt gun","ring","ring","ring"))
Gender <- as.factor(c("M","M","M","M","F","F","F","F","M","F","F","F","M","M","F","M","F","F","F","M","F","M","F","F","F"))

Cereals <- data.frame(child,Cereal,Like,Gift,Gender)

#(b)Calculate percentage of sample for men and women
table(Cereals$Gender)
perc_men <- (table(Cereals$Gender)[2]/(table(Cereals$Gender)[1]+table(Cereals$Gender)[2]))
perc_women <- (table(Cereals$Gender)[1]/(table(Cereals$Gender)[1]+table(Cereals$Gender)[2]))

prop.table(table(Cereals$Gender))
#(c)which cereal was preferred
names(table(Cereals$Cereal))[which.max(table(Cereals$Cereal))]

#(d)which gift would you include in cereal box
table(Cereals)
tableplot()


#A3 - boston housing
lma <- mean(Boston$lstat)
lmz <- median(Boston$lstat)
mv <- var(Boston$medv)
cp <- sum(Boston$chas)/nrow(Boston)

#A4
index <- sample(nrow(Boston), size=100,replace=T)
sample <- Boston[index,]
#Konfidenzintervalle ausrechnen
#was wissen wir über Konfidenzintervall? 
#--> approx. Normalverteilung (aufgrund zentralen Grenzwertsatz)


ci.mean(sample$lstat)
ci.mean(sample$medv)
# ci median
install.packages("asbio")
library("asbio")
ci.median(sampl$lstat)