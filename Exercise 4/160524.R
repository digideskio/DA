#1
library("foreign")
x <- read.spss("C:/Users/sk/Seafile/Meine Bibliothek/pc.pool/daten/ALLBUS2014.SAV",
               to.data.frame = TRUE)                  
##2 
#2a
ctab <- table (x$V868,x$V418)
ctab
chisq.test(ctab)
chisq.test(ctab, simulate.p.value=T)
#2b
ctab <- table (x$V10,x$V11)
ctab
chisq.test(ctab)
chisq.test(ctab, simulate.p.value=T)
#2c
#50%
index <- sample(nrow(x), size=0.5*nrow(x))
ctab5 <- table (x$V10[index],x$V11[index])
ctab5
chisq.test(ctab5)
chisq.test(ctab5, simulate.p.value=T)
#10%
index <- sample(nrow(x), size=0.1*nrow(x))
ctab1 <- table (x$V10[index],x$V11[index])
ctab1
chisq.test(ctab1)
chisq.test(ctab1, simulate.p.value=T)
#2d
w  <- chisq.test(ctab)$statistic
sqrt(w/sum(ctab))
w5 <- chisq.test(ctab5)$statistic
sqrt(w5/sum(ctab5))
w1 <- chisq.test(ctab1)$statistic
sqrt(w1/sum(ctab1))
#3
bank <- read.spss("C:/Users/sk/Seafile/Meine Bibliothek/pc.pool/daten/BANK2.SAV",
               to.data.frame = TRUE)                  
##4
#4a - theory
#4b
apply(bank, 2, class)
apply(bank, 2, function(v) { length(unique(v)) })
#4c
apply(bank, 2, mean)
apply(bank, 2, median)
apply(bank, 2, sd)
apply(bank, 2, IQR)
#4d
group <- ((1:200)<=100)
group
apply(bank, 2, function(v) tapply(v, group, mean))
apply(bank, 2, function(v) tapply(v, group, median))
apply(bank, 2, function(v) tapply(v, group, sd))
apply(bank, 2, function(v) tapply(v, group, IQR))
#4e
cohens.d.equal <- function (x, group) {
  n     <- tapply(rep(1, length(x)), group, length)
  gmean <- tapply(x, group, mean)
  gvar  <- tapply(x, group, var)
  abs(diff(gmean))/sqrt(sum(gvar)/2)
}
apply(bank, 2, cohens.d.equal, group=group)
#4f
param <- apply(bank, 2, function(v) tapply(v, group, mean))
param
for (i in 1:ncol(bank)) {
  print(t.test(bank[101:200,i], mu=param[1,i]))
}
#
sign.test <- function(x, theta0=mean(x), ...) {
  ntie <- sum(x==theta0)
  nlow <- sum(x<theta0)
  nupp <- sum(x>theta0)
  if (nlow<nupp) nlow <- nlow+ntie 
  binom.test(nlow, length(x), p=0.5, ...)
}

param <- apply(bank, 2, function(v) tapply(v, group, median))
param
for (i in 1:ncol(bank)) {
  print(sign.test(bank[101:200,i], theta0=param[1,i]))
}
