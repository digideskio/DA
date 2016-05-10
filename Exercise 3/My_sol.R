#Exercise 3
library(Rmisc)
library(boot)

#Task 1
setwd(paste(getwd(),"/Daten",sep=""))
boston <- read.csv2(file="bostonh.csv")

#Task 2
# a)
mean(boston$MEDV)
median(boston$MEDV)
# b)
boston_sample <- sample(boston$MEDV,100,replace = T)
mean(boston_sample)
median(boston_sample)

#c)
boston_sample_mean <- vector(mode = "numeric",length =150)
for (i in 1:150){
boston_sample_mean[i] <- mean(sample(boston$MEDV,100,replace = T))
}
hist(boston_sample_mean)

boston_sample_median <- vector(mode = "numeric",length =50)
for (i in 1:150){
  boston_sample_median[i] <- median(sample(boston$MEDV,100,replace = T))
}
hist(boston_sample_median)

Conf_mean <- CI(boston_sample_mean, ci= 0.95)

count = 0
count2 = 0
for (obs in boston_sample_mean){
  if(obs > CI(boston_sample_mean, ci= 0.95)[1] | obs < CI(boston_sample_mean, ci= 0.95)[3]){
    count = count + 1
  }else{
    count2 = count2 +1
  }
}
In_conf_mean = count/(count+count2)
Out_conf_mean = 1- In_conf_mean

count = 0
count2 = 0
for (obs in boston_sample_median){
  if(obs > CI(boston_sample_median, ci= 0.95)[1] | obs < CI(boston_sample_median, ci= 0.95)[3]){
    count = count + 1
  }else{
    count2 = count2 +1
  }
}
In_conf_median = count/(count+count2)
Out_conf_median = 1- In_conf_mean

#d
meanboot <- function(){return(mean(boston_sample))}
boot

