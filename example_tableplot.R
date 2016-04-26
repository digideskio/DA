library("tabplot")
library("MASS")
Boston$chas <- factor(Boston$chas)
Boston$rad <- ordered(Boston$rad)
tableplot(Boston)

getwd()
setwd("C:/Users/Theo/Desktop/Organisatorisches/01_Uni/BERLIN - HU/4. Semester/Datenanalyse I/R/Datenanalyse-I/daten")
bost <- read.csv2(file="bostonc.csv", header = TRUE)

tableplot(Boston, sortCol = "tax")

# load diamonds dataset from ggplot2
require(ggplot2)
data(diamonds)

# default tableplot
tableplot(diamonds)

bank2 <- read.csv2(file = "bank2.csv", header = TRUE)
bank2
head(bank2)
#
# to read spss - foreign must be loaded
library("foreign")
 x <- read.spss(file ="BANK2.sav", to.data.frame = TRUE)
 tableplot(x, sortCol = "DIAGONAL")
 
 