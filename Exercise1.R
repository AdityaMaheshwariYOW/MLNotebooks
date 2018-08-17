set.seed(100)

two <- rnorm(100, 0, 1)
print(0)
print(mean(two))

rgen <- function(x, y=100) {
  set.seed(y)
  return (rnorm(100))
}

Mode <- function(x) {
  ux <- unique(x)
  return (ux[which.max(tabulate(match(x, ux)))])
}

mmm <- function(x) {
  return (c(mean(x), median(x), Mode(x)))
}

x <- rgen(1000)
six <- length(x[x>.2])

tricky <- six*two

install.packages(housingData)
library(housingData)
myDf <- housingData::fipsCounty

sort(table(myDf$state), decreasing = TRUE)[1]

housingDf <- housingData::housing
sort(table(housing$state), decreasing = TRUE)[1]

states <- names(table(housing$state))

lists <- c()
sells <- c()
for (i in 0:(length(states)-1)) {
  lpr <- na.omit(housingDf$medListPriceSqft[housingDf$state == states[i]])
  spr <- na.omit(housingDf$medSoldPriceSqft[housingDf$state == states[i]])
  lists[i] <- sum(lpr)/max(length(lpr),1)
  sells[i] <- sum(spr)/max(length(spr),1)
}

max(lists-sells)

plot(housingDf$medListPriceSqft, housingDf$medSoldPriceSqft)

newVec <- housingDf$medListPriceSqft/housingDf$medSoldPriceSqft

library(dplyr)
library(magrittr)
summary(newVec)


