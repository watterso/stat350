## ---- setup
library(lattice)
library(xtable)
my_qqwithline <- function(data, title=NULL){
  qqmath(data,
         panel = function(x) {
           panel.qqmathline(x, distribution = qnorm, lwd=2)
           panel.qqmath(x)
         },
         main=title)
}
my_histogram <- function(x, avg, std, ...){
  histogram(x,
            panel = function(x){
              panel.histogram(x, breaks=NULL, ...)
              panel.mathdensity(dmath = dnorm, col = "red", args = list(mean=avg,sd=std), lwd=3)
            },
            type="density",...)
}
houses <- read.table("houseprice.txt", header=TRUE)
weight <- read.table("ex07-36wtgain.txt", header=TRUE)
# convert to pounds
weight$wta <- weight$wta*2.2
weight$wtb <- weight$wtb*2.2
## ---- A_2
my_data <- weight$wta - weight$wtb
my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Weight Gain")
my_qqwithline(my_data)
## ---- A_3
conf <- t.test(weight$wta, weight$wtb, paired=TRUE, mu=16)
## ---- B_2
my_data <- subset(houses,subset = houses$Bedroom == 3)$Price
my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Weight Gain")
my_qqwithline(my_data)
my_data <- subset(houses,subset = houses$Bedroom == 4)$Price
my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Weight Gain")
my_qqwithline(my_data)
my_data <- houses$Price
my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Weight Gain")
my_qqwithline(my_data)
