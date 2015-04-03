## ---- setup
library(lattice)
library(xtable)
my_format <- function(val){
  format(val,digits=3,big.interval=3, scientific=FALSE, big.mark=',')
}
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
a <- my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Weight Gain")
b <- my_qqwithline(my_data)
# graphs
print(a,position=c(0,0,0.5,1), more=TRUE)
print(b,position=c(0.5,0,1,1))
## ---- A_3
conf <- t.test(weight$wta, weight$wtb, paired=TRUE, mu=16)
## ---- B_2
my_data <- subset(houses,subset = houses$Bedroom == 3)$Price
lh <- my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Housing Prices")
lb <-bwplot(my_data, xlab = "Housing Prices")
lq <- my_qqwithline(my_data)
my_data <- subset(houses,subset = houses$Bedroom == 4)$Price
rh <- my_histogram(my_data, mean(my_data), sd(my_data), col="Blue", xlab="Housing Prices")
rb <-bwplot(my_data, xlab = "Housing Prices")
rq <- my_qqwithline(my_data)
# histograms
print(lh,position=c(0,0,0.5,1), more=TRUE)
print(rh,position=c(0.5,0,1,1))
#qq's
print(lq,position=c(0,0,0.5,1), more=TRUE)
print(rq,position=c(0.5,0,1,1))
#boxplots
print(lb,position=c(0,0,1,0.5), more=TRUE)
print(rb,position=c(0,0.5,1,1))
## ---- B_4
three <- subset(houses, subset = houses$Bedroom == 3)$Price
four <- subset(houses, subset = houses$Bedroom == 4)$Price
conf <- t.test(four, three, paired = FALSE, conf.level = 0.99)
