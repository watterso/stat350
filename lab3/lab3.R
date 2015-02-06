## ---- Imports
library(lattice)
library(xtable)
## ---- Functions
my_qqwithline <- function(data, title=NULL){
  qqmath(data,
         panel = function(x) {
           panel.qqmathline(x, distribution = qnorm)
           panel.qqmath(x)
         },
         main=title)
}
my_histogram <- function(data, title=NULL){
  histogram(~data, type="density", col="blue", main=title)
}
## ---- A
nums_10 <- rnorm(10, mean=2, sd=10)
## ---- A_1
my_histogram(nums_10)
## ---- A_2
my_qqwithline(nums_10);
## ---- B
nums_100 <- rnorm(100, mean=2, sd=10)
## ---- B_1
my_histogram(nums_100)
## ---- B_2
my_qqwithline(nums_100);
## ---- C
n <- 100
right_skew <- rexp(n,rate=2)
left_skew <- rbeta(n,2,0.5,ncp=2)
short_tail <- runif(n,min=0,max=2)
long_tail <- rcauchy(n,location=0,scale=1)
## ---- C_1
my_histogram(right_skew, "Right Skewed")
my_histogram(left_skew, "Left Skewed")
my_histogram(short_tail, "Short Tailed")
my_histogram(long_tail, "Long Tailed")
## ---- C_2
my_qqwithline(right_skew, "Right Skewed")
my_qqwithline(left_skew, "Left Skewed")
my_qqwithline(short_tail, "Short Tailed")
my_qqwithline(long_tail, "Long Tailed")
## ---- D
call_data <- read.table("eg01-15calls80.txt", header=TRUE)
## ---- D_1
my_histogram(call_data, "Service Times for Calls")
## ---- D_2
my_qqwithline(call_data[,1], "Service Times for Calls")
## ---- Data
xtable(matrix(nums_10,ncol=10), caption="nums{\\_}10")
xtable(matrix(nums_100,ncol=10), caption="nums{\\_}100")
xtable(matrix(right_skew,ncol=10), caption="right{\\_}skew")
xtable(matrix(left_skew,ncol=10), caption="left{\\_}skew")
xtable(matrix(short_tail,ncol=10), caption="short{\\_}tail")
xtable(matrix(long_tail,ncol=10), caption="long{\\_}tail")