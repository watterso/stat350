## ---- setup
library(lattice)
my_format <- function(val){
  format(val,digits=3,big.interval=3, scientific=FALSE, big.mark=',')
}
lin_reg_plot <- function(data, ...){
  xyplot(data,
         panel = function(x,y){
          panel.xyplot(x,y)
          panel.lmline(x,y)
         },...)
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
sales <- read.table("sales.txt", header=T)
## ---- A_1
lin_reg_plot(sales$SalesPrice ~ sales$AssessedValue, xlab="Assessed Value", ylab="Sales Price")
## ---- A_3
sales.cor <- cor(sales$SalesPrice, sales$AssessedValue)
## ---- A_5
sales.lm <- lm(sales$SalesPrice ~ sales$AssessedValue)
## ---- A_6
sales.predict <- predict(sales.lm)
## ---- A_7
sales.resid <- sales.lm$residuals
xyplot(sales.resid ~ sales$AssessedValue, panel=function(x,y){panel.xyplot(x,y);panel.abline(h=0)})
## ---- A_8
rh <- my_histogram(sales.resid, mean(sales.resid), sd(sales.resid), col="Blue", xlab="Residuals")
rq <- my_qqwithline(sales.resid)
print(rh,position=c(0,0,0.5,1), more=TRUE)
print(rq,position=c(0.5,0,1,1))
## ---- A_10
lin_reg_plot(sales$AssessedValue ~ sales$SalesPrice, ylab="Assessed Value", xlab="Sales Price")
sales.cor1 <- cor(sales$AssessedValue,sales$SalesPrice)
sales.lm1 <- lm(sales$AssessedValue ~ sales$SalesPrice)
## ---- A_12
sales.conf <- confint(sales.lm, level = 0.99)
## ---- A_13
resume <- summary(sales.lm)
