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
fb_data <- read.table("facebookfriends.txt", header=TRUE)
pick_data <- read.table("pickcount.txt", header=TRUE)
## ---- A_1
my_histogram(fb_data$Friends, mean(fb_data$Friends), sd(fb_data$Friends), col="Blue", xlab="Facebook Friends")
my_qqwithline(fb_data$Friends)
## ---- A_3
a <- qt(.975, length(fb_data$Friends)-1)
x_bar <- mean(fb_data$Friends)
std <- sd(fb_data$Friends)
stderr <- std / sqrt(length(fb_data$Friends))
marg_error <- a * stderr
conf_int <- c(x_bar-marg_error, x_bar+marg_error)
## ---- A_4
t.test(fb_data$Friends,conf.level=0.95, mu=130)
## ---- B_1
my_histogram(pick_data$PickCount, mean(pick_data$PickCount), sd(pick_data$PickCount), col="Blue", xlab="# of Picks")
my_qqwithline(pick_data$PickCount)
bwplot(pick_data$PickCount, xlab="# of Picks")
## ---- B_4
x_bar <- mean(pick_data$PickCount)
std <- sd(pick_data$PickCount)
stderr <- std / sqrt(length(pick_data$PickCount))
## ---- B_5
a <- qt(.975, length(pick_data$PickCount)-1)
marg_error <- a * stderr
lower_conf <- x_bar-marg_error
## ---- B_6 
b <- t.test(pick_data$PickCount, conf.level=0.95, mu = 925, alternative = "greater")
## ---- B_7
b <- t.test(pick_data$PickCount, conf.level=0.95, mu = 935, alternative = "greater")
