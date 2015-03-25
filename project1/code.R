## ---- Setup
library(lattice)
library(xtable)
avg_sd_rpt <- function(x){
  ind <- x[1,1]
  avg <- mean(x[,2])
  stddev <- sd(x[,2])
  c(ind, avg, stddev)
}
final_rpt <- function(x, avg, std){
  ret <- do.call(rbind, lapply(x, function(y) c(y[1], y[2], avg, y[3], std/sqrt(y[1]))))
  colnames(ret) <- c('n', 'Sample Mean', 'Theoretical Mean', 'Sample Std Dev', 'Theoretical Std Dev')
  ret
}
dice_final_rpt <- function(x, avg){
  ret <- do.call(rbind, lapply(x, function(y) c(y[1], y[2], avg*y[1], y[3], sqrt(avg*y[1]*(1-avg)))))
  colnames(ret) <- c('n', 'Sample Mean', 'Theoretical Mean', 'Sample Std Dev', 'Theoretical Std Dev')
  ret
}
my_histogram <- function(x, avg, std){
  histogram(x,
            panel = function(x){
              panel.histogram(x, breaks=NULL)
              panel.mathdensity(dmath = dnorm, col = "red", args = list(mean=avg,sd=std), lwd=3)
            },
            type="density")
}
my_qqwithline <- function(data, title=NULL){
  qqmath(data,
         panel = function(x) {
           panel.qqmathline(x, distribution = qnorm, lwd=3)
           panel.qqmath(x)
         },
         main=title)
}
exp_clt <- function(n,rate=2,s=1000){
  data.vec <- rexp(s*n, rate)
  data.mat <- matrix(data.vec, ncol = n)
  avgs <- apply(data.mat, 1, mean)
  matrix(c(rep(n,s),avgs),s,2)
}
pois_clt <- function(n,lambda=1,s=1000){
  data.vec <- rpois(s*n, lambda)
  data.mat <- matrix(data.vec, ncol = n)
  avgs <- apply(data.mat, 1, mean)
  matrix(c(rep(n,s),avgs),s,2)
}
dice_clt <- function(n,side=2,s=1000){
  data.vec <- sample(6, n*s, replace=T)
  data.mat <- matrix(data.vec, ncol = n)
  avgs <- apply(data.mat, 1, function(x)sum(x==side))
  matrix(c(rep(n,s),avgs),s,2)
}
## ---- 4
sample_sizes <- c(1,5,seq(10,90,10))
x <- lapply(sample_sizes, exp_clt)
t <- lapply(x,avg_sd_rpt)
f <- final_rpt(t, .5, .5)
## ---- 4_table
print(xtable(f, digits=c(1,0,3,1,3,3)), include.rownames=FALSE, add.to.row = list(pos = list(10), command = rep("\\rowcolor[gray]{0.75}",1)))
## ---- 4_graphs
# At n=80 the graph looks the most normal
my_histogram(x[[10]][,2], f[10,3], f[10,5])
my_qqwithline(x[[10]][,2])
## ---- 5
sample_sizes <- c(1,2,5,seq(10,50,10))
x <- lapply(sample_sizes, pois_clt)
t <- lapply(x,avg_sd_rpt)
f <- final_rpt(t, 1, 1)
## ---- 5_table
print(xtable(f, digits=c(1,0,3,1,3,3)), include.rownames=FALSE, add.to.row = list(pos = list(6), command = rep("\\rowcolor[gray]{0.75}",1)))
## ---- 5_graphs
# At n=30 the graph looks the most normal
my_histogram(x[[6]][,2], f[6,3], f[6,5])
my_qqwithline(x[[6]][,2])
## ---- 6
sample_sizes <- c(1,2,seq(5,35,5))
x <- lapply(sample_sizes, dice_clt)
t <- lapply(x,avg_sd_rpt)
f <- dice_final_rpt(t, 0.166666)
## ---- 6_table
print(xtable(f, digits=c(1,0,3,3,3,3)), include.rownames=FALSE, add.to.row = list(pos = list(8), command = rep("\\rowcolor[gray]{0.75}",1)))
## ---- 6_graphs
# At n=30 the graph looks the most normal
my_histogram(x[[9]][,2], f[9,3], f[9,5])
my_qqwithline(x[[9]][,2])
