## ---- setup
library(lattice)
library(xtable)
## ---- A
conf_data <- matrix(rnorm(40*30,10,2), ncol=30)
conf_res <- apply(conf_data,2,function(x) t.test(x,conf.level=0.95, mu=10))
conf_intervals <- matrix(unlist(lapply(conf_res,function(y)c(y[[4]][1],y[[4]][2],y[[4]][1]<10 & y[[4]][2]>10))), ncol=3, byrow=TRUE)
mean_in_range <- sum(apply(conf_intervals, 1, function(x)x[1]<10 & x[2]>10))
## ---- A_table
print(xtable(conf_intervals), include.colnames=FALSE)
## ---- B
power_curve <- function(n,a,mu0,std,int_range=mu0*0.01*2){
  std_n <- std/sqrt(n)
  z <- qnorm(1-a/2)
  mu_p <- seq(mu0-int_range/2, mu0+int_range/2, mu0*0.0001)
  x1 <- mu0 - z*std_n
  x2 <- mu0 + z*std_n
  p_x1 <- pnorm(x1, mu_p, std_n)
  p_x2 <- pnorm(x2, mu_p, std_n, lower.tail=FALSE)
  matrix(c(mu_p,(p_x1 + p_x2)), ncol=2)
}
specific_power <- function(n,a,mu0,std,mu_a){
  std_n <- std/sqrt(n)
  z <- qnorm(1-a/2)
  mu_p <- mu_a
  x1 <- mu0 - z*std_n
  x2 <- mu0 + z*std_n
  p_x1 <- pnorm(x1, mu_p, std_n)
  p_x2 <- pnorm(x2, mu_p, std_n, lower.tail=FALSE)
  return(p_x1 + p_x2)
}
## ---- B_1
b1a <- specific_power(3,0.01,6,0.25,6.5)
b1b <- specific_power(3,0.05,6,0.25,6.5)
b1c <- specific_power(3,0.01,6,0.25,6.75)
b1d <- specific_power(5,0.01,6,0.25,6.5)
## ---- B_2
power_vals <- power_curve(3,0.01,6,0.25,3)
xyplot(power_vals[,2]~power_vals[,1], xlab=expression(paste(mu," Prime")), ylab="Power", size=1, type="l")
## ---- B_3
powers <- sapply(1:20,function(x)specific_power(x,0.01,6,0.25,6.3))
desired_power <- 0.9
cond_arr <- sapply(powers,function(x)x>desired_power)
#cond_arr starts with false, so the first true is located at size-sum
calc_n_size <- length(cond_arr)-sum(cond_arr)
## ---- B_3_graph
xyplot(powers ~ 1:20,
       panel = function(x, y){
         panel.abline(h = desired_power)
         panel.abline(v = calc_n_size)
         panel.xyplot(x, y, type="l")
       }, xlab="Sample Size", ylab="Power")
