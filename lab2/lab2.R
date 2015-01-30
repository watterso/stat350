library(lattice)
#--=PART A=--
d_data <- read.table("earthdensity.txt", header=TRUE)
# A.1
d_fnum <- fivenum(d_data[,1])
# A.2
bwplot(d_data[,1], xlab="Water Density Coefficient")
# A.3
d_mean <- mean(d_data[,1])
d_sd <- sd(d_data[,1])
d_median <- median(d_data[,1])
# yes the median is close to the mean, it is within 1 standard deviation
# A.4
water_density <- 999.97
earth_density <- d_mean*water_density
# mean and median are so close together you can use either value
# to estimate the actual density of the earth
#--=PART B=--
crp <- read.table("ex01-73crp.txt", header=TRUE)
# B.1
f_num <- fivenum(crp[,1])
# B.2
bwplot(crp[,1], xlab="CRP Concentration (mg/l)")      #right skewed
# B.3
iqr <- f_num[4]-f_num[2]
min_out <- f_num[2] - 1.5*iqr
max_out <- f_num[4] + 1.5*iqr
outliers <- c(crp[crp$CRP < min_out,], crp[crp$CRP > max_out,])
# B.4
histogram(~CRP,crp, xlab="CRP Concentration (mg/l)", col="blue")
# yes IQR rule properly highlights outliers