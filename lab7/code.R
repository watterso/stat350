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

isos <- read.table("ex12-45bmd.txt", header=TRUE)
jump <- read.table("ex12-47jump.txt", header=TRUE)

## ---- A_1
con <- subset(isos, isos$Treatment == 'Control')$BMD
low <- subset(isos, isos$Treatment == 'LowDose')$BMD
hi <- subset(isos, isos$Treatment == 'HighDose')$BMD
data = list(con,low,hi)
info <- data.frame(Treatment=unique(isos$Treatment), Size=sapply(data, length), Mean=sapply(data, mean), Std=sapply(data, sd))
print(xtable(info), include.rownames=FALSE)
bwplot(BMD ~ Treatment, isos)

## ---- A_2
ch <- my_histogram(con, mean(con), sd(con), col="Blue", xlab="Control")
cq <- my_qqwithline(con)
print(ch,position=c(0,0,0.5,1), more=TRUE)
print(cq,position=c(0.5,0,1,1))
lh <- my_histogram(low, mean(low), sd(low), col="Blue", xlab="Low Dose")
lq <- my_qqwithline(low)
print(lh,position=c(0,0,0.5,1), more=TRUE)
print(lq,position=c(0.5,0,1,1))
hh <- my_histogram(hi, mean(hi), sd(hi), col="Blue", xlab="High Dose")
hq <- my_qqwithline(hi)
print(hh,position=c(0,0,0.5,1), more=TRUE)
print(hq,position=c(0.5,0,1,1))

## ---- A_3
le_aov <- aov(BMD ~ Treatment, data=isos)
resume <- summary(le_aov)

## ---- A_4
tuk <- TukeyHSD(le_aov, conf.level = 0.95)
plot(tuk)

## ---- B_1
con <- subset(jump, jump$group == 'Control')$density
low <- subset(jump, jump$group == 'Lowjump')$density
hi <- subset(jump, jump$group == 'Highjump')$density
data = list(con,low,hi)
info <- data.frame(Group=unique(jump$group), Size=sapply(data, length), Mean=sapply(data, mean), Std=sapply(data, sd))
print(xtable(info), include.rownames=FALSE)
bwplot(density ~ group, jump)

## ---- B_2
ch <- my_histogram(con, mean(con), sd(con), col="Blue", xlab="Control")
cq <- my_qqwithline(con)
print(ch,position=c(0,0,0.5,1), more=TRUE)
print(cq,position=c(0.5,0,1,1))
lh <- my_histogram(low, mean(low), sd(low), col="Blue", xlab="Low Jumps")
lq <- my_qqwithline(low)
print(lh,position=c(0,0,0.5,1), more=TRUE)
print(lq,position=c(0.5,0,1,1))
hh <- my_histogram(hi, mean(hi), sd(hi), col="Blue", xlab="High Jumps")
hq <- my_qqwithline(hi)
print(hh,position=c(0,0,0.5,1), more=TRUE)
print(hq,position=c(0.5,0,1,1))

## ---- B_3
le_aov <- aov(density ~ group, data=jump)
resume <- summary(le_aov)

## ---- B_4
tuk <- TukeyHSD(le_aov, conf.level = 0.95)
plot(tuk)
