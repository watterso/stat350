## ---- setup
library(lattice)
lin_reg_plot <- function(data, ...){
  xyplot(data,
         panel = function(x,y){
          panel.xyplot(x,y)
          panel.lmline(x,y)
         },...)
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
