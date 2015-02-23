## ---- 1_A
lambda=2
curve(lambda*exp(-lambda*x), 0, 10, main="a. PDF")
curve(1-exp(-lambda*x), 0, 10, main="a. CDF")
## ---- 1_B
curve(3*(8*x-x^2)/256, 0, 8, main="b. PDF")
curve(1-(-3*(-4*x^2 + x^3/3))/256, 0, 8, main="b. CDF")
## ---- 1_C
curve(2/x^3, 1, 10, main="c. PDF")
curve(1-1/x^2, 1, 10, main="c. CDF")
## ---- 1_D
curve(1/x, 1, 10, main="d. PDF")
curve(1-log(x), 1, 10, main="d. CDF")
## ---- 2_B
curve(x/8, 2, 4, main="PDF")
curve(x^2/16, 2, 4, main="CDF")
## ---- 5_A
lambda=0.5
curve(lambda*exp(-lambda*x), 0, 10, main="PDF")
curve(1-exp(-lambda*x), 0, 10, main="CDF")