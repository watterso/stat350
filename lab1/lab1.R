library(lattice)
## ---- A.1
iqs <- read.table('eg01-14iq.txt', header=TRUE)
## ---- A.2
histogram(~IQ, iqs, main = "Histogram of IQs", col="blue")
## ---- B.1
sev <- read.table('ex01-44sevengr.txt', header=TRUE)
## ---- B.2
histogram(~ID, sev, main="Histogram of IDs", col="blue")
histogram(~GPA, sev, main="Histogram of GPAs", col="blue")
histogram(~IQ, sev, main="Histogram of IQs", col="blue")
histogram(~SelfConcept, sev, main="Histogram of Self Concept", col="blue")
