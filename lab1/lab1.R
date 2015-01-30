library(lattice)
iqs <- read.table('eg01-14iq.txt', header=TRUE)
histogram(~IQ, iqs, main = "Histogram of IQs", col="blue")

sev <- read.table('ex01-44sevengr.txt', header=TRUE)
histogram(~ID, sev, main="Histogram of IDs", col="blue")
histogram(~GPA, sev, main="Histogram of GPAs", col="blue")
histogram(~IQ, sev, main="Histogram of IQs", col="blue")
histogram(~SelfConcept, sev, main="Histogram of Self Concept", col="blue")
