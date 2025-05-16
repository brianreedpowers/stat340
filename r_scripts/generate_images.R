## Generate Images

## Intro

## Education by Income
set.seed(2)
yrs <- rbinom(100,20,.40)+8
inc <- (50 + (yrs-12)*(170-55)/10 + rnorm(100,0,15) )/ 10
png(filename="./images/edu_income.png",  
    width     = 3.25,
    height    = 3.25,
    units     = "in",
    res       = 300,
    pointsize = 8)
plot(inc~yrs, col="red", pch=16, main="Income by Education", xlab="education (years)", ylab="income ($10ks)")
dev.off()
