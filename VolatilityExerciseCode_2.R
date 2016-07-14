## Load libraries
library(tseries)

## Define getVol function to Volatility
getVol <- function(d, logrets)
{
  
  var = 0
  
  lam = 0
  
  varlist <- c()
  
  for (r in logrets) {
    
    lam = lam*(1 - 1/d) + 1
    
    var = (1 - 1/lam)* var + (1/lam)*r^2
    
    varlist <- c(varlist, var)
    
  }
  
  sqrt(varlist)
}


## Entering data
MyQuote <- "PG"
SNPdata <- get.hist.quote(MyQuote, quote="Close")

## Calculate log returns
SNPret <- log(lag(SNPdata)) - log(SNPdata)

## Calculate volatility measures
SNPvol <- sd(SNPret) * sqrt(250) * 100

## Calculate  volatility using three different decay factors.
DecayFactor1 <- 1
DecayFactor2 <- 50
DecayFactor3 <- 100

volest1 <- getVol(DecayFactor1,SNPret)
volest2 <- getVol(DecayFactor2,SNPret)
volest3 <- getVol(DecayFactor3,SNPret)


plot(volest1,type="l")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")





## Exponential Smoothing

data(oil)
oil
data <- window(oil, start=1996,end=2007)

plot(oildata, ylab="Oil (millions of tonnes)",xlab="Year")

fit1 <- ses(oildata, alpha=0.2, initial="simple", h=3)

fit2 <- ses(oildata, alpha=0.6, initial="simple", h=3)

fit3 <- ses(oildata, h=3)

plot(fit1, plot.conf=FALSE, ylab="Oil (millions of tonnes)",
     xlab="Year", main="", fcol="white", type="o")

lines(fitted(fit1), col="blue", type="o")

lines(fitted(fit2), col="red", type="o")

lines(fitted(fit3), col="green", type="o")

lines(fit1$mean, col="blue", type="o")

lines(fit2$mean, col="red", type="o")

lines(fit3$mean, col="green", type="o")

legend("topleft",lty=1, col=c(1,"blue","red","green"),
       c("data", expression(alpha == 0.2), expression(alpha == 0.6),
         expression(alpha == 0.89)),pch=1)



## Estimating Trend with Moving Average
install.packages("TTR")

library(TTR)
kingsts.ma3 <- SMA(kingsTS,n=3)

plot.ts(kingsts.ma3)

kingsts.ma8 <- SMA(kingsTS,n=8)

plot.ts(kingsts.ma8)


## Describe the trend


## Decomposing a seasonal time series

data(elecequip)

fit <- stl(elecequip, s.window=5)

plot(elecequip, col="gray",
     main="Electrical equipment manufacturing",
     ylab="New orders index", xlab="")

lines(fit$time.series[,2],col="red",ylab="Trend")

plot(fit)

monthplot(fit$time.series[,"seasonal"],main='',ylab="Seasonal")



## Moving average
plot(elecsales, main="Residential electricity sales",
     ylab="GWh", xlab="Year")

lines(ma(elecsales,5),col="red")



## Moving average with seasonal data to estimate trend

plot(elecequip, ylab="New orders index", col="gray",
     main="Electrical equipment manufacturing (Euro area)")

lines(ma(elecequip, order=12), col="red")




## The birth data with "decompose"
births.ts
Comp <- decompose(birth.ts)
plot(births.tsComp)


## Describe the components


## Seasonal adjustment

births.tsCompadj <- births.ts - births.tsComp$seasonal

plot(births.tsCompadj)


plot(elecequip, col="grey",
     main="Electrical equipment manufacturing",
     xlab="", ylab="New orders index")

lines(seasadj(fit),col="red",ylab="Seasonally adjusted")



## More sophisticated adjustment using LOESS

birth.stl <- stl(births.ts,s.window=7)

plot(birth.stl)