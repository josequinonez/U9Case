# U9 Case
Jose Q  
14 de julio de 2016  




```r
## Load libraries
library(tseries)
```

```
## Warning: package 'tseries' was built under R version 3.3.1
```

```r
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
```

### Selected quote: Procter & Gamble (PG)  

* Source: http://finance.yahoo.com/quote/PG  


```r
## Entering data
MyQuote <- "PG"
SNPdata <- get.hist.quote(MyQuote, quote="Close")
```


```r
## Calculate log returns
SNPret <- log(lag(SNPdata)) - log(SNPdata)
```


```r
## Calculate volatility measures
SNPvol <- sd(SNPret) * sqrt(250) * 100
```


```r
## Calculate volatility using three different decay factors.
DecayFactor1 <- 1   ## Low values in the decay factor
DecayFactor2 <- 50  
DecayFactor3 <- 100 ## High values in the decay factor

volest1 <- getVol(DecayFactor1,SNPret)
volest2 <- getVol(DecayFactor2,SNPret)
volest3 <- getVol(DecayFactor3,SNPret)
```


```r
## Plot volatility data
plot(volest1,type="l", xlab="Number of day", ylab = "Volatility", main= "Volatility by decay factor: cyan = 1, red = 50, blue = 100", col="cyan")

lines(volest2,type="l",col="red")

lines(volest3, type = "l", col="blue")
```

![](Paper_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


```r
MaxVolDay <- which.max(volest1)

paste ("Max volatility = ", max(volest1), " in day ", MaxVolDay, " ", attributes(SNPdata[368])$index, "due to a change from stock price from ", SNPdata[MaxVolDay], " to ", SNPdata[MaxVolDay+1], "." )
```

```
## [1] "Max volatility =  0.700675446980737  in day  367   1992-06-15 due to a change from stock price from  100  to  49.625 ."
```

```r
paste ("The average variation from day to day is +/- ", exp(mean(SNPret)), ".")
```

```
## [1] "The average variation from day to day is +/-  1.00000070755936 ."
```

### Conclusion  

#### The stock price for PG has been very stable, except for four events with extreme changes.  

#### The decay factor of 100 shows a more normalized curve. It means that in the long run, this is a very stable quote with few changes, reducing the importance to those few peaks.  
###  


