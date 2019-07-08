#### PACKAGES ####
rm(list=ls())
require(forecast)
require(rugarch)
require(quantmod)
#### RETRIEVAL OF DATA ####
startDate <- as.Date("2010-01-01")
endDate <- as.Date(as.POSIXlt(Sys.Date()))
getSymbols("LGO.TO",src="yahoo",from=startDate, to=endDate, periodicity = "daily")
LGO.TO <- na.omit(LGO.TO)
#### TREATMENT OF RAW DATA ####
# Cl function retrieves close from the OHLC!
Rets = diff( log( Cl( LGO.TO ) ) ) 
Close = Cl(LGO.TO)
# Returns that we will use
Tail.Ret <- as.ts( tail( Rets, 1000 ) )
Tail.Cl <- as.ts( tail( Close, 1000 ) )

# Specifications of the model
ugarchspecs <- ugarchspec(variance.model=list(garchOrder = c(1, 1)),mean.model=list(
       armaOrder=c(0,1),arfima = FALSE), distribution="std")

#### ESTIMATION OF MODEL ####
ugarch <- ugarchfit(ugarchspecs, Tail.Ret)
# Squared Residuals (Epsilon squared)
resid2 <- (ugarch@fit$residuals)^2
resid <- (ugarch@fit$residuals)
# Variance (Sigma squared)
var <- ugarch@fit$var
# Yt model
Yt <- ugarch@fit$fitted.values

#### VISUALIZATION: STOCK ####
par(mfrow=c(1,1))
# Candlesticks chart
candleChart(LGO.TO, subset='last 3000 days')
# Closing Price
plot(Tail.Cl, type = "l", col= "blue")
# Returns
plot(Tail.Ret, main="First difference of Log Transformation of the Prices",
     ylab = "Returns %", xlab = "Number of trading days since Jan.2015")
# Sigma
plot(ugarch@fit$sigma, type = "l", main="Volatility (sigma) of the Model",
     ylab = "Volatility", xlab = "Number of trading days since Jan.2015")
# Returns vs Yt process
plot(as.numeric(Tail.Ret),type="l")
lines((ugarch@fit$fitted.values),type="l",col="green")
# ugarch@fit$residuals+
# Analyze
coef(ugarch)
names(ugarch@fit)

#### FORECASTS ####
Vtf <- 50 # Visualization time frame
Vtf_d <- 5 # Forecast duration

Vtf_fb <- Vtf + 1 # Forecast begin 
Vtf_fe <- Vtf + Vtf_d # Forecast end 

fc <- ugarchforecast(ugarch, n.ahead = Vtf_d)
fc
fc.sig <- fc@forecast$sigmaFor
fc.ser <- fc@forecast$seriesFor
plot(fc.sig, type = "l") # Forecast of volatility 
plot(fc.ser, type = "l") # Forecast of the process

# get the last x observations for the variance (Sigma squared)
var.tail <- c(tail(var,Vtf),rep(NA,Vtf_d))  
# get the last x observations for residuals (Epsilon squared)
# plot of the volatility, its forecast versus the residuals of the model
resid2.tail <- c(tail(resid2,Vtf),rep(NA,Vtf_d))  
fc2 <- c(rep(NA,Vtf),(fc.sig)^2)
plot(resid2.tail, type="l")
lines(fc2, col="purple")
lines(var.tail, col="brown")
# series (returns) forecast
Yt.tail <- c(tail(Yt,Vtf),rep(NA,Vtf_d))
Tail.Ret.tail <- c(tail(Tail.Ret,Vtf),rep(NA,Vtf_d))
fc3 <- c(rep(NA,Vtf),(fc.ser))

# Different intervals to be displayed

upper.60 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 0.842 * (fc.sig)) # 0.842 is crit for 60% C.I
lower.60 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 0.842 * (fc.sig))

upper.70 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 1.036 * (fc.sig)) # 1.036 -> 70% C.I
lower.70 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 1.036 * (fc.sig))

upper.80 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 1.282 * (fc.sig)) # 1.282 -> 80% C.I
lower.80 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 1.282 * (fc.sig))

upper.90 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] + 1.645 * (fc.sig)) # 1.282 -> 90% C.I
lower.90 <- c(rep(NA,Vtf), fc3[Vtf_fb:Vtf_fe] - 1.645 * (fc.sig))

# Returns prediction
plot(Tail.Ret.tail, type="l")
lines(Yt.tail, col = "brown")
lines(fc3, col="purple"); lines(upper.60,col ="red"); lines(lower.60,col ="red")

# Series forecast transformed back to daily close

# forecast price
# Remove the "logness"
fc.ret <- exp(fc.ser) 
# Feed it the first real price
fc.ret[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret[1] 
# Translation from Returns to Price
fcp <- cumprod(fc.ret) 

# Forecast prices bounds confidence intervals
## 60
# Remove the "logness"
fc.ret.upper.60 <- exp(upper.60[Vtf_fb:Vtf_fe]) ; fc.ret.lower.60 <- exp(lower.60[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.60[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.upper.60[1] ; fc.ret.lower.60[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.lower.60[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.60 <- cumprod(fc.ret.upper.60); fcp.lower.60 <- cumprod(fc.ret.lower.60)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.60 <- c(rep(NA,Vtf), fcp.upper.60); fcp.lower.60 <- c(rep(NA,Vtf), fcp.lower.60)

## 70
# Remove the "logness"
fc.ret.upper.70 <- exp(upper.70[Vtf_fb:Vtf_fe]) ; fc.ret.lower.70 <- exp(lower.70[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.70[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.upper.70[1] ; fc.ret.lower.70[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.lower.70[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.70 <- cumprod(fc.ret.upper.70); fcp.lower.70 <- cumprod(fc.ret.lower.70)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.70 <- c(rep(NA,Vtf), fcp.upper.70); fcp.lower.70 <- c(rep(NA,Vtf), fcp.lower.70)

## 80
# Remove the "logness"
fc.ret.upper.80 <- exp(upper.80[Vtf_fb:Vtf_fe]) ; fc.ret.lower.80 <- exp(lower.80[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.80[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.upper.80[1] ; fc.ret.lower.80[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.lower.80[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.80 <- cumprod(fc.ret.upper.80); fcp.lower.80 <- cumprod(fc.ret.lower.80)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.80 <- c(rep(NA,Vtf), fcp.upper.80); fcp.lower.80 <- c(rep(NA,Vtf), fcp.lower.80)

## 90
# Remove the "logness"
fc.ret.upper.90 <- exp(upper.90[Vtf_fb:Vtf_fe]) ; fc.ret.lower.90 <- exp(lower.90[Vtf_fb:Vtf_fe]) 
# Feed it the first real upper/lower interval
fc.ret.upper.90[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.upper.90[1] ; fc.ret.lower.90[1] <- Tail.Cl[length(Tail.Cl)] * fc.ret.lower.90[1]
# Translation from Returns to Price, compounding the returns
fcp.upper.90 <- cumprod(fc.ret.upper.90); fcp.lower.90 <- cumprod(fc.ret.lower.90)
# Place NAs in prior slots to display the forecast correctly
fcp.upper.90 <- c(rep(NA,Vtf), fcp.upper.90); fcp.lower.90 <- c(rep(NA,Vtf), fcp.lower.90)

fcp <- cumprod(fc.ret)
fcp <- c(rep(NA,Vtf), fcp)
Tail.Cl.tail <- c(tail(Tail.Cl,Vtf),rep(NA,Vtf_d))
plot(Tail.Cl.tail, type="l",
     ylim=c( min(min(na.omit(Tail.Cl.tail),min(na.omit(fcp.lower.90)))) , max(max(na.omit((Tail.Cl.tail)),max(na.omit(fcp.upper.90))))),
     main = "Different Prediction Intervals",
     ylab = "Stock Price",
     xlab = "Trading Days since 22nd of October 2018")
legend(x = "bottomleft",  legend = c("Forecast","60%","70%","80%","90%"),col=c("purple","red","darkcyan","green","blue"), lty=2)
lines(fcp, col ="purple", lty = 2)
lines(fcp.upper.60, col ="red", lty = 2)
lines(fcp.lower.60, col ="red", lty = 2)
lines(fcp.upper.70, col ="darkcyan", lty = 2)
lines(fcp.lower.70, col ="darkcyan", lty = 2)
lines(fcp.upper.80, col ="green", lty = 2)
lines(fcp.lower.80, col ="green", lty = 2)
lines(fcp.upper.90, col ="blue", lty = 2)
lines(fcp.lower.90, col ="blue", lty = 2)

#### VISUALIZATION: OBJECTIVE FUNCTION ####
# Initialization
time <- 400 # The number of working/tradeable days since the start of the forecasts
pred <- 80 # The number of predictions taking place in the space of the 400 days 
fn <- 400/80 # The number of days to forecast ahead. 5 working days is good.
mod.obs <- 1000 # The number of days to each forecast will use
t_crit <- function(x){qt(x, Inf)} # The t distribution  values at Inf df

it <- 10 # function evaluations
t <- 0.498/it # spacing of evaluations between 0.501 and 0.999 (0.9 -> 80% within)

pun <- matrix(nrow = it, ncol = 3) # Punishment matrix. 
pun[,] <-0 # Fill it with 0s to avoid NA

# Evaluate:
for (h in 1:it){
  # Information on the intervals for our stored matrix
  pun[h,2] <- t_crit(0.501+t*(h-1))
  pun[h,3] <- 0.501+t*(h-1)
  for (i in 1:pred){
  # 1400 observations necessary so we have 1000 at all times 
  # (we pretend to be ignorant of the future, but we use the following week to evaluate!)
  # The following lines determine the beginnings and ends of the time periods evaluated
  start <- (fn*i) - (fn-1)
  end <- (mod.obs+time) - (time-fn*i)
  f.start <- (mod.obs) + fn*(i-1) + 1
  f.end <- (mod.obs) + fn*(i-1) + 5
  
  ret <- as.ts( tail( Rets, mod.obs+time ) )
    known.ret <- ret[start:end]
      future.ret <- ret[f.start:f.end]
  cl <- as.ts( tail( Close, mod.obs+time ) )
    known.cl <- cl[start:end]
      future.cl <- cl[f.start:f.end]
      
  # We model LGO.TO stock with an ARMA(0,1,0)-GARCH-(1,1) 
  # We assume student-t distribution 
  model <- ugarchfit(ugarchspecs, known.ret)
  # The forecast function allows us to retrieve a prediction and the volatility.
  # Note: When there is no AR/MA element; its a constant return
  fc <- ugarchforecast(model, n.ahead = fn)
  fc.sig <- fc@forecast$sigmaFor
  fc.ser <- fc@forecast$seriesFor
  # Different intervals to be calculated. t critical value range from 0.501 to 0.999
  upper <-fc.ser + t_crit(0.501+t*(h-1)) * (fc.sig)
  lower <-fc.ser - t_crit(0.501+t*(h-1)) * (fc.sig)
  # We translate back to prices
  # Remove the log in the forecast
  fc.ret <- exp(fc.ser) 
  # Feed it the first price (necessary or else it doesn't know where to start)
  fc.ret[1] <- known.cl[length(known.cl)] * fc.ret[1] 
  # Translation from Returns to Price
  fcp <- cumprod(fc.ret) 
  # Remove the log in the intervals
  fc.ret.upper <- exp(upper) ; fc.ret.lower <- exp(lower) 
  # Feed them the first real upper/lower interval
  fc.ret.upper[1] <- known.cl[length(known.cl)] * fc.ret.upper[1] ; fc.ret.lower[1] <- known.cl[length(Tail.Cl)] * fc.ret.lower[1]
  # Translation from Returns to Price, compounding the returns
  fcp.upper <- cumprod(fc.ret.upper); fcp.lower <- cumprod(fc.ret.lower)
  # Each future value will be compared to the forecast  
  for (j in 1:fn){
      if (future.cl[fn] > fcp.upper[fn]) {
        pun[h,1] <- pun[h,1] + 1
      }
      else if (future.cl[fn] < fcp.lower[fn]) {
        pun[h,1] <- pun[h,1] + 1
      }
      else if ((known.cl[mod.obs] < future.cl[fn] & future.cl[fn] < fcp.upper[fn])) {
        pun[h,1] <- pun[h,1] + (fcp.upper[fn] - future.cl[fn]) / (fcp.upper[fn] - known.cl[mod.obs]) * 0.25
      }
      else if ((known.cl[mod.obs] > future.cl[fn] & future.cl[fn] > fcp.lower[fn])) {
        pun[h,1] <- pun[h,1] + (future.cl[fn] - fcp.lower[fn]) / (known.cl[mod.obs] - fcp.lower[fn]) * 0.25
      }    
    }
  }
}

plot(y = pun[,1], x = pun[,3], type = "l",
     main = "Rough sketch of objective function (100 observations)",
     ylab = "Punishment Score",
     xlab = "Confidence Level")

#### OPTIMIZATION ####
# Initialization
time <- 400 # The number of working/tradeable days since the start of the forecasts
pred <- 80 # The number of predictions taking place in the space of the 400 days 
fn <- 400/80 # The number of days to forecast ahead. 5 working days is good.
mod.obs <- 1000 # The number of days to each forecast will use
t_crit <- function(x){qt(x, Inf)} # The t distribution  values at Inf df

xi <- 0.75 # initial solution
it <- 30 # function evaluations
ms <- 0.05 # maximum step size, neighborhood calibration
cool <- 1 # speed of cooling

# t <- 0.498/it # spacing of evaluations between 0.501 and 0.999 (0.9 -> 80% within)


pun <- matrix(nrow = it, ncol = 3) # Punishment matrix. 
pun[,] <-0 # Fill it with 0s to avoid NAs
pun[1,3] <- xi

# Evaluate:
for (h in 1:it){ # The first row of the matrix is reserved for initial solution
  if (h != 1){
  pun[h,3] <- runif(1,max(pun[h-1,3]-ms,0.501),min(pun[h-1,3]+ms,0.999)) # defines temptative interval that will compete with previous
  }
  T. <- h * cool# "Cooling" sequence
  # Information on the intervals for our stored matrix
  pun[h,2] <- t_crit(pun[h,3])
  for (i in 1:pred){
    # 1400 observations necessary so we have 1000 at all times 
    # (we pretend to be ignorant of the future, but we use the following week to evaluate!)
    # The following lines determine the beginnings and ends of the time periods evaluated
    start <- (fn*i) - (fn-1)
    end <- (mod.obs+time) - (time-fn*i)
    f.start <- (mod.obs) + fn*(i-1) + 1
    f.end <- (mod.obs) + fn*(i-1) + 5
    
    ret <- as.ts( tail( Rets, mod.obs+time ) )
    known.ret <- ret[start:end]
    future.ret <- ret[f.start:f.end]
    cl <- as.ts( tail( Close, mod.obs+time ) )
    known.cl <- cl[start:end]
    future.cl <- cl[f.start:f.end]
    
    # We model LGO.TO stock with an ARMA(0,1,0)-GARCH-(1,1) 
    # We assume student-t distribution 
    model <- ugarchfit(ugarchspecs, known.ret)
    # The forecast function allows us to retrieve a prediction and the volatility.
    # Note: When there is no AR/MA element; its a constant return
    fc <- ugarchforecast(model, n.ahead = fn)
    fc.sig <- fc@forecast$sigmaFor
    fc.ser <- fc@forecast$seriesFor
    # Different intervals to be calculated. t critical value range from 0.501 to 0.999
    upper <-fc.ser + pun[h,2] * (fc.sig)
    lower <-fc.ser - pun[h,2] * (fc.sig)
    # We translate back to prices
    # Remove the log in the forecast
    fc.ret <- exp(fc.ser) 
    # Feed it the first price (necessary or else it doesn't know where to start)
    fc.ret[1] <- known.cl[length(known.cl)] * fc.ret[1] 
    # Translation from Returns to Price
    fcp <- cumprod(fc.ret) 
    # Remove the log in the intervals
    fc.ret.upper <- exp(upper) ; fc.ret.lower <- exp(lower) 
    # Feed them the first real upper/lower interval
    fc.ret.upper[1] <- known.cl[length(known.cl)] * fc.ret.upper[1] ; fc.ret.lower[1] <- known.cl[length(Tail.Cl)] * fc.ret.lower[1]
    # Translation from Returns to Price, compounding the returns
    fcp.upper <- cumprod(fc.ret.upper); fcp.lower <- cumprod(fc.ret.lower)
    # Each future value will be compared to the forecast  
    for (j in 1:fn){
      if (future.cl[fn] > fcp.upper[fn]) {
        pun[h,1] <- pun[h,1] + 1
      }
      else if (future.cl[fn] < fcp.lower[fn]) {
        pun[h,1] <- pun[h,1] + 1
      }
      else if ((known.cl[mod.obs] < future.cl[fn] & future.cl[fn] < fcp.upper[fn])) {
        pun[h,1] <- pun[h,1] + (fcp.upper[fn] - future.cl[fn]) / (fcp.upper[fn] - known.cl[mod.obs]) * 0.25
      }
      else if ((known.cl[mod.obs] > future.cl[fn] & future.cl[fn] > fcp.lower[fn])) {
        pun[h,1] <- pun[h,1] + (future.cl[fn] - fcp.lower[fn]) / (known.cl[mod.obs] - fcp.lower[fn]) * 0.25
      }    
    }
  }
  delta_f <- pun[h,1] - pun[h-1,1]
  if (runif(1,0,1) < (1/(1+exp(delta_f/T.))) || h == 1){ # Because its a minimization problem; sign is + before delta_f
    pun[h,] <- pun[h,]
  }
  else {
  pun[h,] <- pun[h-1,]
  }
}        
  
pun # 1st column: solution; 2nd column: critical value; 3rd column: interval level      
    