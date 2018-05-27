rm(list=ls())

## Required Librabries 
library(dynlm)
library(ggplot2)
library(AER)
library(Hmisc)
library(forecast)
library(dLagM)
library(TSA)
library(car)
library(expsmooth)
library(urca)
library(x12)
library(tseries)

## Mase Funtion 
MASE.dynlm <- function(model, ... ){
  
  options(warn=-1)
  
  if(!missing(...)) {# Several models
    models = list(model, ...)
    m = length(models)
    for (j in 1:m){
      if ((class(models[[j]])[1] == "polyDlm") | (class(models[[j]])[1] == "dlm") | (class(models[[j]])[1] == "koyckDlm") | (class(models[[j]])[1] == "ardlDlm")){
        Y.t = models[[j]]$model$model$y.t
        fitted = models[[j]]$model$fitted.values
      } else if (class(models[[j]])[1] == "lm"){
        Y.t = models[[j]]$model[,1]
        fitted = models[[j]]$fitted.values
      } else if (class(models[[j]])[1] == "dynlm"){
        Y.t = models[[j]]$model$Y.t
        fitted = models[[j]]$fitted.values  
      } else {
        stop("MASE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send a bunch of these objects to the function.")
      }
      # Y.t = models[[j]]$model$y.t
      # fitted = models[[j]]$fitted.values
      n = length(fitted)
      e.t = Y.t - fitted
      sum = 0 
      for (i in 2:n){
        sum = sum + abs(Y.t[i] - Y.t[i-1] )
      }
      q.t = e.t / (sum/(n-1))
      if (j == 1){
        MASE = data.frame( n = n , MASE = mean(abs(q.t)))
        colnames(MASE) = c("n" , "MASE")
      } else {
        MASE = rbind(MASE, c(n , mean(abs(q.t))))
      }
    }
    Call <- match.call()
    row.names(MASE) = as.character(Call[-1L])
    MASE
  } else { # Only one model
    if ((class(model)[1] == "polyDlm") | (class(model)[1] == "dlm") | (class(model)[1] == "koyckDlm") | (class(model)[1] == "ardlDlm")){
      Y.t = model$model$model$y.t
      fitted = model$model$fitted.values
    } else if (class(model)[1] == "lm"){
      Y.t = model$model[,1]
      fitted = model$fitted.values
    } else if (class(model)[1] == "dynlm"){
      Y.t = model$model$Y.t
      fitted = model$fitted.values  
    } else {
      stop("MASE function works for lm, dlm, polyDlm, koyckDlm, and ardlDlm objects. Please make sure that you are sending model object directly or send one of these objects to the function.")
    }
    n = length(fitted)
    e.t = Y.t - fitted
    sum = 0 
    for (i in 2:n){
      sum = sum + abs(Y.t[i] - Y.t[i-1] )
    }
    q.t = e.t / (sum/(n-1))
    MASE = data.frame( MASE = mean(abs(q.t)))
    colnames(MASE) = c("MASE")
    Call <- match.call()
    row.names(MASE) = as.character(Call[-1L])
    MASE
  }
  
}

setwd("C:/Users/ndnad/Desktop/Forecasting/Project/Regular")
## ################################################################
##################################################################

## Reading  dataset 
data <- read.csv("DJI.csv", header = TRUE)
data1 <- data[, c("DJI", "Oil")]
data.ts <- ts(data1, start = c(1987,9), frequency = 12)
#View(data.ts)
class(data.ts)

### -------Non Emperical Analysis-----------------------------------------
## Time Sereis Plot 
plot(data.ts, yax.flip=T,  main = "Monthly DJI Index and Oil Price " )
cor(data.ts) # Moderately positive correlation

## Plotting both series tigether in the same plot
data.ts.scale= scale(data.ts)
plot(data.ts.scale, plot.type="s",col = c("black", "red"), main = "DJI and Oil Price Series", ylab = "Value")
legend("topleft",lty=1, text.width = , col=c("black","red"), c("DJI", "Oil Price"))

## DJI Series 
DJI.ts <- data.ts[,1]
oil.ts <- data.ts[,2]
plot(DJI.ts, main = "Time Series Plot of DJI Series", ylab ="Monthly DJI Value" )
# points(y=DJI.ts,x=time(DJI.ts), pch=as.vector(season(DJI.ts))) ## This will be more messy
par(mfrow=c(1,2)) # Put the ACF and PACF plots next to each other
acf(DJI.ts, main = "DJI Value Series")
pacf(DJI.ts, main = "DJI Value Series")

## Explanession of the DJI series 
# Multiple trend exist in the series, no any efffect of seasonlaity, around 2009-2010 there was 
# intervention effect. The followin incresing and decreasing points in the series suggest the auto-
# regressive behavior in the series. Fluctuation around mean level suggest the moving averge behavior
# Change of the variation is obvious through out the series. 

## Since there is no any repeating patterns in the ACF while slowly decaying patterns of highly 
# significant lag suggest the absesence of seasonal effect and presence of trend accordingly. 

## Oil Series 
oil.ts <- data.ts[,2]
plot(oil.ts, main = "Time Series Plot of Oil Price Series", ylab ="Monthly Oil Price" )
# points(y=oil.ts,x=time(oil.ts), pch=as.vector(season(oil.ts))) ## This will be more messy
par(mfrow=c(1,2)) # Put the ACF and PACF plots next to each other
acf(oil.ts, main = "Oil Price Series")
pacf(oil.ts, main = "Oil Price Series")

## Explanession of the Oil Price series 
# Multiple trend exist in the series, no any efffect of seasonlaity, in 2010 and 2016 there was 
# huge intervention effect. The followin incresing and decreasing points in the series suggest the auto-
# regressive behavior in the series. Fluctuation around mean level suggest the moving averge behavior
# Change of the variation is obvious through out the series. 

## Since there is no any repeating patterns in the ACF while slowly decaying patterns of highly 
# significant lag suggest the absesence of seasonal effect and presence of trend accordingly. 


# ----------- Modelling the Series ---------------------------------------------
# partition of data set for Cross validation procedure  
## We have partition the dataset to ensure the last 2 years of data is use for testing purpose
train <- data1[1:334,]   # choose the first 333 row as training set
test  <- data1[335:358,] # choose the following 24 row as testing set

DJI.train <- ts(train[,1], start=c(1987,9), frequency = 12)
DJI.test <- ts(test[,1], start=c(2015,7), frequency = 12)

oil.train <- ts(train[,2], start=c(1987,9), frequency = 12)
oil.test <- ts(test[,2], start=c(2015,6), frequency = 12)

#####################################################################################
## ----Regression Model (Distribute Lag Model)----dLagM Package

#Dlm model
dlm.1 = dlm(x = as.vector(oil.train ) , y = as.vector(DJI.train), q = 4, show.summary = TRUE)
vif(dlm.1$model)
checkresiduals(dlm.1$model)
bgtest(dlm.1$model)

# plyDlm model
dlm.2 = polyDlm(x = as.vector(oil.train ) , y = as.vector(DJI.train) , q = 4 , k = 2, show.beta = TRUE , show.summary = TRUE)
vif(dlm.2$model)
checkresiduals(dlm.2$model)
bgtest(dlm.2$model)

#KoyckDlm model
dlm.3 = koyckDlm(x = as.vector(oil.train ) , y = as.vector(DJI.train) , show.summary = TRUE)
vif(dlm.3$model)
checkresiduals(dlm.3$model)
bgtest(dlm.3$model)

## ArdDlm mdoel 
dlm.4.1 = ardlDlm(x = as.vector(oil.train) , y = as.vector(DJI.train) , p = 1 , q = 1 , show.summary = TRUE)
checkresiduals(dlm.4.1$model)
bgtest(dlm.4.1$model)
# BG test: The Breusch-Godfrey test for higher-order serial correlation. The null hypothesis (H0) of the test is 
# that there is no serial correlation in the residuals up to the specified order. If type is 
# set to "F", the test statistics follows the F distribution. 

dlm.4.1.forecasts = ardlDlmForecast(model = dlm.4.1 ,  x = oil.test, h = 24)$forecasts

dlm.4.2 = ardlDlm(x = as.vector(oil.train ) , y = as.vector(DJI.train) , p = 2 , q = 2 , show.summary = TRUE)
checkresiduals(dlm.4.2$model)
bgtest(dlm.4.2$model)
dlm.4.2.forecasts = ardlDlmForecast(model = dlm.4.2 ,  x = oil.test, h = 24)$forecasts

dlm.4.3 = ardlDlm(x = as.vector(oil.train ) , y = as.vector(DJI.train) , p = 3 , q = 3 , show.summary = TRUE)
checkresiduals(dlm.4.3$model)
bgtest(dlm.4.3$model)
dlm.4.3.forecasts = ardlDlmForecast(model = dlm.4.3 ,  x = oil.test, h = 24)$forecasts

## This forecast is just for experiment 
# plot(DJI.train, type="l", xlim = range(1987, 2018), ylim = range(DJI.train),  ylab = "Monthly DDI", xlab = "Year", 
#      main="Salar Radiation Forecast for Precipitaitn") 
# lines(ts(dlm.4.3.forecasts, end=c(2017, 6), frequency = 12),col="Red",type="l")

### Comparision of Dlm models using aic, bic and mase 
aic.models = AIC(dlm.1$model, dlm.2$model, dlm.4.1$model, dlm.4.2$model, dlm.4.3$model)
sortScore(aic.models, score= "aic")
bic.models = BIC(dlm.1$model, dlm.2$model, dlm.4.1$model, dlm.4.2$model, dlm.4.3$model)
sortScore(bic.models, score= "bic")
mase <- MASE(dlm.1, dlm.2, dlm.3, dlm.4.1, dlm.4.2, dlm.4.3)
sortScore(mase, score = "mase")

# It is found that the dlm.3 (KoyckDLM) model is better model according mase value 
#### Residual of this model is quite impressive 
dlm.3.forecasts = koyckDlmForecast(model = dlm.3 , x = oil.test, h = 24)$forecasts
dlm.forecast <- ts(dlm.3.forecasts, end=c(2017, 6), frequency = 12)

plot(DJI.train, type="l", xlim = range(1987, 2018), ylim = c(min(DJI.train),max(DJI.train)+3000), 
     ylab = "Salar Radiation", xlab = "Year", 
     main="Salar Radiation Forecast for Precipitaitn")                       
lines(dlm.forecast,col="red",type="l")
legend("topleft",lty=1, pch = 1, text.width = , col=c("black","red"), c("Original", "Forecast"))

###################################################################
## -------Dynamic Linear Model (dynlm package)----------------
## this is the model for intervention analsis using regression approach
Y.t = DJI.train
which(Y.t < -100)
## we god integer == 0
Y.t.1 =  Lag(Y.t,+1)
X.t = oil.train
#which(X.t < -100)
X.t.1 = Lag(X.t,+1)

dynlm.1 = dynlm(Y.t ~ X.t + L(X.t, k=1) + L(Y.t))
summary(dynlm.1)
checkresiduals(dynlm.1)

dynlm.2 = dynlm(Y.t ~ X.t + X.t.1 + L(Y.t) + trend(Y.t))
summary(dynlm.2)
checkresiduals(dynlm.2)

dynlm.3 = dynlm(Y.t ~ X.t + X.t.1 + L(Y.t) + trend(Y.t)+ trend(X.t))
summary(dynlm.3)
checkresiduals(dynlm.3)

dynlm.4 = dynlm(Y.t ~ X.t + X.t.1 + L(X.t , k = 2) + L(X.t, k=2) + L(Y.t , k = 1) + trend(X.t))
summary(dynlm.4)
checkresiduals(dynlm.4)
bgtest(dynlm.4)

## comaparing dynlm models 
bic.models = BIC(dynlm.1, dynlm.2, dynlm.3, dynlm.4)
aic.models =AIC(dynlm.1, dynlm.2, dynlm.3, dynlm.4)
mase_d <- MASE.dynlm(dynlm.1, dynlm.2, dynlm.3, dynlm.4)

sortScore(bic.models, score = "bic")
sortScore(aic.models,score = "aic")
sortScore(mase_d, score = "mase")

## Accoring to the diagonestic check of residulas, AIC, BIc and MASE values
## We have found that that the dynlm.4 model is the best 

## 
par(mfrow=c(1,1))
plot(Y.t, ylab='Monthly DJI',xlab='Year',main = "Time Series Plot of Monthly 
     DJI Series")
lines(dynlm.4$fitted.values,col="red")
legend("topleft",lty=1, pch = 1, col=c("black","red"),c("Original","Fitted"))

####-------Dynlm forecast-------------------
q = 24
n = nrow(dynlm.4$model)
dji.frc = array(NA , (n + q))
dji.frc2 = array(NA , (n + q))
dji.frc[1:n] = X.t[4:length(X.t)]
dji.frc2[1:n] = Y.t[4:length(Y.t)]
trend = array(NA,q)
trend.start = dynlm.4$model[n,"trend(X.t)"]
trend = seq(trend.start , trend.start + q/12, 1/12)

for (i in 1:q){
  dji.frc[n+i] = oil.test[i]
  data.new = c(1,dji.frc[n+i],dji.frc[n-1+i],dji.frc[n-2+i],dji.frc2[n-1+i],trend[i])
  # data.new = c(1, predictor[i], predictor[i-1], predictor[i-2], dji.frc[n-1+i],dji.frc[n-2+i],trend[i],months)
  dji.frc2[n+i] = as.vector(dynlm.4$coefficients) %*% data.new
}

fit.dynlm <- ts(dji.frc2[(n+1):(n+q)],start=c(2015,7),frequency = 12)

plot(Y.t, xlim=c(1987,2018), ylim = c(min(DJI.train),max(DJI.train)+3000),
     ylab='DJI ',xlab='Year',main = "Time Series Plot of Monthly DIJ Series")
lines(fit.dynlm,col="red",type="l")


## think that the dynlm model over fit the series, so it gives the bad forecast. which we can observed
# from the r-square value. 


###################################################################################
## ---Exponential smotheing method-----------------------------
fit.ses = ses(DJI.train, initial="simple", h=24) 
summary(fit.ses) ## Mase == 0.2401
checkresiduals(fit.ses)

fit.hw = hw(DJI.train, trend = "multipicative", damped = FALSE, h=24) 
summary(fit.hw) ## Mase == 0.2373
checkresiduals(fit.hw)

fit.holt = holt(DJI.train, initial="optimal", exponential=FALSE, h=24) 
summary(fit.holt) ## Mase == 0.2382
checkresiduals(fit.holt)

# when comparing the value of mase and, resiudal analysis we have observed fit.hw model 
# best caputure the series. The following code demonstrates the forecast  

## forecast of  exponentila smoothing series
plot(fit.hw, ylab="Monthly DJI", main = "Forecast from Simple Exponential Method",
     plot.conf=FALSE, type="l", fcol="blue", xlab="Year")
legend("topleft",lty=1, pch = 1, col=c("black","blue"),c("original","forecast"))

##############################################################################
## State space model 
fit.ets.1 = ets(DJI.train, model="MAN", damped = TRUE)
summary(fit.ets.1) ## AIc == 5782.371, BIC == 5805.219, MASE == 0.239
checkresiduals(fit.ets.1)

fit.ets.2 = ets(DJI.train, model="MMN")
summary(fit.ets.2) ## AIc == 5772.443, BIC == 5791.484, MASE == 0.233
checkresiduals(fit.ets.2)

fit.ets.3 = ets(DJI.train, model="ZZZ", opt.crit = "mse")
summary(fit.ets.3) ## AIc == 5770.892, BIC == 5782.316.546, MASE == 0.240
checkresiduals(fit.ets.3) ## this is the best method 

## Accoriding to mase and residual analysis the fit.ets.2 model best capture the structure
# the series the following chunks of code best describe the sereis 

## Prediction from space state model 
plot(forecast(fit.ets.2), ylab="Monthly DJI ", plot.conf=FALSE, 
     main="Monthly DJI  Forecast by Space State Model", type="l", xlab="Time")
legend("topleft",lty=1, pch = 1, col=c("black","blue"),c("original","forecast"))

#################################################################################
#################################################################################
## Final model evaluation using cross validation prcedure of ts plot of original
# predicted series
prediction= ts.intersect(DJI.test, fit.hw[]$mean, forecast(fit.ets.2, h=24)[]$mean, 
                           dlm.forecast)
prediction.scale= scale(prediction)
plot(prediction.scale, plot.type="s",col = c("black", "red", "blue", "pink"), 
               main = "Original verses Predicted DJI series", ylab = "Value")
legend("topleft",lty=1, text.width = , col=c("black","red", "blue", "pink"), 
             c("Original", "fit.hw", "fit.ets", "fit.dlm"))
# # predicted series
#   prediction= ts.intersect(DJI.test, fit.hw[]$mean, forecast(fit.ets.2, h=24)[]$mean, dlm.forecast, fit.dynlm)
#   prediction.scale= scale(prediction)
#   plot(prediction.scale, plot.type="s",col = c("black", "red", "blue", "pink", "gray"),
#            main = "Original verses Predicted DJI series", ylab = "Value")
#  legend("topleft",lty=1, text.width = , col=c("black","red", "blue", "pink", "gray"),
#                         c("Original", "fit.hw", "fit.ets", "fit.dlm", "fit.dynlm"))

## Cross validation of forecast line in original line 
plot(DJI.ts, ylab="Monthly DJI", plot.conf=FALSE, type="l", xlab="Year", 
                       main = "Model Comparision using Time seris Plot")
lines(DJI.test, col = "red")
lines(fit.hw[]$mean, col = "green")
lines(forecast(fit.ets.2, h =24)[]$mean, col = "blue", type = "l")
lines(dlm.forecast, col = "pink", type = "l")
# lines(fit.dynlm, col = "gray", type = "l")
legend("topleft",lty=1, text.width = , col=c("black","red", "green","blue", "pink"), c("Original","test", "fit.hw", "fit.ets", "fit.dlm"))

### Plot of test versus forecast confidence interval of ets model 
plot(DJI.ts, ylab="Monthly DJI", plot.conf=FALSE, type="l", xlab="Year", 
     main = "Test and forecast confidence interval of ets model")
lines(DJI.test, col = "red")
lines(forecast(fit.ets.2, h =24)[]$mean, col = "blue", type = "l")
lines(forecast(fit.ets.2, h =24)[]$lower[,2] , col = "green", type = "l")
lines(forecast(fit.ets.2, h =24)[]$upper[,2], col = "green", type = "l")
legend("topleft",lty=1, text.width = , col=c("black","red", "blue", "green"), c("original","test", "fit.ets", "conf.interval"))
# This confidence interval of the forcasted values of state space moel well confined the 
# test series. Further, the mean of the forecasted series is pretty looks liek the trend 
# line of the test series. Thus we are conformed that the state space model is the best model
# according to the forecast accuaracy. 

##################################################################################
##################################################################################
###################################################################################
# -- Two years ahead forecast using whole series ---------------------------

# While performing the diagnestic check of model and cross validation over the 
# training and test set of the  series we have found that the space state model
# resonably fit well the series 

## buildng model using whole series and  two years ahead forecast 
best.fit = ets(DJI.ts, model="MMN")
summary(best.fit) ## AIc == 6266.614, BIC == 6286.017, MASE == 0.2372
checkresiduals(best.fit)

plot(forecast(best.fit, h = 24), ylab="Monthly DJI Value ", plot.conf=FALSE, 
     main="Two years DJI  Forecast by Space State Model", type="l", xlab="Time")
legend("topleft",lty=1, pch = 1, col=c("black","blue"),c("original","forecast"))

####################################################################################

