df <- read_excel("PREMIXED COCKTAILS.xlsx")

df_ts <- ts(df$Sales,start=2016, frequency=12) # ts function defines the dataset as timeseries starting Jan 2004 and having seasonality of frequency 12 (monthly) 

ggseasonplot(df_ts)
ggsubseriesplot(df_ts)

ggseasonplot(diff(df_ts))

ggseasonplot(log(df_ts))

plot(log(df_ts))


# Plot various decompositions into error/noise, trend and seasonality

fit <- decompose(df_ts, type="multiplicative") #decompose using "classical" method, multiplicative form
plot(fit)

fit <- decompose(df_ts, type="additive") #decompose using "classical" method, additive form
plot(fit)

fit <- stl(df_ts, t.window=12, s.window="periodic") #decompose using STL (Season and trend using Loess)
plot(fit)

plot(df_ts)


#######
#Benchmark method. 
# Let's use seasonal naive method as our benchmark method. 


fit <- snaive(diff(df_ts, h=12))
print(summary(fit))
checkresiduals(fit)
#Residual sd: 121225.7528
plot(fit, xlab="Month", ylab="Predicted Difference Sales")

fit1 <- snaive(df_ts, h=12)
print(summary(fit1))
checkresiduals(fit1)
#Residual sd: 102209.152 
plot(fit1, xlab="Month", ylab="Predicted Sales")

# Create exponential smoothing models: additive vs multiplicative noise (first A vs M), additive vs multiplicative trend (second A vs M) and no seasonality vs automatic detection (third N vs Z) trend and no seasonlity (AAN), multiplicative (MMN)
Sales_AAN <- ets(df_ts, model="AAN")
Sales_AAZ <- ets(df_ts, model="AAZ", damped=FALSE)
Sales_MMN <- ets(df_ts, model="MMN", damped=FALSE)
Sales_MMZ <- ets(df_ts, model="MMZ")

# Create their prediction "cones" for 12 months (1 years) into the future with quintile confidence intervals
Sales_AAN_pred <- forecast(Sales_AAN, h=12, level=c(0.8, 0.95))
Sales_AAZ_pred <- forecast(Sales_AAZ, h=12, level=c(0.8, 0.95))
Sales_MMN_pred <- forecast(Sales_MMN, h=12, level=c(0.8, 0.95))
Sales_MMZ_pred <- forecast(Sales_MMZ, h=12, level=c(0.8, 0.95))

# Compare the prediction "cones" visually
par(mfrow=c(1,4)) # This command sets the plot window to show 1 row of 4 plots
plot(Sales_AAN_pred, xlab="Month", ylab="Predicted Sales")
plot(Sales_AAZ_pred, xlab="Month", ylab="Predicted Sales")
plot(Sales_MMN_pred, xlab="Month", ylab="Predicted Sales")
plot(Sales_MMZ_pred, xlab="Month", ylab="Predicted Sales")

# Create a trigonometric box-cox autoregressive trend seasonality (TBATS) model
Sales_tbats <- tbats(df_ts)
Sales_tbats_pred <-forecast(Sales_tbats, h=12, level=c(0.8, 0.95))
par(mfrow=c(1,1))
plot(Sales_tbats_pred, xlab="Month", ylab="Predicted Sales")


par(mfrow=c(1,3)) # Lets look at the three models with seasonality on one graph on the same scale
plot(Sales_AAZ_pred, xlab="Year", ylab="Predicted Sales", ylim=c(0,2000000))
plot(Sales_MMZ_pred, xlab="Year", ylab="Predicted Sales", ylim=c(0,2000000))
plot(Sales_tbats_pred, xlab="Year", ylab="Predicted Sales", ylim=c(0,2000000))

par(mfrow=c(1,1)) # Lets look at them one-by-one
plot(Sales_AAZ_pred, xlab="Year", ylab="Predicted Sales")
plot(Sales_MMZ_pred, xlab="Year", ylab="Predicted Sales")
plot(Sales_tbats_pred, xlab="Year", ylab="Predicted Sales")


# Lets look at what our models actually are
Sales_AAZ # AIC: 976.6377
Sales_MMZ # AIC: 948.5316
Sales_tbats # AIC: 956.6868

# Select a model and export its predictions (mean, 80% and 95% confidence intervals) into a CSV file
write.csv(Sales_MMZ_pred, file = "Predicted Sales.csv") 

#ARIMA model could also be fitted and it's giving a better results. Need ARIMA with covariates (e.g., Fourier)
Sales_arima <- auto.arima(df_ts, seasonal=TRUE)
Sales_arima
Sales_arima_pred <-forecast(Sales_arima, h=12)
plot(Sales_arima_pred, ylab="Predicted Sales")
#AIC=592.33

# Select a model and export its predictions (mean, 80% and 95% confidence intervals) into a CSV file
write.csv(Sales_arima_pred, file = "Predicted Sales Arima.csv") 


# DIFFERENCING and ARIMA

par(mfrow=c(1,3))
View(df_ts)
plot(df_ts, xlab="Month",
     ylab="Sales")
plot(log(df_ts), xlab="Month",
     ylab="log sales")
plot(diff(log(df_ts),12), xlab="Month",
     ylab="Annual change in monthly log Sales")

fit <- stl(log(df_ts), t.window=12, s.window="periodic", robust=TRUE)
plot(fit)


# auto-correlation function
Acf(df_ts,main="") # data "as is"
Acf(log(df_ts),main="") # log-transformed data
Acf(diff(log(df_ts),12),main="") # difference-12 log data


# partial auto-correlation function
par(mfrow=c(1,2))
Acf(diff(log(df_ts),12),main="")
Pacf(diff(log(df_ts),12),main="") 

fit <- auto.arima(df_ts,seasonal=FALSE)
fit # AIC=959.47

fit <- auto.arima(df_ts,seasonal=TRUE)
fit # AIC=592.33

par(mfrow=c(1,1))
Acf(residuals(fit))
plot(forecast(fit,12)) #12 stands for 12 months = 1 years



