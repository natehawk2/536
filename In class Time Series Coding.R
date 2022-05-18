climate <- read.table("climate.txt", header = TRUE)

library(astsa)


library(ggplot2)
ggplot(data=climate, mapping=aes(x = Time, y = AnnAnom)) + geom_line() + 
  labs(x = "Year", 
       title = "AnnAnom over time")

my.ACF <- acf(climate$AnnAnom, lag.max=50)

ACF.dframe <- data.frame(Lag=my.ACF$lag, ACF=my.ACF$acf)
ggplot(data=ACF.dframe, aes(x=Lag, y=ACF)) + geom_col()

colnames(climate)
climate <- climate[,c(3,4)]
plot.ts(x = climate$Time, y = climate$AnnAnom)

acf(climate$AnnAnom)
pacf(climate$AnnAnom)

acf2(climate$AnnAnom)

library(forecast)

auto.arima(climate$AnnAnom)

model2 <- sarima(climate$AnnAnom, p = 5, d=1, q = 1, S = 12, P = 3, Q = 1, D = 1)





library(forecast)
# Create a time-series object
my.ts <- ts(data=climate$AnnAnom, start=c(1850,6), frequency=12)
head(climate)
```


# 4


X <- model.matrix(AnnAnom ~ -1 + Time + pmax(Time-1975, 0), data=climate)

X1 <- model.matrix(AnnAnom ~ -1 + Time + pmax(Time-1975, 0) + pmax(Time - 1940, 0) + pmax(Time - 1920, 0), data=climate)




# 5) Find pdqs


arima.output1 = auto.arima(my.ts, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.d = 1, max.D = 1, ic="aic", stepwise=FALSE, xreg=X1)

arima.output = auto.arima(my.ts, max.p = 2, max.q = 2, max.P = 2, max.Q = 2, max.d = 1, max.D = 1, ic="aic", stepwise=FALSE, xreg=X)



# Fit model

my.sarima.model <- Arima(my.ts, order=c(2,0,1), seasonal=c(2, 0, 0), xreg=X1)
summary(my.sarima.model)


monthly.increase <- my.sarima.model$coef[7] + my.sarima.model$coef[8] + my.sarima.model$coef[9] + my.sarima.model$coef[10] 
monthly.increase

40*monthly.increase


forecast(my.sarima.model, )
