library(Quandl)
library(forecast)
library(ggplot2)
library(tseries)
library(DescTools)

library(Metrics)
library(xts)
library(nnfor)
library(nnet)
library(staTools)

Quandl.api_key("<your key comes here>")

# Obtain Moody's Baa index data
baa <- Quandl("FED/RIMLPBAAR_N_M")

baa.bond <- as.xts(baa$Value, order.by=baa$Date)

plot(baa.bond)

adf.test(baa.bond)

baa.bond.delta <- na.omit(diff(baa.bond))

adf.test(baa.bond.delta)

# 1. ARIMA modelling

train <- baa.bond

start_time <- Sys.time()

model.arima <- auto.arima(train, max.p = 3, max.q = 3)

end_time <- Sys.time();

print("Fit ARIMA (time): ")
end_time - start_time

summary(model.arima)

plot(model.arima, col="red")
lines(model.arima$fitted, col="blue")

train <- baa.bond[index(baa.bond)[1:1000]]
test <- baa.bond[index(baa.bond)[1001:1172]]

fit.arima <- Arima(train, c(2,1,2))
fcst.arima <- forecast(fit.arima, h = length(test))

print("ARIMA:")
mean(fit.arima$residuals^2)
mean((fcst.arima$mean - as.data.frame(test)$V1)^2)/100

mean(abs(fit.arima$residuals))/100

sqrt(mean((fcst.arima$mean - as.data.frame(test)$V1)^2)/100)

mean(abs(fit.arima$residuals))/100

mean(abs((fcst.arima$mean - as.data.frame(test)$V1)))/100


# 2. MLP

train <- baa.bond

start_time <- Sys.time()

fit.mlp <- mlp(as.ts(baa.bond.delta), outplot=TRUE, sel.lag=TRUE, allow.det.season = TRUE, det.type = "auto")

end_time <- Sys.time()

print("Fit MLP (time): ")
end_time - start_time

df.train <- as.data.frame(train)
df.train$V1 <- df.train$V1/100

df.fitted.mlp <- as.data.frame(fit.mlp$fitted)

print("MLP:")
printf("  MSE:")
fit.mlp$MSE
sqrt(fit.mlp$MSE)
print("   ME:")
mean(df.fitted.mlp$x - df.train$V1[1:1168])
print("   MAE:")
mean(abs(df.fitted.mlp$x - df.train$V1[1:1168]))

print("   MPE:")
mean( (abs(df.fitted.mlp$x) - df.train$V1[1:1168])/df.train$V1[1:1168] )

print("   MAPE:")
mean( (abs(df.fitted.mlp$x - df.train$V1[1:1168]))/df.train$V1[1:1168] )

train <- baa.bond[index(baa.bond)[1:1000]]
test <- baa.bond[index(baa.bond)[1001:1172]]


fit.mlp <- mlp(as.ts(train), outplot=TRUE, sel.lag=TRUE, allow.det.season = TRUE, det.type = "auto")
fcst.mlp <- forecast(fit.mlp, h = length(test))

print("MLP (Test):")
mean(fit.mlp$residuals^2)
mean((fcst.arima$mean - as.data.frame(test)$V1)^2)/100

mean(abs(fit.arima$residuals))/100

sqrt(mean((fcst.arima$mean - as.data.frame(test)$V1)^2)/100)

mean(abs(fit.arima$residuals))/100

mean(abs((fcst.arima$mean - as.data.frame(test)$V1)))/100

# 3. ELM

train <- baa.bond

start_time <- Sys.time()

fit.elm <- elm(as.ts(train))

end_time <- Sys.time()

print("Fit ELM (time): ")
end_time - start_time

df.fitted.elm <- as.data.frame(fit.elm$fitted)

df.train <- as.data.frame(train)
df.train$V1 <- df.train$V1

print("ELM:")
printf("  MSE:")
fit.elm$MSE
sqrt(fit.elm$MSE)
print("   ME:")
mean(df.fitted.elm$x - df.train$V1[1:1168])
print("   MAE:")
mean(abs(df.fitted.elm$x - df.train$V1[1:1168]))

print("   MPE:")
mean( (df.fitted.elm$x - df.train$V1[1:1168])/df.train$V1[1:1168] )*100

print("   MAPE:")
mean( (abs(df.fitted.elm$x - df.train$V1[1:1168]))/df.train$V1[1:1168] )


train <- baa.bond[index(baa.bond)[1:1000]]
test <- baa.bond[index(baa.bond)[1001:1172]]

fit.elm <- elm(as.ts(train))
fcst.elm <- forecast(fit.elm, h = length(test))

print("ELM (Test):")
mean((fcst.elm$mean - as.data.frame(test)$V1))/100
mean((fcst.elm$mean - as.data.frame(test)$V1)^2)/100
sqrt(mean((fcst.elm$mean - as.data.frame(test)$V1)^2)/100)

mean(abs(fcst.elm$mean - as.data.frame(test)$V1))/100


#################



