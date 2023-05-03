library(forecast)

air_dec <- decompose(AirPassengers, type = 'multiplicative')

rand_air <- ts(air_dec$random[13:138], start = c(1949,1), frequency = 12)

acf(rand_air)
pacf(rand_air)

auto.arima(rand_air)