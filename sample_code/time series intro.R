spots <- read.csv("Sunspots.csv")
spots_ts <- ts(spots$Monthly.Mean.Total.Sunspot.Number, frequency = 12, c(1749,1))

acf(spots_ts)
pacf(spots_ts)

pass <- read.csv("passenger.csv.csv")
pass_ts <- ts(pass$AirPassengers, frequency = 12, start = c(1949, 1), end = c(1960,12))

tsn_pass <- decompose(pass_ts, type = "multiplicative")
plot(tsn_pass)