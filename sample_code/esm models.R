library(forecast)

# naive model
pass_naive <- naive(pass_ts)
plot(pass_naive)

# seasonal naive model
pass_snaive <- snaive(pass_ts, h = 12)
plot(pass_snaive)

# simple exponential smoothing
pass_ses <- ses(pass_ts)
plot(pass_ses)

# holt model
pass_holt <- holt(pass_ts)
plot(pass_holt)

# holt-winters
pass_hw <- hw(pass_ts, h = 12)
plot(pass_hw)

# example of MAPE calculation (for H-W)
mean(abs(pass_hw$residuals)/pass_ts)