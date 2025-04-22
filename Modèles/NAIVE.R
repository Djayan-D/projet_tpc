library(forecast)

pred_naive <- naive(ts_freq_mens_cinema_0020_corr, h=12)
show(pred_naive)
plot(pred_naive)