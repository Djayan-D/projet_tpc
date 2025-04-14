# Ajustement du modèle SARIMA
fit_sarima <- auto.arima(ts_freq_mens_cinema_0020_corr, seasonal = TRUE)

# Affichage du modèle
summary(fit_sarima)
plot(fit_sarima$residuals)

# Prévision sur 12 périodes
prev_SARIMA <- forecast(fit_sarima, h=12)

# Affichage des prévisions

plot(prev_SARIMA)