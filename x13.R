# Méthode x13()----
library(seasonal)

# Estimation automatique
mod_x13 <- seas(ts_freq_mens_cinema_0020_corr)

# Résumé du modèle
summary(mod_x13)

# Composantes (tendance, saison, irrégulier)
plot(mod_x13)

# Extraire le modèle ARIMA utilisé
arima_model <- auto.arima(ts_freq_mens_cinema_0020_corr)

# Prévisions sur 12 mois
forecast_x13 <- forecast(arima_model, h = 12)

# Visualisation
autoplot(forecast_x13)

## --------- Autre méthode -------## 

# on continuera l'étude avec cette méthode là
# car permet prévision

library(RJDemetra)

myspec <- x13_spec("RSA5c")

mysax13 <- x13(ts_freq_mens_cinema_0020_corr, myspec)

summary(mysax13$regarima)

mysax13

plot(mysax13$final)



