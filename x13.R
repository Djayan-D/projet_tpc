# Méthode x13()----
library(seasonal)

# # Estimation automatique
# mod_x13 <- seas(ts_freq_mens_cinema_0020_corr)
# 
# # Résumé du modèle
# summary(mod_x13)
# 
# # Composantes (tendance, saison, irrégulier)
# plot(mod_x13)
# 
# # Extraire le modèle ARIMA utilisé
# arima_model <- auto.arima(ts_freq_mens_cinema_0020_corr)
# 
# # Prévisions sur 12 mois
# forecast_x13 <- forecast(arima_model, h = 12)
# 
# # Visualisation
# autoplot(forecast_x13)

## --------- Autre méthode -------## 

# dans cette méthode l'effet de calendrier n'est pas significatif alors que seas l'est

library(RJDemetra)

myspec <- x13_spec("RSA5c")

mysax13 <- x13(ts_freq_mens_cinema_0020_corr, myspec)

summary(mysax13$regarima)

mysax13

plot(mysax13$final)


### --------- Commentaires ------- ###

# Modèle RegArima(1,0,0)(0,1,1) avec transformation logarithmique 

## y = regression model + arima (1, 0, 0, 0, 1, 1)
## AR(1) : Composante autorégressive d'ordre 1.
## MA(1) : Composante de moyenne mobile d'ordre 1.
## Différenciation saisonnière d'ordre 1.
##
## Log-transformation: yes ==> décomposition multiplicative
## Pas de composante de jours ouvrables, d'années bissextile ni pâques ni d'outliers
## 
## Phi et BTheta significatif à 1%
## Phi est un coefficient qui correspond à un terme d'auto-régression dans le modèle ARIMA.
## Phi = -0.17669, cela signifie qu'il existe une corrélation négative entre la valeur actuelle et la précédente.
## BTheta est un coefficient associé au terme de moyenne mobile saisonnière
## BTheta = -0.70568, cela indique une forte relation négative avec les observations passées dans la saison précédente
##
## aic =  7317, aicc =  7317
























