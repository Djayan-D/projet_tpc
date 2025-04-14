fit_SSARIMA <- auto.ssarima(ts_freq_mens_cinema_0020_corr, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_SSARIMA

par(mfcol=c(2,2))
plot(fit_SSARIMA)

plot(fit_SSARIMA$residuals)

### --------- Commentaires ------- ###

## le modèle ne spécifiant pas de tendance et de saisonnalité
## nous allons faire quelques test pour vérifier qu'il n'y à pas de problème,
## c'est-à-dire d'oublie du modèle

Box.test(residuals(fit_SSARIMA), lag = 24, type = "Ljung-Box")


Acf(residuals(fit_SSARIMA), main = "ACF des résidus")
pacf(residuals(fit_SSARIMA), main = "PACF des résidus")

### --------- Commentaires 2 ------- ###

## test de Ljung-Box
## p-value = 0.4019 > 0.05 
## pas d’autocorrélation résiduelle significative → le modèle a bien capté la structure temporelle principale.
##
## ACF/PACF aucun signe d’autocorrélation persistante (pas de lag significatif, les résidus sont bruits blancs)
## pas de saisonnalité ou dépendance non captée.

prev_SSARIMA <- forecast(fit_SSARIMA, h=12)
prev_SSARIMA
plot(prev_SSARIMA)

## SARIMA(0,0,2)[1](3,0,0)[12]
## Composante non saisonnière avec 2 termes MA
## Composante saisonnière avec 3 termes AR sur une saisonnalité de 12
## 
## AIC 7707.376      /  AICc 7707.858      /   BIC 7731.740      /   BICc  7733.063   


## CHATGPT
## Ce modèle SARIMA indique que la série étudiée présente une forte composante saisonnière annuelle,
## bien captée par des effets autorégressifs à 12 mois. La dynamique à court terme est modélisée par
## deux composantes de moyenne mobile, tandis que l’absence de différenciation suggère une stationnarité de la série.

