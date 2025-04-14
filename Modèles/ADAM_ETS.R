library(smooth)


fit_ADAM_ETS <- auto.adam(ts_freq_mens_cinema_0020_corr, model = "ZZZ", lags = c(1, 12), select = TRUE)
# ZZZ car je ne spécifie rien (tendance, saisonnalité, erreur)
fit_ADAM_ETS
summary(fit_ADAM_ETS)

par(mfcol=c(2,2))
plot(fit_ADAM_ETS)

par(mfcol=c(1,1))


plot(fit_ADAM_ETS$states)
plot(fit_ADAM_ETS$residuals)


prev_ADAM_ETS <- forecast(fit_ADAM_ETS, h=12)
show(prev_ADAM_ETS)
plot(prev_ADAM_ETS)

### --------- Commentaires ------- ###

## ETS(MNA)
## Error : M = composante multiplicative
## Trend: N = pas de tendance
## Seasonal : A = composante additive
##
## On observe graphiquement que les résidus ne comporte pas de tendance évidente,
## qui aurait pu ne pas être prise en compte par le modèle
##
## gamma : contrôle la sensibilité du modèle aux changements dans la composante saisonnière
## gamma = 0.4260 : un bon équilibre, le modèle n'est pas trop rigide pas trop flexible face aux variations saisonnières
##
## AIC 7688.839    /  AICc 7691.278    /   BIC 7744.529    /   BICc  7751.214 







