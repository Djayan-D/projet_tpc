# Smoothing parameters:
# alpha = 0.8561 
# beta  = 0.1282 
# gamma = 0.1439 
# phi   = 0.9403 # pour la courbure

## Approfondir cette section là sur la compréhension
# ETS ça va très vite (interprétation et codage)

# Uniquement pour prévisions (je crois c'est ce qu'il à dit)


fitets <- ets(ts_freq_mens_cinema_0020_corr, ic = "aic") # pour avoir le meilleur AIC
show(fitets)
plot(fitets)


prevets <- forecast(fitets,12)
show(prevets)
plot(prevets)


### --------- Commentaires ------- ###

## ETS(M,N,M)
## Error : M = composante multiplicative
## Trend: N = pas de tendance
## Seasonal : M = composante multiplicative
## 
## alpha : contrôle la sensibilité du modèle aux changements dans le niveau
## alpha = 0.0551 : Une valeur faible nous informe que le modèle
## est moins réactif aux changements récents dans le niveau.
## Cela signifie que le modèle donne plus de poids aux observations passées
## et est plus stable face aux fluctuations récentes.
##
## gamma : contrôle la sensibilité du modèle aux changements dans la composante saisonnière
## gamma = 1e-04 : Une valeur très faible indique que le modèle est
## très peu réactif aux changements dans la saisonnalité.
## Cela signifie que le modèle met beaucoup de temps à ajuster la composante saisonnière en réponse aux nouvelles données.

