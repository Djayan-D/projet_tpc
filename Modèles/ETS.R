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
##
## AIC 8339.824   /  AICc 8341.967   /   BIC 8392.034 