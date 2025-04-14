fitAES <- auto.adam(ts_freq_mens_cinema_0020_corr, model="ZZZ", lags=c(1,1,12),
                    orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fitAES
summary(fitAES)
plot(fitAES)




plot(fitAES$states)
plot(fitAES$residuals)


prev_AES <- forecast(fitAES, h=12)
show(prev_AES)
plot(prev_AES)

### --------- Commentaires ------- ###

##La composante résiduelle du modèle ADAM ETS(M,A,N)+ARIMA(3,0,0) présente un
## fort comportement saisonnier
## Les courbes de la tendance et level sont complètement ..........
##
## On neutralise la composante saisonnière dans le modèle ETS pour estimer un
## modèle SARIMA12 :

fitadam3 <- auto.adam(ts_freq_mens_cinema_0020_corr, model="ZZN", lags=c(1,12), orders=list(ar=c(3,3), i=(2),
                                                                ma=c(3,3), select=TRUE))
fitadam3
summary(fitadam3)

par(mfcol=c(2,2))
plot(fitadam3)


plot(fitadam3$states)
plot(fitadam3$residuals)

### --------- Commentaires 2 ------- ###

## le modèle 2 est beaucoup mieux estimer graphiquement
##
## ETS(AAN) + SARIMA(2,0,2)[12]
## Error : A = composante additive
## Trend: A = composante additive
## Seasonal : N = pas de saisonnalité
## 
## Le modèle ETS(AAN) ne prend pas explicitement en compte la saisonnalité dans sa structure
## Cela signifie que la composante saisonnière n'est pas directement modélisée par le lissage exponentiel.
## Cependant, la combinaison avec le modèle SARIMA permet de capturer les effets saisonniers.
## L'ETS capture les composantes de niveau et de tendance,
## tandis que le SARIMA gère la saisonnalité et les dépendances temporelles
##
## AIC 7665.149     /  AICc 7676.042     /   BIC 7780.010     /   BICc  7809.861  

