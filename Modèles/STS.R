fitsts = StructTS(ts_freq_mens_cinema_0020_corr)
plot(cbind(fitted(fitsts), residuals(fitsts)))
show(fitsts)

summary(fitsts)
prevsts <- forecast(fitsts, 12) 
show(prevsts) # pas mettre en annexe
plot(prevsts) # en annexe


### --------- Commentaires ------- ###


## Variances:
##   level      slope       seas     epsilon
## 1.093e+12  2.087e+08  8.152e+11  1.803e+12
## 
## level = 1.093e+12 : Cette variance élevée signifie que les fluctuations autour du niveau moyen sont très importantes.
## slope = 2.087e+08 : Une variance plus faible suggère que la tendance est relativement stable.
## seas = 8.152e+11 : Une variance significative indique une forte composante saisonnière dans les données.
## epsilon = 1.803e+12 : Une variance élevée indique qu'il reste des variations non expliquées par le modèle.
##
## Conclusion:
## Les variances indiquent que la série est principalement influencée par des fluctuations
## autour du niveau moyen et des variations saisonnières. La pente
## contribue moins à la variabilité globale. La variance élevée des résidus suggère
## qu'il reste des variations non expliquées par le modèle

