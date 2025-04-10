decomp = stl(ts_freq_mens_cinema_0020_corr, s.window="periodic")
# show(decomp)
plot(decomp)

fitstl = stlm(ts_freq_mens_cinema_0020_corr)

prevstl <- forecast(fitstl,12) #période d'une année

# show(prevsts) # pas mettre en annexe

plot(prevstl) # en annexe

summary(prevstl)

### --------- Commentaires ------- ###

## Structure du modèle ETS(A,N,N):
## A : La composante niveau est modélisée de manière additive
## N : Pas de tendance
## N : Pas de saisonnalité dans ETS car captée séparément par STL
##
## AIC 8247.401  /  AICc 8247.503  /   BIC 8257.843 