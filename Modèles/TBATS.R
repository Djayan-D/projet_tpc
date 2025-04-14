fit_tbats <- tbats(ts_freq_mens_cinema_0020_corr)
show(fit_tbats)
plot(fit_tbats)

prev_TBATS <- forecast(fit_tbats, h=12)
plot_prev_TBATS <- plot(prev_TBATS)

### --------- Commentaires ------- ###

## AIC: 8372.645
##
## TBATS(0.187, {0,0}, -, {<12,5>})
##
## {0,0} : estimation d'un modèle Arma(0,0)
## - : pas d'estimation d'une tendance damped
##
## lambda de Box-Cox = 0.05227885
## transformation appliquée pour corriger la variance du modèle
##
## Ce modèle prend en compte une saisonnalité mais pas de tendance.
