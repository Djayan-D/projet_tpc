library(seasonal)
library(timeSeries)

seasX <- seas(ts_freq_mens_cinema_0020_corr)
cvs <- final(seasX)

# lissage
WH_add = HoltWinters(cvs) # partie constante, tendance et saisonnalité et choix additif ou multiplicatif
#WH_add = HoltWinters(cvs, seasonal = "mu") # il suppose un modèle add de base donc je change à la main
show(WH_add)
summary(WH_add)
plot(WH_add)
plot(WH_add$fitted[,1])

# Calcul de l'AIC
# Calcul des résidus
residuals <- residuals(WH_add)

# Calcul du MSE
mse <- mean(residuals^2)

# Estimation de la vraisemblance
n <- length(cvs)  # nombre d'observations
k <- length(coef(WH_add))  # nombre de paramètres
log_likelihood <- -(n/2) * log(2 * pi * mse) - (1/2) * sum(residuals^2 / mse)


aic_value <- 2 * k - 2 * log_likelihood
print(aic_value)

# Calcul de l'AICc
aicc_value <- aic_value + (2 * k * (k + 1)) / (n - k - 1)
print(aicc_value)
# Forecasting ----


## -------- sans forecast  -------- ##


# horizon h=50 - intervals 95%
# p = predict(WH_add, 50, prediction.interval = TRUE) 
# plot(WH_add, p)
# show(p)
 

## -------- avec forecast  -------- ##


# horizon h=50 - intervals 80% & 95%
library(forecast)
fit = forecast(WH_add, h=50)
plot(fit)
show(fit)

### --------- Commentaires ------- ###

## Smoothing parameter :
## paramètre de lissage pour le niveau
## alpha = 0.1571975 prévisio lisse : fort poids du passé récent
## paramètre de lissage pour la tendance
## beta = 0.01476799 prévision très souple : forte influence des observations les plus récentes
## paramètre de lissage pour la saisonnalité
## gamma = 0.2192295 prévisio lisse : fort poids du passé récent
##
## composante additive

















