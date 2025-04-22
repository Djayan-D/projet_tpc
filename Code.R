#---------- 1. CHARGER LES PACKAGES ----------

library(readxl)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(EnvStats)
library(moments)
library(tibble)
library(seasonal)
library(RJDemetra)
library(forecast)
library(tsoutliers)
library(smooth)
library(gridExtra)





#---------- 2. IMPORTER LES DONNÉES ----------

#----- 2.1. Charger les données -----

## https://www.cnc.fr/documents/36995/158946/fr%C3%A9quentation+et+films+dans+les+salles+de+cin%C3%A9ma.xlsx/df2fce1f-52e3-08f4-a8a1-172ee8fd8203

freq_mens_cinema <- read_excel("data/freq_mens_cinema.xlsx")



#----- 2.2. Réorganiser la base -----

#--- 2.2.1. Renommer la première colonne (année) ---

names(freq_mens_cinema)[1] <- "Annee"



#--- 2.2.2. Retirer la colonne total ---

freq_mens_cinema <- freq_mens_cinema |> 
  select(-Total) |> # retirer la colonne "Total"
  round() # arrondir pour tirer les virgules



#--- 2.2.3. Extraire la période 2000-2020 ---

freq_mens_cinema_0020 <- freq_mens_cinema |> 
  filter(Annee >= 2000 & Annee < 2020)




#--- 2.2.4. Convertir les données en format long ---

freq_mens_cinema_long <- freq_mens_cinema |> 
  pivot_longer(cols = -Annee, names_to = "Mois", values_to = "Valeur")

freq_mens_cinema_0020_long <- freq_mens_cinema_0020 |> 
  pivot_longer(cols = -Annee, names_to = "Mois", values_to = "Valeur")



#--- 2.2.5. Créer une colonne Date ---

freq_mens_cinema_long$Date <- as.yearmon(paste(freq_mens_cinema_long$Annee, freq_mens_cinema_long$Mois), 
                                         format = "%Y %B")

freq_mens_cinema_0020_long$Date <- as.yearmon(paste(freq_mens_cinema_0020_long$Annee, freq_mens_cinema_0020_long$Mois),
                                              format = "%Y %B")



#---------- 3. ANALYSE DES DONNÉES ----------

#----- 3.1. Vérifier les valeurs atypiques -----

#--- 3.1.1. Convertir en TS ---

# 1980 - 2024

ts_freq_mens_cinema <- ts(freq_mens_cinema_long$Valeur, 
                          start = c(1980, 1), 
                          frequency = 12)


# 2000 - 2020

ts_freq_mens_cinema_0020 <- ts(freq_mens_cinema_0020_long$Valeur, 
                               start = c(2000, 1), 
                               frequency = 12)


#--- 3.1.2. Identifier les valeurs atypiques ---

# 1980 - 2024

outliers_8024 <- tso(ts_freq_mens_cinema) 
print(outliers_8024)

plot(outliers_8024)
show(outliers_8024)

# Pour justifier pourquoi on coupe


# 2000 - 2020

outliers_0020 <- tso(ts_freq_mens_cinema_0020) 
print(outliers_0020)

plot(outliers_0020)
show(outliers_0020)

window(ts_freq_mens_cinema_0020, start = c(2008, 3), end = c(2008, 3))

# Mars 2008 (27 056 406 entrées)
#
# En mars 2008, un outlier additive (AO) a été détecté dans la fréquentation 
# mensuelle des cinémas, avec une anomalie estimée à 9,8 millions d’entrées 
# supplémentaires.
# Un AO (outlier additif) est une valeur aberrante isolée qui perturbe 
# temporairement une série temporelle, sans affecter les périodes suivantes. 
# Il s'agit d'une fluctuation ponctuelle et inhabituelle qui ne modifie pas la 
# tendance globale.
# Dans ce cas précis, l’AO de mars 2008 est directement lié au phénomène 
# "Bienvenue chez les Ch’tis", sorti le 27 février 2008. Ce film de Dany Boon 
# a provoqué un afflux exceptionnel de spectateurs, générant une hausse soudaine 
# des entrées en salle. Cet événement a marqué un record historique du box-office 
# français, qui ne sera dépassé qu’en 2011 avec la sortie du film "Intouchables".



#--- 3.1.3. Traiter les valeurs atypiques ---

# 1980 - 2024

ts_freq_mens_cinema_corr <- outliers_8024$yadj


# 2000 - 2020

ts_freq_mens_cinema_0020_corr <- outliers_0020$yadj



#--- 3.1.4. Visualiser la TS corrigée ---

# 1980 - 2024

plot(ts_freq_mens_cinema_corr / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 1980-2024")


# 2000 - 2020

plot(ts_freq_mens_cinema_0020_corr / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 2000-2020")



#----- 3.2. Statistiques descriptives -----

#--- 3.2.1. Créer une fonction ---

stats_desc <- function(data) {
  # Calcul des statistiques de base
  moyenne <- mean(data, na.rm = TRUE)
  ecart_type <- sd(data, na.rm = TRUE)
  asymetrie <- skewness(data, na.rm = TRUE)
  aplatissement <- kurtosis(data, na.rm = TRUE)
  shapiro_test <- shapiro.test(data)$p.value  # Test de normalité (Shapiro-Wilk)
  
  # Calcul des quantiles
  quantiles <- quantile(data, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  
  # Création d'un tableau de résultats
  resultats <- tibble(
    Statistique = c("Moyenne", "Écart-type", "Skewness", "Kurtosis", 
                    "p-value Shapiro-Wilk", "Min", "1er Quartile (Q1)", 
                    "Médiane (Q2)", "3e Quartile (Q3)", "Max"),
    Valeur = c(moyenne, ecart_type, asymetrie, aplatissement, shapiro_test, 
               quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5])
  )
  
  return(resultats)
}



#--- 3.2.2. Appliquer à la série ---

# 1980 - 2024

ts_freq_mens_cinema_corr |> 
  as.numeric() |> 
  stats_desc()


# 2000 - 2020

ts_freq_mens_cinema_0020_corr |> 
  as.numeric() |> 
  stats_desc()



#--- 3.2.3. Boxplot ---

# 1980 - 2024

ts_freq_mens_cinema_corr |> 
  as.numeric() |> 
  boxplot()


# 2000 - 2020

ts_freq_mens_cinema_0020_corr |> 
  as.numeric() |> 
  boxplot()

# Juste dire que la série est plutôt propre



#----- 3.3. Détecter la saisonnalité -----

#--- 3.3.1. Graphiques ---

ts_freq_mens_cinema_0020_corr |> 
  decompose(, type = "additive") |> 
  plot()

ts_freq_mens_cinema_0020_corr |> 
  decompose(, type = "multiplicative") |> 
  plot()


# Test

summary(regarima_x13(ts_freq_mens_cinema_0020_corr, spec ="RG5c"))

## Multiplicatif est plus adapté





#---------- 4. DÉSAISONNALISATION ET DÉCOMPOSITION ----------

# Appliquer X13-ARIMA-SEATS sur la série corrigée
x13_result <- seas(ts_freq_mens_cinema_0020_corr)

# Afficher un résumé du modèle
summary(x13_result)

# Graphique de la décomposition
plot(x13_result)

# Extraire la série désaisonnalisée
serie_desaisson <- final(x13_result)

# Afficher la série désaisonnalisée
plot(serie_desaisson, main = "Série Désaisonnalisée", col = "blue", lwd = 2)

# Retourner les données désaisonnalisées
serie_desaisson



#-- 5. Prévision de la série saisonnière corrigée des points atypiques sur une année ----------

#-- 5. 1. Estimer et prévoir les modèles suivants ----------

#-- 5. 1. 1. Les premières méthodes ----------

#### naïves ----

pred_naive <- naive(ts_freq_mens_cinema_0020_corr, h=12)
show(pred_naive)
plot(pred_naive)

####  StructTS ----

fitsts = StructTS(ts_freq_mens_cinema_0020_corr)
plot(cbind(fitted(fitsts), residuals(fitsts)))
show(fitsts)

summary(fitsts)
prevsts <- forecast(fitsts, 12) 
show(prevsts) 
plot(prevsts)

#### stlm ----

decomp = stl(ts_freq_mens_cinema_0020_corr, s.window="periodic")
# show(decomp)
plot(decomp)

fitstl = stlm(ts_freq_mens_cinema_0020_corr)

prevstl <- forecast(fitstl,12) #période d'une année

# show(prevsts) # pas mettre en annexe

plot(prevstl) # en annexe

summary(prevstl)

#### X13 ----

myspec <- x13_spec("RSA5c")

mysax13 <- x13(ts_freq_mens_cinema_0020_corr, myspec)

summary(mysax13$regarima)

mysax13

plot(mysax13$final)

# Extraire la série désaisonnalisée
sa_series <- mysax13$final$series[, "sa"]

# Appliquer la prévision sur 12 périodes
forecast_x13 <- forecast(sa_series, h = 12)

# Visualiser le résultat
plot(forecast_x13)


#-- 5. 1. 2. Prédiction sur les méthodes de lissage exponentiel ----------


#### Holt-winters ----

seasX <- seas(ts_freq_mens_cinema_0020_corr)
cvs <- final(seasX)

# lissage
# partie constante, tendance et saisonnalité et choix additif ou multiplicatif
WH_add = HoltWinters(cvs, seasonal = "mul") # il suppose un modèle add de base donc je change à la main
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

# horizon h=50 - intervals 80% & 95%
library(forecast)
fit = forecast(WH_add, h=50)
plot(fit)
show(fit)

#### ETS ----

fitets <- ets(ts_freq_mens_cinema_0020_corr, ic = "aic") # pour avoir le meilleur AIC
show(fitets)
plot(fitets)


prevets <- forecast(fitets,12)
show(prevets)
plot(prevets)

#### TBATS ----

fit_tbats <- tbats(ts_freq_mens_cinema_0020_corr)
show(fit_tbats)
plot(fit_tbats)

prev_TBATS <- forecast(fit_tbats, h=12)
plot_prev_TBATS <- plot(prev_TBATS)

#### ADAM ETS ----

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

#### ADAM ETS + SARIMA ----

fitadam3 <- auto.adam(ts_freq_mens_cinema_0020_corr, model="ZZN", lags=c(1,12), orders=list(ar=c(3,3), i=(2),
                                                                                            ma=c(3,3), select=TRUE))
fitadam3
summary(fitadam3)

par(mfcol=c(2,2))
plot(fitadam3)

par(mfcol=c(1,1))
plot(fitadam3$states)
plot(fitadam3$residuals)

prev_AES <- forecast(fitadam3,12)
show(prev_AES)
plot(prev_AES)

#### SSARIMA ----

fit_SSARIMA <- auto.ssarima(ts_freq_mens_cinema_0020_corr, lags=c(1,12), orders=list(ar=c(3,3), i=(2), ma=c(3,3), select=TRUE))
fit_SSARIMA

par(mfcol=c(2,2))
plot(fit_SSARIMA)

par(mfcol=c(1,1))

plot(fit_SSARIMA$residuals)

prev_SSARIMA <- forecast(fit_SSARIMA, h=12)
prev_SSARIMA
plot(prev_SSARIMA)

#-- 5. 1. 3. Modèle SARIMA(p, d, q)(P, D, Q)[12] ----------

fit_sarima <- auto.arima(ts_freq_mens_cinema_0020_corr, seasonal = TRUE)

# Affichage du modèle
summary(fit_sarima)
plot(fit_sarima$residuals)

# Prévision sur 12 périodes
prev_SARIMA <- forecast(fit_sarima, h=12) 

# Affichage des prévisions

plot(prev_SARIMA)

#-- 5. 2.  le meilleur modèle d’après les critères AIC et AICc ----------


# X13 : 7317 juste après   /   SARIMA : 7329.52


#---------- 6. Représenter graphiquement l’évolution des prévisions des différents modèles ----------

ts_freq_mens_cinema_2021 <- ts(ts_freq_mens_cinema_corr, 
                               start = c(2020, 1), end =c(2020, 12),
                               frequency = 12)

library(ggplot2)
library(dplyr)
library(zoo)
library(scales)  # Pour formater l'axe des dates

# Fonction mise à jour : transforme la date en format mois
extract_forecast_df <- function(fcast_obj, name) {
  df <- data.frame(
    date = as.Date(as.yearmon(time(fcast_obj$mean))),  # conversion en date
    value = as.numeric(fcast_obj$mean),
    model = name
  )
  df %>% slice_head(n = 12)
}

# Appliquer la fonction à chaque objet de prévision
dfs <- list(
  extract_forecast_df(prev_ADAM_ETS, "ADAM_ETS"),
  extract_forecast_df(prev_AES, "AES"),
  extract_forecast_df(prevets, "ETS"),
  extract_forecast_df(fit, "fit"),
  extract_forecast_df(prev_SARIMA, "SARIMA"),
  extract_forecast_df(prev_SSARIMA, "SSARIMA"),
  extract_forecast_df(prevstl, "STL"),
  extract_forecast_df(prevsts, "STS"),
  extract_forecast_df(prev_TBATS, "TBATS"),
  extract_forecast_df(forecast_x13, "X13"),
  extract_forecast_df(pred_naive, "NAÏVE")
  
)

# Regrouper tous les modèles ensemble
df_all <- bind_rows(dfs)

# Transformer la série temporelle en data frame pour ggplot
ts_freq_mens_cinema_2021_df <- data.frame(
  date = as.Date(time(ts_freq_mens_cinema_2021)),
  value = as.numeric(ts_freq_mens_cinema_2021),
  model = "Observed Data"
)

# Affichage avec mois formatés
ggplot() +
  geom_line(data = df_all, aes(x = date, y = value, color = model), size = 1) +
  geom_line(data = ts_freq_mens_cinema_2021_df, aes(x = date, y = value),
            linetype = "dashed", color = "black", size = 1) +
  labs(title = "Prévisions sur 12 mois par modèle",
       x = "Mois", y = "Valeur prévue", color = "Modèle") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "1 month") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


#---------- 7. Qualité de prévision ----------

## MSE & R²OOS

# Exemple de données (à remplacer par vos données réelles)
actual_values <- ts_freq_mens_cinema_2021

# Liste des prévisions pour chaque modèle
forecasts <- list(
  prev_ADAM_ETS$mean,
  prev_AES$mean,
  prevets$mean,
  fit$mean,
  prev_SARIMA$mean,
  prev_SSARIMA$mean,
  prevstl$mean,
  prevsts$mean,
  prev_TBATS$mean,
  forecast_x13$mean,
  pred_naive$mean
)

# Fonction pour calculer MSE et R²OOS
calculate_metrics <- function(actual, forecast, benchmark_forecast) {
  mse <- mean((actual - forecast)^2)
  sst <- sum((actual - benchmark_forecast)^2)
  sse <- sum((actual - forecast)^2)
  r2oos <- 1 - (sse / sst)
  return(list(mse = mse, r2oos = r2oos))
}

# Calculer les métriques pour chaque modèle
metrics <- lapply(forecasts, function(fcast) {
  calculate_metrics(as.numeric(actual_values), as.numeric(fcast), as.numeric(prevstl$mean))
})

# Convertir en data frame pour une meilleure lisibilité
metrics_df <- as.data.frame(do.call(rbind, metrics))
rownames(metrics_df) <- c("ADAM_ETS", "AES", "ETS", "HW", "SARIMA", "SSARIMA", "STL", "STS", "TBATS", "X13", "NAÏVE")
print(metrics_df)

## CSPE

# Fonction pour calculer les CSPE
calculate_cspe <- function(actual, forecast) {
  errors <- (actual - forecast)^2
  cspe <- cumsum(errors)
  return(cspe)
}

# Calculer les CSPE pour chaque modèle
cspe_list <- lapply(forecasts, function(fcast) {
  calculate_cspe(as.numeric(actual_values), as.numeric(fcast))
})

# Convertir en data frame pour le traçage
cspe_df <- do.call(cbind, cspe_list)
colnames(cspe_df) <- c("ADAM_ETS", "AES", "ETS", "fit", "SARIMA", "SSARIMA", "STL", "STS", "TBATS", "X13", "NAÏVE")
cspe_df <- cbind(Date = as.Date(time(actual_values)), cspe_df)


# Convertir la matrice en data frame
cspe_df <- as.data.frame(cspe_df)

# Vérifier la structure du data frame
str(cspe_df)

# Utiliser pivot_longer pour transformer le data frame
cspe_df_long <- pivot_longer(cspe_df, cols = -Date, names_to = "Model", values_to = "CSPE")

# Tracer les CSPE
ggplot(cspe_df_long, aes(x = Date, y = CSPE, color = Model)) +
  geom_line() +
  labs(title = "Cumulative Squared Prediction Errors (CSPE)",
       x = "Date", y = "CSPE", color = "Model") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#---------- 8. Test de précision ----------

# Définir la période de temps
start_date <- c(2020, 1)
end_date <- c(2020, 12)

# Fonction pour ajuster les séries temporelles
adjust_time_series <- function(ts_data) {
  return(window(ts_data, start = start_date, end = end_date))
}

# Ajuster les séries temporelles pour chaque modèle
for_observed <- adjust_time_series(ts_freq_mens_cinema_2021)
for_ADAM_ETS <- adjust_time_series(prev_ADAM_ETS$mean)
for_AES <- adjust_time_series(prev_AES$mean)
for_ETS <- adjust_time_series(prevets$mean)
for_fit <- adjust_time_series(fit$mean)
for_SARIMA <- adjust_time_series(prev_SARIMA$mean)
for_SSARIMA <- adjust_time_series(prev_SSARIMA$mean)
for_STL <- adjust_time_series(prevstl$mean)
for_STS <- adjust_time_series(prevsts$mean)
for_TBATS <- adjust_time_series(prev_TBATS$mean)
for_X13 <- adjust_time_series(forecast_x13$mean)
for_NAÏVE <- adjust_time_series(pred_naive$mean)


# Vérifiez que toutes les séries temporelles ont la bonne longueur
print(length(for_observed))
print(length(for_ADAM_ETS))
print(length(for_AES))
print(length(for_ETS))
print(length(for_fit))
print(length(for_SARIMA))
print(length(for_SSARIMA))
print(length(for_STL))
print(length(for_STS))
print(length(for_TBATS))
print(length(for_X13))
print(length(for_NAÏVE))


# Calculer les erreurs de prévision
error_ADAM_ETS <- for_ADAM_ETS - for_observed
error_AES <- for_AES - for_observed
error_ETS <- for_ETS - for_observed
error_fit <- for_fit - for_observed
error_SARIMA <- for_SARIMA - for_observed
error_SSARIMA <- for_SSARIMA - for_observed
error_STL <- for_STL - for_observed
error_STS <- for_STS - for_observed
error_TBATS <- for_TBATS - for_observed
error_X13 <- for_X13 - for_observed
error_NAIVE <- for_NAÏVE - for_observed


# Calculer les MSE
mse_ADAM_ETS <- mean(error_ADAM_ETS^2)
mse_AES <- mean(error_AES^2)
mse_ETS <- mean(error_ETS^2)
mse_fit <- mean(error_fit^2)
mse_SARIMA <- mean(error_SARIMA^2)
mse_SSARIMA <- mean(error_SSARIMA^2)
mse_STL <- mean(error_STL^2)
mse_STS <- mean(error_STS^2)
mse_TBATS <- mean(error_TBATS^2)
mse_X13 <- mean(error_X13^2)
mse_NAIVE <- mean(error_NAIVE^2)



# Calculer d'autres mesures d'erreur
accuracy(for_ADAM_ETS, for_observed, h = 12)
accuracy(for_AES, for_observed, h = 12)
accuracy(for_ETS, for_observed, h = 12)
accuracy(for_fit, for_observed, h = 12)
accuracy(for_SARIMA, for_observed, h = 12)
accuracy(for_SSARIMA, for_observed, h = 12)
accuracy(for_STL, for_observed, h = 12)
accuracy(for_STS, for_observed, h = 12)
accuracy(for_TBATS, for_observed, h = 12)
accuracy(for_X13, for_observed, h = 12)
accuracy(for_NAÏVE, for_observed, h = 12)


# Calculer le test DM ----

## On n'utilise pas l'option "less" car les valeurs retournée du test ne sont pas significatives
## ce qui est contraire aux observations graphiques.
## On garde donc le test sans l'option.

dm.test(error_NAIVE, error_ADAM_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_AES, h = length(for_observed))
dm.test(error_NAIVE, error_ETS, h = length(for_observed))
dm.test(error_NAIVE, error_fit, h = length(for_observed))
dm.test(error_NAIVE, error_SARIMA, h = length(for_observed))
dm.test(error_NAIVE, error_SSARIMA, h = length(for_observed))
dm.test(error_NAIVE, error_STL, h = length(for_observed))
dm.test(error_NAIVE, error_STS, h = length(for_observed))
dm.test(error_NAIVE, error_TBATS, h = length(for_observed))
dm.test(error_NAIVE, error_X13, h = length(for_observed))


# Calculer le test DM avec h=1
dm.test(error_NAIVE, error_ADAM_ETS, h = 1)
dm.test(error_NAIVE, error_AES, h = 1)
dm.test(error_NAIVE, error_ETS, h = 1)
dm.test(error_NAIVE, error_fit, h = 1)
dm.test(error_NAIVE, error_SARIMA, h = 1)
dm.test(error_NAIVE, error_SSARIMA, h = 1)
dm.test(error_NAIVE, error_STL, h = 1)
dm.test(error_NAIVE, error_STS, h = 1)
dm.test(error_NAIVE, error_TBATS, h = 1)
dm.test(error_NAIVE, error_X13, h = 1)

## le meilleur modèle est X13

#---------- 9. Prévision ----------

fit <- tso(ts_freq_mens_cinema_0020)
adj <- fit$yadj
adj <- ts(adj)

estim <- 228 # 240 - 12 = 228
h <- 12
format <- matrix(nrow=h, ncol=1)

for(i in 1:h)
{
  adj2 <- adj[i:(estim-1+i)]
  adjdemetra <- ts(adj2, start = c(2000, 1),frequency = 12)
  myregx13 <- regarima_x13(adjdemetra, spec ="RG5c")
  forex13 <- matrix(myregx13$forecast[1])
  forc <- as.numeric(forex13)
  format[i,1] <- forc
}


# Affichage des prévisions
print(format)


# Création des mois à afficher
mois <- format(seq(as.Date("2020-01-01"), by = "month", length.out = 12), "%b %Y")

# Tracer les prévisions
plot(format, type = "o", main = "Prévisions X-13 glissantes", ylab = "Valeur", xlab = "Mois", col = "navy", lwd = 2, xaxt = "n")

# Ajouter les étiquettes des mois
axis(1, at = 1:12, labels = mois, las = 2, cex.axis = 0.8)


