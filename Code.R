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

## Estimer et prévoir les modèles suivants : ----

### Les méthodes naïves ----

####  StructTS ----

fitsts = StructTS(ts_freq_mens_cinema_0020_corr)
prevsts <- forecast(fitsts,12) #période d'une année
show(prevsts) # pas mettre en annexe
plot(prevsts) # en annexe

#### stlm ----

fitstl = stlm(ts_freq_mens_cinema_0020_corr)
prevstl <- forecast(fitstl,12)
show(prevstl)
plot(prevstl)

#### X13 ----

# prevX13 = predict(ts_freq_mens_cinema_0020_corr, 12, prediction.interval = TRUE) # on spécifie les intervalles de confiance
# plot(ts_freq_mens_cinema_0020_corr, prevX13)
# show(prevX13)
# prevp = prevX13[1]
# show(prevp)

### Prédiction sur les méthodes de lissage exponentiel ----

#### Holt-winters ----

WH_add<- HoltWinters(ts_freq_mens_cinema_0020_corr,seasonal="add") # je spécifie schéma additif
# on à ici une tendance et une saisonnalité
show(WH_add)
plot(WH_add)
plot(WH_add$fitted[,1])

library(forecast)
fit_wh = forecast(WH_add, h=12)
plot(fit_wh)
show(fit_wh)
# Point forecasts
prevf_hw = fit_wh$mean
show(prevf_hw)

#### ETS ----

fit_ets <- ets(ts_freq_mens_cinema_0020_corr)
show(fit_ets)
plot(fit_ets)

prev_ETS





