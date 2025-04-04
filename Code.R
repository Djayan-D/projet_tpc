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

#----- 3.1. Statistiques descriptives -----

#--- 3.1.1. Créer une fonction ---

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



#--- 3.1.2. Appliquer à la série 1980-2024 ---

stats_desc(freq_mens_cinema_long$Valeur)



#--- 3.1.3. Appliquer à la série 2000-2020 ---

stats_desc(freq_mens_cinema_0020_long$Valeur)



#----- 3.2. Vérifier les valeurs atypiques -----

#--- 3.2.1. Identifier les valeurs atypiques ---

outliers <- tso(ts_freq_mens_cinema_0020) 
print(outliers)

plot(outliers)
show(outliers)

# Mars 2008 (27 056 406 entrées)
#
# Le phénomène "Bienvenue chez les Ch’tis", sorti le 27 février 2008, a explosé 
# tous les records en France. Ce film de Dany Boon est rapidement devenu le plus 
# gros succès du box-office français (jusqu'à l'arrivée d'"Intouchables" en 2011).



#--- 3.2.2. Traiter les valeurs atypiques ---

ts_freq_mens_cinema_0020_corr <- outliers$yadj



#--- 3.2.3. Visualiser la TS corrigée ---


plot(ts_freq_mens_cinema_0020_corr / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 2000-2020")



#----- 3.4. Détecter la saisonnalité -----

#--- 3.4.1. Graphiques ---

ts_freq_mens_cinema_0020_corr |> 
  as.ts() |> 
  decompose(, type = "additive") |> 
  plot()

ts_freq_mens_cinema_0020_corr |> 
  as.ts() |> 
  decompose(, type = "multiplicative") |> 
  plot()


# Test

summary(regarima_x13(ts_freq_mens_cinema_0020_corr, spec ="RG5c"))

## Multiplicatif est plus adapté





#---------- 4. DÉSAISONNALISATION ET DÉCOMPOSITION ----------

# Appliquer X13-ARIMA-SEATS sur la série corrigée
x13_result <- seas(ts_freq_mens_cinema_0020)

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

# Lissage exponentiel

# Méthode LED tiens compte de la tendance mais pas de la saisonnalité

serie_desaisson_LED <- HoltWinters(serie_desaisson,gamma=FALSE) 

show(serie_desaisson_LED)
plot(serie_desaisson_LED)
plot(serie_desaisson_LED$fitted[,1])

# Smoothing parameters:
# alpha: 0.3460451
# beta : 0.09966349
# gamma: FALSE

# Indique un lissage modéré des valeurs récentes.
# Une valeur plus proche de 1 donnerait plus de poids aux observations récentes.

# Une faible valeur de Beta signifie que la tendance évolue lentement.

## La composante de tendance montre une augmentation générale au fil du temps,
# avec des fluctuations bien visibles autour de la tendance.


#-- 5. Prévision de la série saisonnière corrigée des points atypiques sur une année ----------

## Estimer et prévoir les modèles suivants :

### Forecasting with h number of periods for forecasting with StructTS

fitsts = StructTS(ts_freq_mens_cinema_0020)
prevsts <- forecast(fitsts,12)
show(prevsts) # pas mettre en annexe
plot(prevsts) # en annexe

### Forecasting with h number of periods for forecasting with stlm

fitstl = stlm(ts_freq_mens_cinema_0020)
prevstl <- forecast(fitstl,12)
show(prevstl)
plot(prevstl)

### Forecasting with X13

# prevX13 = predict(ts_freq_mens_cinema_0020, 12, prediction.interval = TRUE) # on spécifie les intervalles de confiance
# plot(ts_freq_mens_cinema_0020, prevX13)
# show(prevX13)
# prevp = prevX13[1]
# show(prevp)
















