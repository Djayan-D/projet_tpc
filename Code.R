#---------- 1. CHARGER LES PACKAGES ----------

library(readxl)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)
library(EnvStats)





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



#--- 2.2.3. Extraire la période 2000-2024 ---

freq_mens_cinema_0024 <- freq_mens_cinema |> 
  filter(Annee >= 2000)



#--- 2.2.4. Convertir les données en format long ---

freq_mens_cinema_long <- freq_mens_cinema |> 
  pivot_longer(cols = -Annee, names_to = "Mois", values_to = "Valeur")

freq_mens_cinema_0024_long <- freq_mens_cinema_0024 |> 
  pivot_longer(cols = -Annee, names_to = "Mois", values_to = "Valeur")



#--- 2.2.5. Créer une colonne Date ---

freq_mens_cinema_long$Date <- as.yearmon(paste(freq_mens_cinema_long$Annee, freq_mens_cinema_long$Mois), 
                                         format = "%Y %B")

freq_mens_cinema_0024_long$Date <- as.yearmon(paste(freq_mens_cinema_0024_long$Annee, freq_mens_cinema_0024_long$Mois),
                                              format = "%Y %B")



#----- 2.3. Vérifier les valeurs atypiques -----

#--- 2.3.1. Boxplots ---

boxplot(freq_mens_cinema_long$Valeur,
        main = "Boxplot 1980-2024")

## 3 valeurs potentiellement atypiques.


boxplot(freq_mens_cinema_0024_long$Valeur,
        main = "Boxplot 1980-2024")

## 4 valeurs potentiellement atypiques.



#--- 2.3.2. Rosner Test ---

rosnerTest(freq_mens_cinema_long$Valeur,
           k = 3)

## Aucune valeur réellement atypique, pas besoin de modifier la base.


rosnerTest(freq_mens_cinema_0024_long$Valeur,
           k = 4)

## Aucune valeur réellement atypique, pas besoin de modifier la base.



#----- 2.4. Convertir en TS -----

#--- 2.4.1. Convertir en séries temporelles ---

ts_freq_mens_cinema <- zoo(freq_mens_cinema_long$Valeur, order.by = freq_mens_cinema_long$Date)

ts_freq_mens_cinema_0024 <- zoo(freq_mens_cinema_0024_long$Valeur, order.by = freq_mens_cinema_0024_long$Date)



#--- 2.4.2. Visualiser les séries temporelles

plot(ts_freq_mens_cinema / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 1980-2024")

plot(ts_freq_mens_cinema_0024 / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 2000-2024")



#--- 2.4.3. Décomposer les séries temporelles ---

# 1980 - 2024

ts_freq_mens_cinema |> 
  as.ts() |> 
  decompose(, type = "additive") |> 
  plot()

ts_freq_mens_cinema |> 
  as.ts() |> 
  decompose(, type = "multiplicative") |> 
  plot()

## Multiplicatif est plus adapté

# 2000 - 2024

ts_freq_mens_cinema_0024 |> 
  as.ts() |> 
  decompose(, type = "additive") |> 
  plot()

ts_freq_mens_cinema_0024 |> 
  as.ts() |> 
  decompose(, type = "multiplicative") |> 
  plot()

## Multiplicatif est plus adapté