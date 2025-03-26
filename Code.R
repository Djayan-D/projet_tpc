#---------- 1. CHARGER LES PACKAGES ----------

library(readxl)
library(zoo)
library(ggplot2)
library(dplyr)
library(tidyr)





#---------- 2. IMPORTER LES DONNÉES ----------

#----- 2.1. Charger les données -----

## https://www.cnc.fr/documents/36995/158946/fr%C3%A9quentation+et+films+dans+les+salles+de+cin%C3%A9ma.xlsx/df2fce1f-52e3-08f4-a8a1-172ee8fd8203

freq_mens_cinema <- read_excel("data/freq_mens_cinema.xlsx")



#----- 2.2. Réorganiser la base -----

# Renommer la première colonne (année)

names(freq_mens_cinema)[1] <- "Annee"


# Retirer la colonne total

freq_mens_cinema <- freq_mens_cinema |> 
  select(-Total) |> # retirer la colonne "Total"
  round() # arrondir pour tirer les virgules


# Extraire la période 2000-2024

freq_mens_cinema_0024 <- freq_mens_cinema |> 
  filter(Annee >= 2000)



#----- 2.3. Convertir en TS -----

# Convertir les données en format long

freq_mens_cinema_long <- freq_mens_cinema |> 
  pivot_longer(cols = -Annee, names_to = "Mois", values_to = "Valeur")

freq_mens_cinema_0024_long <- freq_mens_cinema_0024 |> 
  pivot_longer(cols = -Annee, names_to = "Mois", values_to = "Valeur")


# Créer une colonne Date 

freq_mens_cinema_long$Date <- as.yearmon(paste(freq_mens_cinema_long$Annee, freq_mens_cinema_long$Mois), 
                                         format = "%Y %B")

freq_mens_cinema_0024_long$Date <- as.yearmon(paste(freq_mens_cinema_0024_long$Annee, freq_mens_cinema_0024_long$Mois),
                                              format = "%Y %B")



# Convertir en séries temporelles

ts_freq_mens_cinema <- zoo(freq_mens_cinema_long$Valeur, order.by = freq_mens_cinema_long$Date)

ts_freq_mens_cinema_0024 <- zoo(freq_mens_cinema_0024_long$Valeur, order.by = freq_mens_cinema_0024_long$Date)


# Visualiser les séries temporelles

plot(ts_freq_mens_cinema / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 1980-2024")

plot(ts_freq_mens_cinema_0024 / 1e6,
     xlab = "Temps",
     ylab = "Nombre d'entrées (en millions)",
     main = "Série temporelle des valeurs mensuelles 2000-2024")


# Décomposer les séries temporelles

ts_freq_mens_cinema |> 
  as.ts() |> 
  decompose() |> 
  plot()

ts_freq_mens_cinema_0024 |> 
  as.ts() |> 
  decompose() |> 
  plot()