# Analyse et prévisions de la fréquentation des cinémas

**[Lien vers le dossier complet](https://drive.google.com/file/d/1ra52C2-ifjRMP10lQAayo-9o95teBYOf/view?usp=sharing)**

**Auteur** : Djayan Daëron, Arthur Ernoul de la Provôté  
**Contexte** : Techniques de prévision et conjoncture, Master 1 ECAP, Nantes Université (2024-2025)

---

## Contexte et objectifs
Ce projet vise à **analyser et prévoir la fréquentation mensuelle des cinémas en France** entre 1980 et 2023, en utilisant des données fournies par le **Centre National du Cinéma et de l'image animée (CNC)**. L'objectif principal est de :
- **Détecter les points atypiques** (ex. : impact de la pandémie de COVID-19, succès exceptionnels de films).
- **Identifier la saisonnalité** et les tendances sous-jacentes.
- **Comparer différents modèles de prévision** (SARIMA, TBATS, X13, etc.) pour évaluer leur performance.

---

## Structure des données
- **Période** : Janvier 1980 à décembre 2023.
- **Variable cible** : Nombre d'entrées mensuelles dans les cinémas.
- **Points atypiques** :
  - **Mars 2008** : Succès exceptionnel du film *Bienvenue chez les Ch'tis*.
  - **Mars 2020** : Début de la pandémie de COVID-19 (fermeture des salles).
  - **Novembre 2020** : Deuxième confinement.

---

## Méthodologie

### 1. Analyse exploratoire
- **Détection des points atypiques** :
  - Utilisation d'un modèle ARIMA saisonnier pour identifier les valeurs aberrantes.
  - Trois anomalies majeures détectées (mars 2008, mars 2020, novembre 2020).

- **Détection de la saisonnalité** :
  - **Périodogramme** : Pic marqué à une fréquence de 0.083 (période de 12 mois).
  - **Tests statistiques** :
    - Test de Wo : $p$-value = 0 (saisonnalité forte).
    - Test de Kruskal-Wallis : $p$-value < $2.2 \times 10^{-16}$ (distributions mensuelles différentes).

- **Désaisonnalisation** :
  - Décomposition multiplicative via X13-ARIMA-SEATS.
  - Modèle retenu : ARIMA(1,0,0)(0,1,1)[12] avec transformation logarithmique.

---

### 2. Modèles de prévision
**10 modèles testés** :
1. **Méthode naïve** : Prévision basée sur la dernière valeur observée.
2. **X13-ARIMA-SEATS** : Décomposition saisonnière et ajustement ARIMA.
3. **STL + ETS** : Décomposition STL suivie d'un modèle ETS.
4. **Holt-Winters** : Lissage exponentiel avec composante saisonnière.
5. **ETS** : Modèle ETS multiplicatif.
6. **TBATS** : Modèle TBATS avec transformation Box-Cox.
7. **ADAM ETS** : Modèle ETS avec ajustement automatique.
8. **ADAM ETS + SARIMA** : Combinaison de ETS et SARIMA.
9. **SARIMA** : Modèle SARIMA(2,0,1)(1,1,1)[12].
10. **SSARIMA** : Modèle SSARIMA(0,0,2)(3,0,0)[12].

---

## Résultats

### Qualité des prévisions
- **Meilleur modèle selon AIC** : **X13-ARIMA-SEATS** (AIC = 7317).
- **Meilleur modèle selon MSE et $R^{2}$ OOS** : **TBATS** (MSE = $1.097 \times 10^{13}$, $R^{2}$ OOS = 0.205).
- **Meilleur modèle en rolling window** : **SARIMA** (ajustement précis aux valeurs récentes).

**Tableau comparatif des performances** :
| Modèle               | MSE (x $10^{13}$) | $R^{2}$ OOS  | AIC     |
|----------------------|-------------------|--------------|---------|
| TBATS                | 1.097             | 0.205        | 8372.6  |
| X13                  | 1.249             | 0.095        | 7317.0  |
| SARIMA               | 1.561             | -0.131        | 7329.5  |
| Méthode naïve        | 6.133             | -3.443        | -       |

---

### Test de Diebold-Mariano
- **Modèles les plus performants** : **X13** et **Holt-Winters** (valeurs DM élevées, $p$-value < $10^{-6}$).

---

## Conclusion
- **SARIMA** est le modèle le plus adapté pour des **prévisions à court terme** (rolling window), grâce à sa réactivité aux valeurs récentes.
- **TBATS** offre le meilleur compromis pour des **prévisions globales** (MSE et $R^{2}$ OOS optimaux).
- **X13** est idéal pour une **analyse structurelle** (décomposition saisonnière robuste).

---

## Perspectives
- **Améliorer les modèles** :
  - Intégrer des variables exogènes (ex. : sorties de films majeurs, événements économiques).
  - Tester des modèles hybrides (ex. : SARIMA + réseaux de neurones).
- **Étendre l'analyse** :
  - Appliquer la méthodologie à d'autres secteurs culturels (ex. : théâtres, musées).
  - Comparer avec des données post-COVID pour évaluer l'impact durable de la pandémie.

---

## Sources
- **Données** : [CNC - Fréquentation et films en salles](https://www.cnc.fr/-/statistiques-par-secteur-open-data-1)
- **Outils** : R (`forecast`, `seasonal`, `ggplot2`), Quarto.
