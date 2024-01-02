#EXERCICI 1

#Carreguem les dades
pinguins <- read.csv("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/pinguins.csv")

#Mostrem el dataset i un resum
head(pinguins)
summary(pinguins)

#Guardem cada variable en un vector
especies <- pinguins$species
especies <- as.numeric(especies == "Adelie")
bec_llargada <- pinguins$bill_length_mm
bec_profunditat <- pinguins$bill_depth_mm
aleta_llargada <- pinguins$flipper_length_mm
massa_corporal <- pinguins$body_mass_g

#Fem el model
model <- lm(especies ~ bec_llargada + bec_profunditat + aleta_llargada + massa_corporal)


# VALIDACIÓ GLOBAL ---------------------------------------------------------------------------------------------------------------------------------------
anova(model, test = "Chisq")


# VALIDACIÓ INDIVIDUAL ---------------------------------------------------------------------------------------------------------------------------------------
coef(summary(model))


# MATRIU DE VARIÀNCIES I COVARIÀNCIES ---------------------------------------------------------------------------------------------------------------------------------------
penguins_matriu <- data.frame(especies = especies, bec_llargada = bec_llargada, bec_profunditat=bec_profunditat, aleta_llargada = aleta_llargada, massa_corporal = massa_corporal)
matriu_covariancies <- vcov(penguins_matriu)
matriu_covariancies

variancies <- diag(matriu_covariancies)
variancies


# RESIDUALS ---------------------------------------------------------------------------------------------------------------------------------------
residuals <- residuals(model)
residuals
qqnorm(residuals)
qqline(residuals)


# PREDICCIONS ---------------------------------------------------------------------------------------------------------------------------------------
#Llegim les dades de test 
library(readxl)
pinguins_test <- read_excel("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/penguins_test.xlsx")

#Guardem cada variable en un vector
especies_t <- pinguins_test$species
especies_t <- as.numeric(especies_t == "Adelie")
bec_llargada_t <- pinguins_test$bill_length_mm
bec_profunditat_t <- pinguins_test$bill_depth_mm
aleta_llargada_t <- pinguins_test$flipper_length_mm
massa_corporal_t <- pinguins_test$body_mass_g
dataset_test <- data.frame(especies = especies_t, bec_llargada = bec_llargada_t, bec_profunditat=bec_profunditat_t, aleta_llargada = aleta_llargada_t, massa_corporal = massa_corporal_t)

#Fer les prediccions
prediccions <- predict(model, dataset_test, type = "response")
prediccions_binarias <- as.numeric(prediccions > 0.5)
prediccions_binarias


# INTÈRVALS DE CONFIANÇA ---------------------------------------------------------------------------------------------------------------------------------------
#Intèrval de confiança
interval_confiança <- confint(model)
interval_confiança

#Intèrval de confiança per les dades de test
prediccions_amb_interval <- predict(model, dataset_test, type = "response", interval = "confidence")
prediccions_amb_interval


# MODEL RESTRINGIT ---------------------------------------------------------------------------------------------------------------------------------------
model_restringit <- lm(especies ~ bec_llargada + aleta_llargada + massa_corporal)
anova(model_restringit, model, test = "Chisq")


# ALGUNES GRÀFIQUES ---------------------------------------------------------------------------------------------------------------------------------------
# Gràfic de barres de la distribució d'espècies
ggplot(pinguins, aes(x = species, fill = species)) +
  geom_bar() +
  labs(title = "Distribució d'espècies",
       x = "Espècie",
       y = "Freqüència")

# Gràfic de dispersió de la longitud del bec (bec_llargada) i la longitud de l'aleta (aleta_llargada)
ggplot(pinguins, aes(x = bec_llargada, y = aleta_llargada, color = species)) +
  geom_point() +
  labs(title = "Gràfic de dispersió entre longitud del bec i longitud de l'aleta",
       x = "Longitud del bec",
       y = "Longitud de l'aleta")

# Gràfic de dispersió de la longitud del bec (bec_llargada) i la massa corporal (massa_corporal)
ggplot(pinguins, aes(x = bec_llargada, y = massa_corporal, color = species)) +
  geom_point() +
  labs(title = "Gràfic de dispersió entre longitud del bec i massa corporal",
       x = "Longitud del bec",
       y = "Massa corporal")

# Gràfic de dispersió de la longitud de l'aleta (aleta_llargada) i la massa corporal (massa_corporal)
ggplot(pinguins, aes(x = aleta_llargada, y = massa_corporal, color = species)) +
  geom_point() +
  labs(title = "Gràfic de dispersió entre longitud de l'aleta i massa corporal",
       x = "Longitud de l'aleta",
       y = "Massa corporal")



