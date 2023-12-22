
#Carreguem les dades
pinguins <- read.csv("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/penguins_binary_classification.csv")

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

#Fem el model de regressió lineal múltiple
modelo_glm <- glm(especies ~ bec_llargada + aleta_llargada + massa_corporal, family = "binomial", maxit = 30)
summary(modelo_glm)

#Valoració global
anova(modelo_glm, test = "Chisq")

#Valoració individual
coef(summary(modelo_glm))

# Matriu de covariànices
penguins_matriu <- data.frame(especies = especies, bec_llargada = bec_llargada, aleta_llargada = aleta_llargada, massa_corporal = massa_corporal)
matriu_covariancies <- cov(penguins_matriu)
matriu_covariancies

#Matriu de variàncies
variancies <- diag(matriu_covariancies)
variancies

#Residuals
residuals <- residuals(modelo_glm)
residuals
ggplot() +
  geom_point(aes(x = fitted(modelo_glm), y = residuals)) +
  labs(title = "Gràfic de residuals",
       x = "Valors ajustats",
       y = "Desviació residual")
qqnorm(residuals)
qqline(residuals)

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

dataset_test <- data.frame(especies = especies_t, bec_llargada = bec_llargada_t, aleta_llargada = aleta_llargada_t, massa_corporal = massa_corporal_t)

# Realiza las predicciones
prediccions <- predict(modelo_glm, dataset_test, type = "response")
prediccions_binarias <- as.numeric(predicciones > 0.5)
prediccions_binarias

#Intèrval de confiança per les dades de train
interval_confiança <- confint(modelo_glm)
interval_confiança

#Intèrval de confiança per les dades de test
prediccions_amb_interval <- predict(modelo_glm, dataset_test, type = "response", interval = "confidence")
prediccions_amb_interval

#Model restringit
model_restringit <- glm(especies ~ aleta_llargada + massa_corporal, family = "binomial")
anova(model_restringit, modelo_glm, test = "Chisq")

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
