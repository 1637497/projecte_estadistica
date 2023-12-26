EXERCICI 1

#Carreguem les dades
pinguins <- read.csv("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/penguins_binary_classification.csv")

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
# PREDICCIONS ---------------------------------------------------------------------------------------------------------------------------------------
# PREDICCIONS ---------------------------------------------------------------------------------------------------------------------------------------
# PREDICCIONS ---------------------------------------------------------------------------------------------------------------------------------------
# PREDICCIONS ---------------------------------------------------------------------------------------------------------------------------------------





# PREDICCIONS ---------------------------------------------------------------------------------------------------------------------------------------
#Llegim les dades de test 
library(readxl)
pinguins_test <- read_excel("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/penguins_test.xlsx")

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
prediccions_binarias <- as.numeric(predicciones > 0.5)
prediccions_binarias

