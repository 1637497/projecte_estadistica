#EXERCICI 3

#Carreguem les dades
pinguins <- read.csv("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/pinguins.csv")

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
model <- glm(especies ~  aleta_llargada + massa_corporal, family = "binomial")
summary(model)

# ANOVA ---------------------------------------------------------------------------------------------------------------------------------------
anova <- anova(model)
summary(anova)
