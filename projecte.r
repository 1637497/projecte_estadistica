penguins <- read.csv("/Users/aina/Desktop/uni/3r/1r_semestre/estadística/projecte/penguins_binary_classificati1.csv")

head(penguins)
summary(penguins)
 
especies <- penguins$species
especies_bin <- as.numeric(especies == "Adelie")
illa <- penguins$island
bec_llargada <- penguins$bill_length_mm
bec_profunditat <- penguins$bill_depth_mm
aleta_llargada <- penguins$flipper_length_mm
massa_corporal <- penguins$body_mass_g
anys <- penguins$year

pinguinos_adelie <- subset(pinguinos, species == "Adelie")
pinguinos_gentoo <- subset(pinguinos, species == "Gentoo")
summary(pinguinos_adelie)
summary(pinguinos_gentoo)


modelo_glm <- glm(especies_bin ~ bec_llargada + aleta_llargada + massa_corporal, family = "binomial")
summary(modelo_glm)
