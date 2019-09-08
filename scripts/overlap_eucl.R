library(dplyr)
cm <- read.csv("data/CWM_plots.csv")

cm1 <- select(cm, plot, div, especie, SLA, altura_aerea_cm,  t_dens, t_capac_arm_casca, 
              r_capac_arm_casca, comp_raiz_cm,  r_dens, SRA)

cm1<- filter(cm1, div != "1")

pairw <- cm1 %>% group_by(plot, div, especie) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

#Niche overlap
#Uma matriz por trait por comunidade SLA
a <- select(pairw, plot, div, especie, SRA)
a1 <- filter(a, plot=="91")
a1 <- a1[,-c(1:3)]
a1 <- as.matrix(a1)

a2 <- dist(a1, method="euclidean")

print(a3 <- sum(as.matrix(a2/2)))
mean(a3/length(a2))

(dist(a1, method="euclidean"))

#Calculo com o overlap por trait sobre overyield
library(mgcv)
eu <- read.csv("Overyield_euclid.csv")

d<- gam(overyield_biom ~ s(SLA_mean) + s(altura_aerea_mean) + s(t_dens_mean) + s(t_capac_arm_casca_mean)
        +s(r_capac_arm_casca_mean) +s(comp_raiz_cm_mean) +s(r_dens_mean) +s(SRA_mean), data=eu)
summary(d)
