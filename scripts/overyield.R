sdErr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))
library(dplyr)
# 1. A sobreposição de atributos funcionais influencia o overyield? Y=overyield, X=sobreposição. 
#Altura aerea, densidade do tronco, capacidade armazenamento casca raiz, densidade lenho raiz, comprimento raiz

#Calculo de Overyield do Andy Hector e Loreau - Nature 2001 
#Mi = yield of species i in monoculture
# Yo,i = observed yield of species i in the mixture
#Yo= Sum Yo,i = total observed yield of the mixture
#RYe,i = expected relative yield of species i in the mixture, which is simply its proportion seeded or planted
#RYo,i = Yo,i Mi = observed relative yield of species i in the mixture #Ye,i = RYe,i Mi= expected yield of species i in the mixture
#Ye= Sum Ye,i = total expected yield of the mixture
#dY= Yo - Ye = deviation from total expected yield in the mixture
#dRYi= RYo,i - RYe,i = deviation from expected relative yield of species i in the mixture.
#N= number of species in a mixture

# dY= Yo - Ye = Sum RYo,i Mi - Sum RYe,i Mi = Sum dRYi Mi

#To quantify CE, the number of species in mixture (N) is multiplied by the average, across all component species, monoculture yield ( ̄M) and the average, across all component species, deviation from ex- pected relative yield (ΔRY).
# Complementarity effect = N*  mean(dRY) * mean(M)
# Selection effect = N cov(dRY,M) 

#Pacote pronto para fazer os cálculos <3 https://rdrr.io/github/BenjaminDelory/bef/man/apm.html
library(bef)
library(remotes)

dmono <- read.csv("mono.csv", h=T)
rownames(dmono)<-dmono[,1]
dmono<-dmono[,-1]
dmono<-as.matrix(dmono)

dmix <- read.csv("mix.csv", h=T)
rownames(dmix)<-dmix[,1]
dmix<-dmix[,-1]
dmix<-as.matrix(dmix)

dry <- read.csv("ry.csv", h=T)
dry <- arrange(dry, plot)
rownames(dry)<-dry[,1]
dry<-dry[,-1]
dry<-as.matrix(dry)

addlo<- apm(mix=dmix, mono=dmono, method="loreau")


#Testando quais NBE são diferentes de zero.

oy <- read.csv("overyield_Ben.csv")


#Calculo de Overyield do Andy Hector 1998. 

#Primeiro, fiz a média de monocultura entre as 3 réplicas.
#Depois disso, calculo a relação de produtividade entre produção em g  entre proporção de biomassa em policultura pela monocultura 
#proporção: policultura/monocultura = RY. A soma dos RYs=RY
#Para obter o RYT, soma-se os RY
cres <- read.csv("data/BDcrescimento_por_sp_por_plot.csv", h=T) 

mono <- cres %>% 
      filter( div=="1")%>% group_by(especie)%>%
      summarise(biom=mean(cresc.biomassa))

mix <- cres%>%
        filter(div != "1")%>% group_by(plot,especie)%>%
  summarise(biom=mean(cresc.biomassa))

over$RY_biom <- over$cresc.biomassa/over$mono_biomassa
over$RY_alt <- over$cresc.altura/over$mono_altura
overyield <- summarise(group_by(over, plot, especie, div),
                      bioms=sum(RY_biom, na.rm=T),
                      alts=sum(RY_alt, na.rm=T))
 

#autocorrelação espacial
#Do site http://www.ats.ucla.edu/stat/r/faq/morans_i.htm

library(ape)
# Matriz de distancias xy inversa e com zeros na diagonal principal
over.dist = as.matrix(dist(cbind(og$y, og$x)))

over.dist.inv = 1/over.dist
diag(over.dist.inv) = 0

Moran.I(Y, over.dist.inv)
#Retorna um P geral para autocorrelação espacial
og$div <- as.factor(og$div)
og$plot <- as.factor(og$plot)


