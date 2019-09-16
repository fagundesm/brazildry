# 1. A sobreposição de atributos funcionais e a diversidade funcional influencia o overyield? Y=overyield, X=sobreposição.
library(dplyr)
setwd("~/Dropbox/brazildry")

biom <- read.csv("data/BDcrescimento_por_plot.csv")
biom <- arrange(biom, plot)

no <-read.csv("no_geange.csv")
no <- arrange(no, PLOT)
no$PLOT <- as.factor(no$PLOT)

fd <- read.csv("FD.csv")
fd <- arrange(fd, plot)
fd$plot <- as.factor(fd$plot)

biom$plot == no$PLOT
no$PLOT == fd$plot

oy <- read.csv("overyield_Ben.csv")
oy <- arrange(oy, plot)
oy$plot <- as.factor(oy$plot)

chp <- cbind(no, biom[,-c(1,2)], fd[,-3])
summary(chp)
chp <- as.data.frame(chp)

#
f<- fd %>% filter( SR != "1")
n<- no %>% filter( DIV != "1")
chp2 <- cbind(f, n[,-c(1,2)], oy[,-1])

# meanG= média das sobreposições pareadas do geange, 
# NBE = Net biodiversity effect (Loreau & Hector 2001)
# CE = Complementarity effect (Loreau & Hector 2001)
# SE = Sampling effect (Loreau & Hector 2001)
# FD = Functional diversity (Patchey & Gaston)
# SR = Species richness
library(lme4)
library(lmerTest)
library(jtools)

#Testando quais plots tem NBE diferente de zero
nbe <- chp2%>%
    select(SR, COMP, NBE,CE,SE) 
b <- select(nbe, CE)
t.test(b)

#1 A produtividade é explicada por efeitos de complementariedade?
str(chp)
chp$cresc.biomassa <- chp$cresc.biomassa + 1
modb <- glm(log(cresc.biomassa) ~  PD +  DIV + COMP , data=chp)
summary(modb)
plot(modb)
summ(modb)

#discutir o efeito liquido do NBE
library(bestNormalize)

y<- bestNormalize(chp2$NBE)
FD<- bestNormalize(chp2$PD)

chp2$NBE <- chp2$NBE + 5
as.factor(chp$SR)
modc <- lmer(y$x.t ~  FD$x.t +  SR + (1|COMP), data=chp2)
plot(modc)
summary(modc)

#Destrinchar os mecanismos de CE e SE na montagem de comunidades
chp2$CE <- chp2$CE +10 
ya<- bestNormalize(chp2$CE)

modd<- lmer(ya$x.t ~ log(PD)  +  SR + (1|COMP), data=chp2)
summary(modd)
plot(modd)

#ver se o selection effect é afetado (diminuido) se a DF ou NO da comunidade for menor
chp2$SE <- chp2$SE + 224.4985
yb<- bestNormalize(chp2$SE)

mode <- lmer(yb$x.t ~ log(PD)  +  SR + (1|COMP), data=chp2)
plot(mode)
summary(mode)     
summ(mode)

plot(chp2$SE, chp2$CE)


# 1 Descrição da biomassa geral 
a <-chp%>%
  group_by(COMP, DIV)%>%
  summarise_all(funs(mean(., na.rm = TRUE), sd(., na.rm = TRUE)))
  
plot(chp$cresc.biomassa ~ chp$CE)
plot(chp$cresc.biomassa ~ chp$comp)
plot(chp$cresc.biomassa ~ chp$SE)
plot(chp$cresc.biomassa ~ chp$div)

r2(modb)

#Qual espécie tem a maior produção de biomassa?
se <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

s <- read.csv("data/BDcrescimento_por_sp_por_plot.csv")

s$div <- as.factor(s$div)
s$plot <- as.factor (s$plot)

creb<- bestNormalize(s$cresc.biomassa)
hist(creb$x.t)

anovabio<- aov(creb$x.t ~ especie * div, data=s)
summary(anovabio)



