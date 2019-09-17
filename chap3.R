# 1. A sobreposição de atributos funcionais e a diversidade funcional influencia o overyield? Y=overyield, X=sobreposição.
library(dplyr)
library(lme4)
library(lmerTest)
library(jtools)

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

c <- read.csv("data/CWM_plots.csv")
cmw <- c[,-3] %>% group_by(plot, div) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))%>%
  arrange(plot, div)
bm <- biom%>%
  arrange(plot, div)
cwm_b <- cbind(bm,cmw[,-c(1,2)], chp[,c(1,3,8)])


f<- fd %>% filter( SR != "1")
n<- no %>% filter( DIV != "1")
chp2 <- cbind(f, n[,-c(1,2)], oy[,-1])

# meanG= média das sobreposições pareadas do geange, 
# NBE = Net biodiversity effect (Loreau & Hector 2001)
# CE = Complementarity effect (Loreau & Hector 2001)
# SE = Sampling effect (Loreau & Hector 2001)
# FD = Functional diversity (Patchey & Gaston)
# SR = Species richness


#1 A produtividade é explicada por efeitos de complementariedade, diversidade ou CWM?
#glmm cwm biomassa
cm2<- scale(cwm_b[,-c(1,2,3,4,5,17,18)])
cor(cm2)

cwm_ana<- cbind(cwm_b[,c(1,2,3,4,5,17,18)], cm2)
cwm_b$div <- as.factor(cwm_b$div)
cwm_b$plot <- as.factor(cwm_b$plot)

biom_rand <- lmer(cresc.biomassa ~ SLA + LA + altura_aerea_cm + t_dens+ r_dens + SRA + div + PD + (1|COMP), data=cwm_ana)
summary(biom_rand)

#1 A NBE é explicada por efeitos de complementariedade, diversidade ou CWM?
#glmm cwm biomassa
cmmb<- filter(cwm_b, div !="1")
NBE <- cbind(chp2, cmmb[,-c(1,2)])
NBE_ana<- select(NBE, plot, div, SLA, LA, altura_aerea_cm, t_dens, r_dens, SRA)



#Testando quais plots tem NBE diferente de zero
nbe <- chp2%>%
  select(SR, COMP, NBE,CE,SE) 
b <- select(nbe, CE)
t.test(b)


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



