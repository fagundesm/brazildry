  library(dplyr)
  library(ggplot2)
  
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

#Como a diversidade funcional cresce com a diversidade
f <- summarise(group_by(fd, SR, plot),
                   fd=mean(PD, na.rm=T))
f$SR<-as.factor(f$SR)
f$plot<-as.factor(f$plot)
ggplot(f, aes(y=fd, x=SR, fill=factor(plot)))+
  geom_point(size=4,show.legend = FALSE)+
  #geom_smooth(method="auto")+
  xlab("Species Diversity")+
  ylab("Functional Diversity")+
  theme(axis.text.x = element_text(size=16, colour="black"),
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=16,colour="black"),
        panel.background = element_rect(fill='white', colour='black'))


# Plot de crescimento de biomassa por diversidade. 
library(dplyr)
sdErr <- function(x) sqrt(var(x,na.rm=TRUE)/length(na.omit(x)))

bioma <- summarise(group_by(chp, DIV),
                   bioms=mean(cresc.biomassa, na.rm=T),
                   bstdr=sd(cresc.biomassa),
                   er=sdErr(cresc.biomassa))

bioma$DIV <- as.factor(bioma$DIV)

point <- select(chp, DIV, cresc.biomassa)
point$DIV <- as.factor(point$DIV)
 
chp$DIV <- as.factor(chp$DIV)
chp<- filter(chp, PLOT != "73")

ggplot(bioma, aes(y=bioms, x= DIV, fill=factor(DIV)))+
  theme(axis.text.x = element_text( hjust = 1, size=20),
        axis.text.y=element_text(size=20), 
        axis.title=element_text(size=20), 
        panel.background = element_rect(fill='white', colour='black'), 
        axis.text=element_text(colour="black"),
        legend.text=element_text(size=16))+
  geom_point(data=chp, aes(y=cresc.biomassa, x=DIV))+
  geom_bar(stat="identity",position="dodge", alpha = 0.6)+
  ylab("Relative biomass growth (g)")  +  xlab("Species diversity")+ 
  geom_errorbar(aes(ymin=bioms - er, ymax= bioms+ er), width=.5,
                position=position_dodge(.5))
   


# Plot de NBE por diversidade por composição
biomax <- summarise(group_by(chp2, SR, COMP),
                    nbe=mean(NBE, na.rm=T),
                    nbesd=sdErr(NBE))
                    
ggplot(biomax,aes(y=nbe, x= reorder(COMP, SR), fill=factor(SR)))+
  theme(axis.text.x = element_text( hjust = 1, size=20, angle=90),
        axis.text.y=element_text(size=20), 
        axis.title=element_text(size=20), 
        panel.background = element_rect(fill='white', colour='black'), 
        axis.text=element_text(colour="black"),
        legend.text=element_text(size=16))+
  geom_bar(stat="identity",position="dodge")+
  ylab("Net Biodiversity Effect")  +  xlab("Composition")+ 
  geom_errorbar(aes(ymin=nbe - nbesd, ymax= nbe+ nbesd), width=.5,
                position=position_dodge(.5))


# NBE por diversidade
bioma2 <- summarise(group_by(chp2, SR, COMP),
                   nbe=mean(NBE),
                   nbesd=sd(NBE),
                   se=mean(SE),
                   sesd=sdErr(SE),
                   ce=mean(CE),
                   cesd=sdErr(CE))

bioma2$SR <- as.factor(bioma2$SR)
ggplot(bioma2, aes(y=se, x=SR, fill=factor(COMP)))+
  geom_point(size=4,show.legend = FALSE)+
  #geom_smooth(method="auto")+
  xlab("Diversity level")+
  ylab("Selection Effect")+
    theme(axis.text.x = element_text(size=16, colour="black"),
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=16,colour="black"),
        panel.background = element_rect(fill='white', colour='black'))+
  geom_errorbar(aes(ymin=se - sesd, ymax= se+ sesd), width=.1)


#Qual espécie tem a maior produção de biomassa?
s<- read.csv("data/BDcrescimento_por_sp_por_plot.csv")
smo<- filter(s, div=="1")
s$div <- as.factor(s$div)

spbiom<- summarise(group_by(s, especie, div),
                   cb=mean(cresc.biomassa, na.rm=T),
                   scb=sdErr(cresc.biomassa))
smo<- filter(spbiom, div=="4")

ggplot(smo, aes(y=cb, x= reorder(cb,especie), fill=factor(especie)))+
    theme(axis.text.x = element_text( hjust = 1, size=20, angle=90),
          axis.text.y=element_text(size=20), 
          axis.title=element_text(size=20), 
          panel.background = element_rect(fill='white', colour='black'), 
          axis.text=element_text(colour="black"),
          legend.text=element_text(size=16))+
      geom_bar(stat="identity",position="dodge", alpha = 0.6)+
  ylab("Relative biomass growth (g)")  +  xlab("Species diversity 16")+ 
  geom_errorbar(aes(ymin=cb - scb, ymax= cb+ scb), width=.5,
                position=position_dodge(.5))

geom_point(data=chp, aes(y=cresc.biomassa, x=DIV))

