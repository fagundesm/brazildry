library(dplyr)
library(ggplot2)

fac<- read.csv("data/BD_17-18.csv")
fac$plot <- as.factor(fac$plot)
head(fac)
str(fac)

b<- read.csv("data/bruna.csv")
b<- b[,1:3]

m <- fac %>% 
  filter(ano==2017 & div!= "ctrl" )%>%
  select(plot, div, especie, fac_cresc, fac_sob, fac_tot)%>%
  group_by(plot, div)%>%
  summarise(medc= mean(fac_cresc),
            meds= mean(fac_sob),
            medt= mean(fac_tot),
            sc= sum(fac_cresc),
            ss= sum(fac_sob),
            st= sum(fac_tot),
            sdc= sd(fac_cresc),
            sds= sd(fac_sob),
            sdt= sd(fac_tot))

cres <- read.csv("data/BDcrescimento_por_plot.csv")

m$plot == cres$plot

g <- cbind(cres, as.data.frame(m[ ,-c(1,2)]))
s<- read.csv("brazilDry_gis.csv")

s$plot == g$plot

g<- cbind(g, s[,(8:11)]) 

g1 <- g %>% group_by(div) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

g1$div <- as.factor(g1$div)

ggplot(g1,aes(y=cresc.nfol , x=div))+
  theme(axis.text.x = element_text( hjust = 1, size=20),
        axis.text.y=element_text(size=20), 
        axis.title=element_text(size=20), 
        panel.background = element_rect(fill='white', colour='black'), 
        axis.text=element_text(colour="black"),
        legend.text=element_text(size=16))+
  ylab("")  +  xlab("")+ 
  geom_bar(stat="identity",position="dodge")+
  ylab("number of leaves growth ")  +  xlab("Diversity")+
  geom_errorbar(aes(ymin=cresc.nfol - cresc.nfol_sdErr, ymax= cresc.nfol+ cresc.nfol_sdErr), width=.5,position=position_dodge(.5))



a <- ggplot(g, aes(y=cresc.biomassa, x=meds))+
  geom_point(size=4,show.legend = FALSE)+
  geom_smooth()+
  xlab("Potential of Facilitation on survival")+
  ylab("Aboveground Biomass growth (g)")+
  theme(axis.text.x = element_text(size=16, colour="black"),
        axis.title = element_text(size=20),
        axis.text.y = element_text(size=16,colour="black"),
        panel.background = element_rect(fill='white', colour='black'))


m$plot == b$PLOT
pl<- cbind(b,as.data.frame(m))

pl1<- pl %>% 
  filter(div=="16")


