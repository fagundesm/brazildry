BD<-read.table("BDcrescimento.txt", h=T)
data<-read.table("data_DivExp.txt", h=T)
sppMatrix<-data[data$richness!=0,8:ncol(data)]

BD$facilitation<-data$facilitation[BD$plot]
BD$compositions<-as.factor(data$compositions[BD$plot])
BD$log_rich<-log(BD$div+1)
BD$live<-as.numeric(BD$status=="V")
BD$cresc.altura[BD$live==0]<-NA


data$cresc.altura<-aggregate(BD$cresc.altura,list(BD$plot),mean, na.rm=T)[,2]

data$compositions<-as.factor(data$compositions)

pool<-c("MimTen","HanImp","AspPyr","ComLep","PipSti","AmbCea","CynFle","CombLep",
        "LibFer","BauChe","PoiGar","PseMar","CocVit","ZizJoa","AnaCol","SebMac")


#Observed facilitation index of the species planted in the experiment
sppFaci<-c(0.130,0.101,0.187,0.057,0.058,0.068,0.060,0.060,0.033,
           0.047,0.022,0.006,-0.001,-0.009,-0.063,-0.069)
names(sppFaci)<-pool

library(lme4)
library(lmerTest)


#Analyses using Potential Plot Facilitation as additional covariate
#Full model with facilitation
m1 <- lmer(cresc.altura ~ log(richness+1) + facilitation + (1|compositions), data=data)
summary(m1)
anova(m1)
plot(m1)

coefs<-fixef(m1)

windows(200,100)
par(mfrow=c(1,2))
plot(cresc.altura ~ richness,data, log="x", frame=F,ylab="Height relative growth" ,xlab="Richness", xaxt="n", pch=16, col=as.factor(data$richness))
curve(coefs[1]+coefs[2]*log(x),add=T, lwd=2)
axis(1,at=c(1,2,4,8,16))
plot(cresc.altura ~ facilitation,data, frame=F,ylab="", xlab="Facilitation", col=as.factor(data$richness), pch=16)
curve(coefs[1]+coefs[3]*x,add=T, lwd=2)



#Analyses using species identity
#Full model
f1_identity<-formula(paste("scale(cresc.altura) ~ 0+",paste(colnames(sppMatrix), collapse=" + "), "+ (1|compositions)"))

m1_identity <- lmer(f1_identity , data=data)
summary(m1_identity, correlation=T)
plot(m1_identity)

#Extract individual species effect of functioning
sppEffect <- fixef(m1_identity)

sppFaci2<-sppFaci[names(sppFaci) %in% names(sppEffect)]

#Correlation between species effect on ecosystem functioning and species facilitation index
summary(lm(sppEffect ~ sppFaci2))

windows()
plot(sppEffect ~ sppFaci2, xlim=c(-0.1,0.2), type="n")
text(sppFaci2,sppEffect,names(sppEffect), cex=0.8)
