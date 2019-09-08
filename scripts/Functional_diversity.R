library(dplyr)
comm <-read.csv("matriz_pres_aus.csv")
rownames(comm)<-comm[,1]# Esse comando estabelece o nome das linhas da prmeira coluna como nome das linhas
comm<-comm[,-1]

# Conferindo a matriz de comunidades

#fix(comm)

# Matriz de atributos

traits<-read.csv("trait_mean_bd.csv")
traits<- arrange(traits, id )
traits <-traits[,-1]
rownames(traits)<-traits[,1]
traits<-traits[,-1]

# Conferindo a matriz de atributos

#fix(traits)

colnames(traits)

## Conferindo se nomes das espécies estão iguais entre matrizes de comunidades e de atributos

colnames(comm)
rownames(traits)

summary(colnames(comm) == rownames(traits)) #Testa se nomes são iguais e estão na mesma ordem

#summary(colnames(comm) %in% rownames(traits_ok)) #Testa se nomes são iguais independentemente da sua ordem


## Gerando a matriz de distâncias (Pavoine et al. 2009, Oikos)
library(ade4) 
?ade4 #Ajuda da função. Em "index", olhar pacotes que estão sendo usados para entender o que estão fazendo.

colnames(traits)

ktab<-ktab.list.df(list(traits)) #Junta grupos de variáveis

dist.func <-dist.ktab(ktab,type=c("Q"),option=c("scaledBYrange")) #Indica tipos de variáveis; quando há muitos tipos de variáveis, a padronização pela amplitude é a mais indicada.

as.matrix(dist.func)[1:16,1:16]

is.euclid(dist.func)

kplot(dist.func)

## Calculando a diversidade funcional "alfa"

## Índice FD de Petchey & Gaston (Petchey & Gaston 2002, Ecology Letters):

# Produzindo um dendrograma funcional
den.func<-hclust(dist.func,"average") #average quer dizer método de ligação UPGMA

# Plotando o dendrograma funcional
par(mai=c(0.5,1,0.5,1), cex=0.5)
plot(den.func, cex=0.6)
class(den.func)

# Convertendo o dendrograma em um objeto "phylo"
library(ape)
phylo.den<-as.phylo(den.func)
par(mfrow=c(1,1), mai=c(0,1,0,1))
plot(phylo.den, cex=0.3)

dev.off() #Para "desligar" o "par" no plot

library(picante)

FD <-pd(comm,phylo.den) #Para FD de Petchey & Gaston variar de 0 a 1, criar uma comunidade com todas as spp, i.e. com FD total e padronizar cada valor pela diversidade máxima
FD #Dataframe com valores de FD (PD) e riqueza de espécies (SR) para todas as comunidades
FD$plot <- rownames(FD)

plot(FD$SR, FD$PD)

#write.csv(FD,"FD.csv") #Salvar resultado em planilha Excel (.csv)
