library(dplyr)
library(reshape2)

BD <-read.csv("data/BD_17-18.csv")
BD <- arrange(BD, ano, plot, linha, planta)
str(BD)
#BD <- filter(BD, div != "ctrl") 

                                      #TABELAS COM VALORES DE CRESCIMENTO PARA CADA PLANTA 
# Um objeto para cada ano
cr  <- select(BD, ano, linha, plot, div, planta, especie, status, altura, n_fol, LA, biomassa)
a17 <- filter(cr, ano == "2017"); arrange(a17, ano, plot, linha, planta)
a18 <- filter(cr, ano == "2018"); arrange(a18, ano, plot, linha, planta)

# Verificar se as plantas estão alinhadas
summary(a17$planta == a18$planta)

#colocando as planilhas paralelas.
brazilD1<- cbind(a17, a18)

colnames(brazilD1) <- c("ano","linha","plot","div","planta","especie","status","altura","n_fol","LA","biomassa",
                    "ano1","linha1","plot1","div1","planta1","especie1","status1","altura1","n_fol1","LA1","biomassa1")

#Excluindo as plantas que morreram em 2018, porque foram substituidas por um novo indivíduo. 
brazilD <- filter(brazilD1, status1=="V")
brazilD$n_fol1[is.na(brazilD$n_fol1)] <- 0 #colocando 0 em vez de NA em 2018, porque NA foram as plantas que morreram e perderam toda sua biomassa. Quando a célula = NA, o R ignora o cálulo, quando 0 estamos penalizando a perda da biomassa

brazilD$n_fol <-brazilD$n_fol +1
brazilD$n_fol1 <-brazilD$n_fol1 +1

#Crescimento relativo  entre tempo 0 e tempo 1 (2018) altura em metros
brazilD$cresc.altura <- (brazilD$altura1 - brazilD$altura)/ brazilD$altura
  
#Crescimento em biomassa em g
brazilD$c.biomassa <- (brazilD$n_fol* brazilD$biomassa)
brazilD$c.biomassa1 <- (brazilD$n_fol1* brazilD$biomassa1)

#Crescimento relativo entre tempo 0 e tempo 1 em biomassa foliar
brazilD$cresc.biomassa <- (brazilD$c.biomassa1 - brazilD$c.biomassa)/brazilD$c.biomassa
 
#Crescimento relativo entre tempo 0 e tempo 1 número de folhas
brazilD$cresc.nfol <- (brazilD$n_fol1 - brazilD$n_fol)/brazilD$n_fol

write.csv(brazilD, "BDcrescimento_por_individuo.csv")
                                                      
                             #TABELA COM VALORES POR PLOT  
#CRESCIMENTO
# Média de crescimento por parcela
cresc1 <- select(brazilD, plot, div, cresc.altura, cresc.nfol,  cresc.biomassa)
cresc1$plot <- as.factor(cresc1$plot)

cresc <-  cresc1 %>% group_by(plot, div) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))


#write.csv(cresc, "BDcrescimento_por_plot.csv")

# Média de crescimento por espécie por parcela
cresc2 <- select(brazilD, plot, especie, div, cresc.altura, cresc.nfol,  cresc.biomassa)
cresc2$plot <- as.factor(cresc2$plot)

cresc2 <-  cresc2 %>% group_by(plot, div, especie) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))


#write.csv(cresc2, "BDcrescimento_por_sp_por_plot.csv")

  #FACILITAÇÃO
#média de facilitação por parcela. 
facilitação <- select(a17, plot, div, fac_cresc, fac_sob, fac_tot)

fac <-  facilitação %>% group_by(plot, div) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

#SOBREVIVÊNCIA
#Calculo da sobrevivência em 2 anos.
#Registro de moratalidade em dois anos. As plantas mortas em 2017, permanecem como mortas em 2018. 
##Porcentagem de sobrevivêmcia da parcela. 

mor  <- select(BD, ano, linha, plot, div, planta, especie, status)
m17 <- filter(mor, ano == "2017"); arrange(m17, ano, plot, linha, planta)
m18 <- filter(mor, ano == "2018"); arrange(m18, ano, plot, linha, planta)

mortalidade <- cbind(m17, m18)

# Verificar se as plantas estão alinhadas
summary(m17$planta == m18$planta)

colnames(mortalidade) <- c("ano","linha","plot","div","planta","especie","status",
                       "ano1","linha1","plot1","div1","planta1","especie1","status1")

#Criando uma nova coluna com o cálculo de mortalidade única, considerando 2017 e 2018
newdata <- mutate(mortalidade, vm = ifelse(c(status1 == "V" & status =="V" ), "V", "M"))

#Porcentagem de sobrevivencia por espécie por parcela. 
mortas <- with(newdata,aggregate(newdata[,"vm"],list(plot,div,especie),table))
mortas <- data.frame(mortas[,1:3],mortas[,4][,1],mortas[,4][,2])
colnames(mortas)<-c("plot","div","especie","M","V")

mortas <- arrange(mortas, plot, div, especie, M, V)
mortas$total <- (mortas$V + mortas$M)
mortas$sob_porc_especie <- (mortas$total - mortas$M)/mortas$total
write.csv(mortas, "sobrev_epecieporparcela.csv")

#Porcentagem de sobrevivência por espécie no experimento todo
sobrev_especies <- summarise(group_by(mortas, especie),
                             mortas=sum(M),
                             vivas=sum(V),
                             total=sum(total))
sobrev_especies$sobrev_porc <- (sobrev_especies$total - sobrev_especies$mortas)/sobrev_especies$total
write.csv(sobrev_especies, "sobrev_experimento.csv")

#Porcentagem de sobrevivencia por espécie geral em todo experimento. 
mortast <-with(newdata,aggregate(newdata[,"vm"],table))
mortast <-data.frame(mortast[,1],mortast[,2][,1],mortast[,2][,2])
colnames(mortast)<- c("ano", "M", "V")

#Porcentagem de sobrevivencia por  parcela. 
mortasp <- with(newdata,aggregate(newdata[,"vm"],list(plot,div),table))
mortasp <- data.frame(mortasp[,1:2],mortasp[,3][,1],mortasp[,3][,2])
colnames(mortasp)<-c("plot","div","M","V")
mortasp<- arrange(mortasp, plot, div, M, V)
mortasp$total <- (mortasp$V + mortasp$M)
mortasp$sob_porc_parcela <- (mortasp$total - mortasp$M)/mortasp$total
write.csv(mortasp, "sobrev_por_parcela.csv")


