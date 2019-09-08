library(mctest)
library(dplyr)

cm <- read.csv("data/CWM_plots.csv")

cmw <- cm[,-c(3)] %>% group_by(plot, div) %>% 
  summarise_all(funs(mean(., na.rm = TRUE)))

cwm<- as.matrix(cmw[,-c(1,2)])
eigprop(cwm, prop = 0.7)

#teste de multicolinearidade 
######################################################################################
# Função para plotar correlação na parte de baixo da matriz e p na parte de cima 
#(https://stat.ethz.ch/pipermail/r-help/2000-January/009758.html)

cor.prob <- function(X, dfr = nrow(X) - 2) {
  R <- cor(X)
  above <- row(R) < col(R)
  r2 <- R[above]^2
  Fstat <- r2 * dfr / (1 - r2)
  R[above] <- 1 - pf(Fstat, 1, dfr)
  R
}

cor <- cor.prob(cwm)

cor <- as.data.frame(cor)
names(cor)

#Plot bonitinho de grafico em baixo e cor em cima
library(psych)
pairs.panels(cwm, 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = F,
             smooth=T,
             lm = F, cex.cor=3,
             cex=1
)

pairs(adcor, upper.panel= panel.cor)

cor.test(q1$t_capac_arm_casca, q1$t_dens)
