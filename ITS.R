#Direcionado o R para o Diretorio a ser trabalhado
setwd('C:/Users/User/Desktop')

#Limpa o Ambiente Global
rm(list=ls())

#Inicio do Script
#Analise de Regressão com o R


#Pacotes a serem utilizados

library(kableExtra)
library(urca)
library(foreign)
library(readxl)
library(mFilter)
library(forecast)
library(dplyr)
library(tsutils)
library(xts)
library(ggthemes)
library(FinTS)
library(scales)
library(quantmod)
library(patchwork)#unir graficos ggplot
library(httr)
library(jsonlite)
library(optimr)
library(deSolve)
library(tidyverse)
library(lubridate)
library("growthrates")
library(growthmodels)
library(ggplot2)
library(greybox)##pacote para função MIS
library(nlme)
library(car)
library(ggpubr)
library(tidyquant)
library(dotwhisker)
library(splines2)
library(foreign)
library(vcd)
library(tsModel)
library(Epi)
library(readr)
library(lmtest)
library(outliers)
library(pillar)
library(psych)
library(QuantPsyc)
library(scatterplot3d)
library(rstatix)
library(AER)
library(stargazer)
library(robustbase)
library(gmm)
library(ggpmisc)
library(dlookr)
library(corrplot)
library(tseries)
library(quantmod)
library(gridExtra)
library(dygraphs)
library(zoo)#Para calcular a média móvel
library(greybox)
library(magrittr)# pipe operations
library(DT)
library(gtrendsR)
library(pander)

#Entrando dados
dados <- read_excel("C:/Users/User/OneDrive/Área de Trabalho/dados19.xlsx")
attach(dados)
View(dados)

plot( dados$dia, dados$obitosdia,
      bty="n", pch=19, col="gray",
      ylim = c(0, 200), xlim=c(0,950),
      xlab = "Time (days)", 
      ylab = "Obitos diários" )

# Line marking the interruption
abline( v=44, col="firebrick", lty=2 )
text( 44, 60, "Começo do efeito", col="firebrick", cex=1.3, pos=4 )

# Add the regression line
ts <- lm( obitosdia ~ dia + lockdown, data=dados )
lines( dados$dia, ts$fitted.values, col="steelblue", lwd=2 )

regTS = lm ( obitosdia ~ dia + lockdown)  # Our time series model

stargazer( regTS, 
           type = "html", 
           dep.var.labels = ("obitosdia"),
           column.labels = ("Resultado ITS"),
           covariate.labels = c("Tempo", "Intervenção"), 
           omit.stat = "all", 
           digits = 2 )
regTS

#PLOTANDO OS RESULTADOS PREVISTOS

# We create a small dataset with the new values
data1 <- as.data.frame( cbind( dia = 44, lockdown = 1)) 

# We use the function predict to (1) take the 
#  coefficients estimated in regTS and 
#  (2) calculate the outcome Y based on the 
#  values we set in the new datset
y1 <- predict( regTS, data1 ) 

# We plot our initial observations, the column Y in our dataset
plot( obitosdia,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 950), 
      ylim = c(0, 200),
      xlab = "Time (days)", 
      ylab = "Obitos diários")

# We add a point showing the level of lockdown at time = 44)
points( 44, y1, col = "dodgerblue4", 
        pch = 19, bg = "dodgerblue4", cex = 2 )
text( 44, y1, labels = "t = 44", pos = 4, cex = 1 )

# Line marking the interruption
abline( v=43, col="red", lty=2 )



#EFEITO APÓS 15 DIAS

data2 <- as.data.frame( cbind( dia = 59, lockdown = 1)) # New data

y2 <- predict( regTS, data2 ) # We predict the new outcome

# We plot our initial observations, the column Y in our dataset
plot( obitosdia,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 950), 
      ylim = c(0, 200),
      xlab = "Time (days)", 
      ylab = "Óbitos diários")

# We add a point showing the level of lockdown at time = 44
points(44, y1, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a new point showing the level of lockdown at time = 74)
points(59, y2, col = "pink", pch = 19, bg = "red", cex = 2)

# Label for our predicted outcome
text(44, y1, labels = "t = 44", pos = 2, cex = 1)

#Label for the counterfactual 
text(59, y2, labels = "t = 59", pos = 4, cex = 1)

# Line marking the interruption
abline( v=43, col="red", lty=2 )


#Cenário contrafactual

data3 <- as.data.frame(cbind( dia= 59, lockdown = 0)) # Data if the intervention does not occur

y3 <- predict(regTS, data3) #Counterfactual

# We plot our initial observations, the column Y in our dataset
plot( obitosdia,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 950), 
      ylim = c(0, 200),
      xlab = "Time (days)", 
      ylab = "Óbitos diários")

# We add a  point showing the level of lockdown at time = 59
points(59, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# We add a point indicating the counterfactual
points(59, y3, col = "darkorange2", pch = 19, cex = 2)

# Label for our predicted outcome
text(59, y2, labels = "t = 59", pos = 2, cex = 1)

#Label for the counterfactual 
text(59, y3, labels = "t = 59", pos = 4, cex = 1)

# Line marking the interruption
abline( v=46, col="red", lty=2 )

data4 <- as.data.frame(cbind( dia = 916, lockdown = 1)) 
data5 <- as.data.frame(cbind( dia = 916, lockdown = 0))

y4 <- predict(regTS, data4)
y5 <- predict(regTS, data5)

# We plot our initial observations, the column Y in our dataset
plot( obitosdia,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 916), 
      ylim = c(0, 200),
      xlab = "Time (days)", 
      ylab = "Obitos diários")

# lockdown at time = 74
points(74, y2, col = "dodgerblue4", pch = 19, bg = "red", cex = 2)

# Counterfactual at time = 74
points(74, y3, col = "darkorange2", pch = 19, cex = 2)

# lockdown at time = 450
points(470, y4, col = "dodgerblue4", pch = 19, cex = 2)

# Counterfactual at time =  450
points(470, y5, col = "darkorange2", pch = 19, cex = 2)

# Adding labels
text(470, y4, labels = "Y at t = 403", pos = 4, cex = 1)
text(470, y5, labels = "C at t = 403", pos = 4, cex = 1)
text(74, y2, labels = "Y at at = 74", pos = 4, cex = 1)
text(74, y3, labels = "C at t = 74", pos = 4, cex = 1)

# Line marking the interruption
abline( v=43, col="red", lty=2 )

pred1 <- predict(regTS, dados) 
# To estimate all predicted values of Y, we just use our dataset

datanew <- as.data.frame(cbind(dia = rep(1 : 916), lockdown = rep(0))) 
# Create a new dataset where Treatment and Time Since Treatment are equal to 0 as the intervention did not occur.

pred2 <- predict(regTS, datanew) 
# Predict the counterfactuals

plot( obitosdia,
      bty="n",
      col = gray(0.5,0.5), pch=19,
      xlim = c(1, 950), 
      ylim = c(0, 200),
      xlab = "Time (days)", 
      ylab = "OBitos diários")

lines( rep(1:42), pred1[1:42], col="dodgerblue4", lwd = 3 )
lines( rep(44:916), pred1[44:916], col="dodgerblue4", lwd = 3 )
lines( rep(43:916), pred2[43:916], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 55, labels = "Previstos", pos = 2, cex = 1, col = "dodgerblue3")
text(800, 1, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=43, col="darkorange2", lty=2 )

#EFeitos retardados

reg2 <- lm(obitosdia ~ dia + lockdown, data = dados)

pred1 <- predict(reg2, dados) 

datanew <- as.data.frame(cbind(dia = rep(1 : 916), lockdown = rep(0))) 

pred2 <- predict(reg2, datanew) 

plot(obitosdia,
     col = "gray",
     xlim = c(1, 950), 
     ylim = c(0, 200),
     xlab = "Time (days)", 
     ylab = "Obitos diários")

lines( rep(1:42), pred1[1:42], col="dodgerblue4", lwd = 3 )
lines( rep(44:916), pred1[44:916], col="dodgerblue4", lwd = 3 )
lines( rep(43:916), pred2[43:916], col="darkorange2", lwd = 3, lty = 5 ) 

text(0, 45, labels = "Predicted values", pos = 4, cex = 1, col = "dodgerblue3")
text(800, 1, labels = "Counterfactual", pos = 4, cex = 1, col = "darkorange2")

# Line marking the interruption
abline( v=43, col="red", lty=2 )

# Line at t = 400
abline( v=400, col="forestgreen", lty=2 )

