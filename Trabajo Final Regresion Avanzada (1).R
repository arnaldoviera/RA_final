#arrancamos en R, despues lo pasamos a RMarkdown

rm( list=ls() )
gc()


library(readr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(alr4)
library(ISLR)
library(GGally)
library(qpcR)
library(UsingR)
library(data.table)


#Cargamos el dataset
#dos aca cada uno pone si ingreso de la data

setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
data <- fread("clinton.txt")
View(data)
summary(data)

data1<- dplyr::select(data, pje:crimen)

grafico1<-ggpairs(data1, title="correlogram with ggpairs()") 
grafico2<-ggcorr(data1, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")

mod1<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data )
summary(mod1)

#analisis de residuos
PRESS(mod1)$residuals

plot(mod1)

