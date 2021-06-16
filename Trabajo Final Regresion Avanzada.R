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
library(funModeling)



#Cargamos el dataset
#dos aca cada uno pone si ingreso de la data
#Cadauno usa su directorio
#setwd("C:/Users/vieraa/Documents/GitHub/RA_final" )   #establezco la carpeta donde voy a trabajar
setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
data <- fread("clinton.txt")
View(data)
#creo un dataset sacando los estados; me pregunto, vale la pena hacer un one hot encoding con los estados?
data1<- dplyr::select(data, pje:crimen)


#EDA? es necesario? o arrancamos con los modelos
#EDA
summary(data1)
glimpse(data1)
print(status(data1))
freq(data1) 
print(profiling_num(data1))
plot_num(data1)
describe(data1)


grafico1<-ggpairs(data1, title="correlogram with ggpairs()") 
grafico2<-ggcorr(data1, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")

grafico1
grafico2

#correlacion inversa entre ingpc y pobreza

#creo modelo de regresion multiple sin modificaciones
mod1<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data )
summary(mod1)

#vemos los datos
plot(mod1)
#el dato 1602 esta completamente desubicado






criterios <- function(mmod1, maxi) {
  
  #analisis de residuos
  PRESS(mod1)$residuals

  
  SCE <- deviance(mod1)
  n <- length(residuals(mod1))
  p <- length(coefficients(mod1)) #incluye intercepto
  
  #CME
  cme <- SCE/(n-p) #igual al cuadrado de la residual SE
  
  #R2 ajustado
  r2aj <- 1 - ((n-1)/(n-p)) * (1-summary(mod1)$r.squared)
  
  summary(mod1)$adj.r.squared
  yi <- iris$Petal.Length
  1 - (deviance(mod1) / (n-p)) / ((sum((yi - mean(yi))^2))/(n-1))
  
  #PRESS
  estpress <- sum((residuals(mod1)/(1-hatvalues(mod1)))^2) #qpcR::PRESS(m)
  
  #Mallows CP
  mcp <- SCE/(deviance(maxi)/(n-length(coefficients(maxi)))) + 2*p - n
  #olsrr::ols_mallows_cp(m, maxi)
  
  #AKAIKE
  akaike <- n * log(SCE/n) + 2*p #extractAIC(m)[2]
  
  #BIC
  schwarz <- n * log(SCE/n) + p*log(n) #extractAIC(m, k = log(n))[2]
  
  tibble(CME = cme, R2Aj = r2aj, PRESS = estpress, Cp = mcp, AIC = akaike, BIC = schwarz)
  
}

#método forward

library(MASS)
data(data)

stepAIC(
  object = lm(pje ~ 1, data = data), #punto de partida
  scope = list(upper = lm(pje ~ ., data = data)), #máximo modelo posible
  direction = "forward", #método de selección
  trace = FALSE #para no imprimir resultados parciales
)

#mejor subconjunto
library(leaps)

mejorsub <- regsubsets(
  x = pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, 
  data = data
)

mejorsub_print <- summary(mejorsub)
mejorsub_print

#Técnicas paso a paso
mod_forward <- regsubsets(pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data, method = "forward")
mod_backward <- regsubsets(pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data, method = "backward")
mod_seqrep <- regsubsets (pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data, method = "seqrep")





####llegué hasta acá!!!!!!!!

#Criterios de selección de modelos
resumen$cp
resumen$adjr2
resumen$bic

#Gráficos para comparar ajustes
plot(mejorsub, scale = "Cp")
plot(mejorsub, scale = "adjr2")
plot(mejorsub, scale = "bic")

#Técnicas paso a paso
regsubsets(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, method = "forward")
regsubsets(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, method = "backward")
regsubsets(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, method = "seqrep")
