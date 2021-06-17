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
install.packages("funModeling")
library(funModeling)



#Cargamos el dataset
#dos aca cada uno pone si ingreso de la data
#Cadauno usa su directorio
#setwd("C:/Users/vieraa/Documents/GitHub/RA_final" )   #establezco la carpeta donde voy a trabajar
#setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
#setwd("C:/Users/jgiberti/Documents/Univ. Austral - Maestria en Ciencias de Datos/10. Regresion Avanzada/TP/" )   #establezco la carpeta donde voy a trabajar
setwd("C:/Users/isant/OneDrive/Desktop/MCD/Regresión Avanzada/TP Final")

data <- fread("clinton.txt")
View(data)

#creo un dataset sacando los estados; me pregunto, vale la pena hacer un one hot encoding con los estados?
# IRU -> A mi me parece que, si Brooklyn está siendo una observación influyente (NO outlier), quizás tenga sentido.
# De todas maneras, habría que ver cómo performan ambos modelos y quedarnos con el mejor. 
# Podemos plantear ambos. 
data1<- dplyr::select(data, pje:crimen)


#EDA? es necesario? o arrancamos con los modelos
# JOR -> Si, creo que es necesario hacer una introd de los que se va a analizar
# IRU -> Para conocimiento nuestro, no vamos a tener espacio para incluírlo (algo super breve)
#EDA
summary(data1)
glimpse(data1)
print(status(data1))
freq(data1) 
print(profiling_num(data1))
plot_num(data1)
describe(data1)

### Jor -> 1. ANALISIS SOBRE LA RELACION ENTRE LAS VARIABLES####

grafico1<-ggpairs(data1, title="correlogram with ggpairs()") 
grafico2<-ggcorr(data1, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")

grafico1
grafico2


#JOR -> Grafico 1 De esta matriz de correlación puede observarse que las correlaciones mas altas se deben a pobreza e ingpc (0.617) pobreza y pje (0.501), veteranos y edad (0.526), ancianos y edad (0.48), crimen y densidad (0.405)
#correlacion inversa entre ingpc y pobreza

# IRU -> Yo lo comentaría pero en principio no me parecen demasiado preocupantes, 
# no son tan altas, en todo caso, cuando analicemos modelo, vemos con cuál nos quedamos.


library(psych)
multi.hist(x = data1, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = "")

#Jor -> hay muchas variables que no parecen tener una distrib normal, y son altamente sesgadas

#Jor -> del examen visual de la matriz de correlacion y los histogramas capaz que variables
#como "ahorros", "ingp", "pobreza", "mujeres", "ancianos" y "crimen" muestran una distribución exponencial????
#habira que probar si con una transformación logarítmica posiblemente haría más normal su distribución.

# IRU -> Podemos ver también de usar Ridge o Lasso?


### Jor --> 2. GENERACION DEL MODELO###

#creo modelo de regresion multiple sin modificaciones
mod1<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data )
summary(mod1)

#JOR -> Coef determinacion muy chico, casi no hay relacion de las variables explicat con la variable respuesta
#Algunos coef del modelo no son significativos

# IRU -> Lasso nos podría servir porque también elimina las variables que no están correlacionadas.

#vemos los datos

plot(mod1)

# el dato 1602 esta completamente desubicado

# IRU -> El gráfico QQ da distribución con outliers. OJO con el 1602 porque quizás es influyente y no outlier
# y puede requerir otro tratamiento. 

#JOR ->
shapiro.test(mod1$residuals)
# el test de hipótesis no confirma la normalidad.
#Es necesario SI O SI Aplicar alguna transformacion a las variables y despues vemos el resto


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

#mÃ©todo forward

library(MASS)
data(data)

stepAIC(
  object = lm(pje ~ 1, data = data), #punto de partida
  scope = list(upper = lm(pje ~ ., data = data)), #mÃ¡ximo modelo posible
  direction = "forward", #mÃ©todo de selecciÃ³n
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

#TÃ©cnicas paso a paso
mod_forward <- regsubsets(pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data, method = "forward")
mod_backward <- regsubsets(pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data, method = "backward")
mod_seqrep <- regsubsets (pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data, method = "seqrep")


######################################################


#Jor -> stepwise mixto. 


#El valor matemático empleado para determinar la calidad del modelo va a ser Akaike(AIC).

step(object = mod1, direction = "both", trace = 1)

mod2<-lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
           densidad + ancianos + crimen, data = data)

summary(mod2)
#DA HORRIBLE EL MODELO, POR ENDE HAY QUE VER PREVIAMENTE QUE HACEMOS CON ESA OBSERVACION


########################################################

####lleguÃ© hasta acÃ¡!!!!!!!!

#Criterios de selecciÃ³n de modelos
resumen$cp
resumen$adjr2
resumen$bic

#GrÃ¡ficos para comparar ajustes
plot(mejorsub, scale = "Cp")
plot(mejorsub, scale = "adjr2")
plot(mejorsub, scale = "bic")

#TÃ©cnicas paso a paso
regsubsets(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, method = "forward")
regsubsets(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, method = "backward")
regsubsets(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width, iris, method = "seqrep")0
