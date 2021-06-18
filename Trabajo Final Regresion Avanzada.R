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
#setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
setwd("C:/Users/jgiberti/Documents/Univ. Austral - Maestria en Ciencias de Datos/10. Regresion Avanzada/TP/" )   
#setwd("C:/Users/isant/OneDrive/Desktop/MCD/Regresión Avanzada/TP Final")

data <- fread("clinton.txt")

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
freq(data1) # Arnaldo -> freq no arroja nada en concreto, qué queremos ver?
print(profiling_num(data1))
plot_num(data1)
describe(data1)

### Jor -> 1. ANALISIS SOBRE LA RELACION ENTRE LAS VARIABLES####

grafico1<-ggpairs(data1, title="correlogram with ggpairs()") 
grafico1

grafico2<-ggcorr(data1, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")
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



pairs(data1)
#Analisis de correlacion
cor(data1)

# Mostramos visualmente la relaci?n de la variable dependiente con cada una de las variables independientes
par(mfrow = c(2, 2))

plot(data1$pje,data1$edad)
plot(data1$pje,data1$ahorros)
plot(data1$pje,data1$ingpc)
plot(data1$pje,data1$pobreza)
plot(data1$pje,data1$veteranos)
plot(data1$pje,data1$mujeres)
plot(data1$pje,data1$densidad)
plot(data1$pje,data1$ancianos)
plot(data1$pje,data1$crimen)

par(mfrow = c(1, 1))


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


#Jor -->Analisis de residuos


rstandard(mod1)
rstudent(mod1)
hatvalues(mod1)
library(qpcR)
PRESS(mod1)$residuals

plot(x,y)
barplot(PRESS(mod1)$residuals)
plot(PRESS(mod1)$residuals, type="l")


#Jor --> Supuesto de normalidad

library(UsingR)
residuos <- residuals(mod1)

#JOR ->
shapiro.test(mod1$residuals)

# el test de hipótesis no confirma la normalidad.
#Es necesario SI O SI Aplicar alguna transformacion a las variables y despues vemos el resto



#Jor --> Transf BoxCox
mod1_boxcox <- boxcox(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data)

mod1$edad[which.max(mod1$pje)] 

mod1$ahorros[which.max(mod1$pje)] 

mod1$ingpc[which.max(mod1$pje)] 

mod1$pobreza[which.max(mod1$pje)] 

mod1$veteranos[which.max(mod1$pje)] 

mod1$mujeres[which.max(mod1$pje)] 

mod1$densidad[which.max(mod1$pje)] 

mod1$ancianos[which.max(mod1$pje)] 

mod1$crimen[which.max(mod1$pje)] 

#Todos me dieron 0..Lo que indicaria que habria que transformar la variable

data$logpje <- log(data$pje) * exp(mean(log(data$pje))) #log X media geom


#Jor -- Distancia de Cook

cooks.distance(mod1)
#Jor --> Todos los puntos son mayores a 1, entonces son puntos influyentes??

influence.measures(mod1)
# Jor -> hay que analizar la tabla que tira



# Jor _> Seleccion del modelo
#Colinealidad entre las variables. ¿Existe? ¿Tenemos que hacer algo?
# por lo que se visuaaliza en la matriz de corr la correlacion es baja..
#no hay indicio de colinealidad


#Jor --> Verifico con VIF
vif(lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data ))


#edad      ahorros     ingpc   pobreza   veteranos   mujeres   densidad  ancianos   crimen
#1.733274  1.610381  2.428866  2.057863  1.551142    1.177141  1.304469  1.611870    1.527998

#Jor --> no hay VIF mayores a 5, o sea no habria problema con la colinealidad




#Jor --> metodo forward

library(MASS)
data(data1)

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

#Jor --> Regresion Ridge
library(MASS)
lm.ridge(
  formula = pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, 
  data = data1, 
  lambda = seq(0, 1, by = 0.1)
)

#############################################################


######################################################

#Jor --> Regresion Lasso
library(glmnet)
#Armar

#############################################################



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
