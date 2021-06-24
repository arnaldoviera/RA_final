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
library(scales)
library(dummies)
library(kableExtra)



#Cargamos el dataset
#dos aca cada uno pone si ingreso de la data
#Cadauno usa su directorio
setwd("C:/Users/vieraa/Documents/GitHub/RA_final" )   #establezco la carpeta donde voy a trabajar
#setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
#setwd("C:/Users/jgiberti/Documents/Univ. Austral - Maestria en Ciencias de Datos/10. Regresion Avanzada/TP/" )   
#setwd("C:/Users/isant/OneDrive/Desktop/MCD/Regresión Avanzada/TP Final")

data <- fread("clinton.txt")

#creo un dataset sacando los estados; me pregunto, vale la pena hacer un one hot encoding con los estados?
# IRU -> A mi me parece que, si Brooklyn está siendo una observación influyente (NO outlier), quizás tenga sentido.
# De todas maneras, habría que ver cómo performan ambos modelos y quedarnos con el mejor. 
# Podemos plantear ambos. 
# dejo la consigna: "A través de un modelo de Regresión Lineal Múltiple ajustado por Mínimos Cuadrados Ordinarios,
#estudiar el porcentaje de votos obtenidos por el candidato Bill Clinton en cada uno de los condados
#estadounidenses. Pueden incorporar como explicativas a cualquiera de las restantes variables presentes en
#la base"

#logaritmo

Dlog <- data
Dlog$pje <- log(Dlog$pje)
Dlog$edad <- log(Dlog$edad)
Dlog$ahorros <- log(Dlog$ahorros)
Dlog$ingpc <- log(Dlog$ingpc)
Dlog$pobreza <- log(Dlog$pobreza)
Dlog$veteranos <- log(Dlog$veteranos)
Dlog$densidad <- log(Dlog$densidad)
Dlog$ancianos <- log(Dlog$ancianos)
Dlog$crimen <- log(Dlog$crimen)
Dlog$mujeres <- log(Dlog$mujeres)






DS <- data
DS$pje <- rescale(DS$pje)
DS$edad <- rescale(DS$edad)
DS$ahorros <- rescale(DS$ahorros)
DS$ingpc <- rescale(DS$ingpc)
DS$pobreza <- rescale(DS$pobreza)
DS$veteranos <- rescale(DS$veteranos)
DS$densidad <- rescale(DS$densidad)
DS$ancianos <- rescale(DS$ancianos)
DS$crimen <- rescale(DS$crimen)
DS$mujeres <- rescale(DS$mujeres)

##Arnaldo DS Dataset-Reescalado

summary(DS)



data1<- dplyr::select(data, pje:crimen)
data1_DS <- dplyr::select(DS, pje:crimen)
data1_Dlog <- dplyr::select(Dlog, pje:crimen)


## Pasé estados a datos dummies
data_dummy <- data
data_dummy <- cbind(data, dummy(data$estado, sep ="_"))


#EDA? es necesario? o arrancamos con los modelos
# JOR -> Si, creo que es necesario hacer una introd de los que se va a analizar
# IRU -> Para conocimiento nuestro, no vamos a tener espacio para incluírlo (algo super breve)
# Arnaldo -> va eda a matar, les encanta. 

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
#correlacion inversa entre ingpc y pobreza ##Arnaldo => es correcto que sea inverso

# IRU -> Yo lo comentaría pero en principio no me parecen demasiado preocupantes, 
# no son tan altas, en todo caso, cuando analicemos modelo, vemos con cuál nos quedamos.



library(psych)
multi.hist(x = data1, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = NULL)

multi.hist(x = data1_DS, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = NULL)

multi.hist(x = data1_Dlog, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = NULL)


#Jor -> hay muchas variables que no parecen tener una distrib normal, y son altamente sesgadas

#Jor -> del examen visual de la matriz de correlacion y los histogramas capaz que variables
#como "ahorros", "ingp", "pobreza", "mujeres", "ancianos" y "crimen" muestran una distribución exponencial????
#habira que probar si con una transformación logarítmica posiblemente haría más normal su distribución.

# IRU -> Podemos ver también de usar Ridge o Lasso?
#agregué las variables: Arnaldo

### Jor --> 2. GENERACION DEL MODELO###

#creo modelo de regresion multiple sin modificaciones
mod1<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data )

summary(mod1)

#FE
mod11<-lm(pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen + 
           ingpc:pobreza + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
           ingpc:ahorros + ahorros:edad + crimen:ancianos + veteranos:ahorros, data=data)

summary(mod11)

mod12<-lm(pje ~ ingpc + pobreza + mujeres + densidad + ancianos + 
            ingpc:pobreza + ancianos:ahorros + ingpc:ahorros + ahorros:edad, data=data)

summary(mod12)

## IDEM mod1 pero escalado entre 0 y 1
mod1_scale<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data1_DS)

summary(mod1_scale)

## IDEM mod1 pero en logaritmo
mod1_log<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data1_Dlog)

summary(mod1_log)

## IDEM mod1 pero estado pasado a dummi###### Adjusted R-squared:  0.560
mod1_dummystate <-lm(pje ~ . , data=data_dummy) 

summary(mod1_dummystate)

# Ninguno consigue un ajuste muy bueno

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

plot(x,y) ##no funciona Arnaldo
barplot(PRESS(mod1)$residuals)
plot(PRESS(mod1)$residuals, type="l")


#Jor --> Supuesto de normalidad

library(UsingR)

residuos <- residuals(mod1)
plot(residuos)

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




###########################################################################
# PROPUESTA MEJORADA
###########################################################################
# ANALISIS EXPLORATORIO

summary(data1)

library(tidyverse)
data1 %>%
  ggplot(aes(edad, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Edad", y = "PJE")

data1 %>%
  ggplot(aes(ahorros, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Ahorros", y = "PJE")

# pruebo con el log de la edad???

data1 %>%
  ggplot(aes(log(edad), pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_continuous(trans = "log")+
  labs(x = "Edad, log", y = "PJE")

###########################################################################
#Ajustamos un modelo de regresi?n lineal simple mediante MCO y obtenemos los siguientes resultados:
  
mod1_mco<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data )


summary(mod1_mco)




# guardo coeficientes en un data.frame para despu?s
coeficientes <- tribble(
  ~"Modelo", ~"Intercepto", ~"Pendiente",
  "MCO", coef(mod1_mco)[1], coef(mod1_mco)[2] 
)
summary(mod1_mco)
  

#Gr?ficos de diagn?stico:
par(mfrow = c(2, 2))
plot(mod1_mco)
par(mfrow = c(1, 1))

#Del grafico de los residuos sin estandarizar se observa que a la obs 1602 como muy alejada del patron del resto 
#La curva QQ esta bien salvo 2 extremos, las obs 1602 y 2184
#El leverage de la obs 1602 es muy elevado

#Medidas de diagn?stico:


mod1_mco_df <- 
  tibble(
    id = 1:nrow(data),
    residuos = mod1_mco$residuals,
    leverage = hatvalues(mod1_mco),
    res_est = rstandard(mod1_mco),
    res_stu = rstudent(mod1_mco),
    res_press = residuos / (1 - leverage),
    cook = cooks.distance(mod1_mco)
  )

View(mod1_mco_df)

fwrite (mod1_mco_df, "mod1_mco_df en xls_1.xlsx") ##cambié por fwrite (no entiendo para qué hacemos el excel)

influencePlot(mod1_mco)

#studRes         Hat       CookD
#180   1.109979 0.111134855 0.015403044
#250   3.102502 0.095000495 0.100719484
#1602 -6.785021 0.488076625 4.317031805
#2184  4.056228 0.002321314 0.003806307

influenceIndexPlot(mod1_mco)

#obs 1602 y 2184 pesan mucho


#Ajuste MCO sin algunos datos
#Vamos a ajustar nuevamente el modelo por MCO, pero excluyendo las observaciones 250, 1602, 2184:
#Pruebo eliminando solo 1602

excluir <- c(1602)
data2 <- slice(data, -excluir)
mod2_mco<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen, data=data2 )

summary(mod2_mco)


#Comparaci?n de coeficientes:

coeficientes <- 
  coeficientes %>% 
  rbind(list("MCO 2", coef(mod2_mco)[1], coef(mod2_mco)[2]))
coeficientes


#Modelo Intercepto Pendiente
#* <chr>       <dbl>     <dbl>
#  1 MCO         -38.3    0.0707
#  2 MCO 2       -35.3    0.100 



#Gr?ficos de diagn?stico mod2:
par(mfrow = c(2, 2))
plot(mod2_mco)
par(mfrow = c(1, 1))


#Medidas de diagn?stico mod2:
mod2_mco_df <- 
  tibble(
    id = setdiff(1:2704, excluir),
    residuos = mod2_mco$residuals,
    leverage = hatvalues(mod2_mco),
    res_est = rstandard(mod2_mco),
    res_stu = rstudent(mod2_mco),
    res_press = residuos / (1 - leverage),
    cook = cooks.distance(mod2_mco)
  )

View(mod2_mco_df)

write.xlsx(mod2_mco_df, "mod2_mco_df en xls_1.xlsx")

influencePlot(mod2_mco)

#StudRes        Hat        CookD
#180  -0.9983386 0.193860425 0.023968186
#250   3.2827632 0.095458629 0.113316218
#948  -3.7389215 0.013611583 0.019198408
#1070 -0.1437827 0.128238539 0.000304223
#1546 -3.0771338 0.119996167 0.128709905
#2183  4.1015004 0.002324026 0.003895765

influenceIndexPlot(mod2_mco)

## Hago prueba con el modelo Ridge

library(MASS)
lm.ridge(
  formula = pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, 
  data = data1, 
  lambda = seq(0, 1, by = 0.1)
)


## Hago prueba con el modelo Lasso
#Preparamos matrices

View(data1)

library(dplyr)

X_pje <- data1 
X_pje$pje <-  NULL
X_pje <- as.matrix(X_pje)


View(X_pje)

Y_pje <- data1$pje

#Elegimos Lambda usando CV
library(glmnet)
set.seed(2013)
lambda_pje <- cv.glmnet(x = X_pje, y = Y_pje, nfolds = 5, alpha = 1)$lambda.min

#Ajustamos modelo
lasso_pje <- glmnet(x = X_pje, y = data1$pje, alpha = 1, lambda = lambda_pje)

round(coefficients(lasso_pje), 4)

lasso_pred <- predict(lasso_pje, s = lambda_pje, newx = X_pje)

sst <- sum((Y_pje - mean(Y_pje))^2)
sse <- sum((lasso_pred - Y_pje)^2)

rsq <- 1 - sse/sst
rsq #" R2 del modelo ajustado por Lasso



# el ajuste descarto a todas las variables??? CHEQUEAR
# IRU -> Armé un Lasso que no, entiendo que no estaba bien el dataset del renglón 350 
# (lo  modifiqué un poquito)
# El R2 del lasso da 0.3235246 (FEO!)



#Jor --> metodo forward

data(data1)

stepAIC(
  object = lm(pje ~ 1, data = data1), #punto de partida
  scope = list(upper = lm(pje ~ ., data = data1)), #maximo modelo posible
  direction = "forward", #mÃ©todo de selecciÃ³n
  trace = FALSE #para no imprimir resultados parciales
)

#Call:
#  lm(formula = pje ~ pobreza + densidad + mujeres + ahorros + veteranos + 
#       ancianos + ingpc + crimen, data = data1)

#Coefficients:
#  (Intercept)      pobreza     densidad      mujeres      ahorros    veteranos  
#-3.767e+01    7.659e-01    1.888e-03    1.209e+00   -3.040e-05    3.515e-01  
#ancianos        ingpc       crimen  
#-8.112e-02    1.772e-04   -1.539e-03



#Jor --> metodo backward

stepAIC(
  object = lm(pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, data = data1), #punto de partida
  scope = list(upper = lm(medv ~ 1, data = Boston)), #m?ximo modelo posible
  direction = "backward", #m?todo de selecci?n
  trace = F #para no imprimir resultados parciales
)

Call:
  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
       densidad + ancianos + crimen, data = data1)

#Coefficients:
#  (Intercept)      ahorros        ingpc      pobreza    veteranos      mujeres  
#-3.767e+01   -3.040e-05    1.772e-04    7.659e-01    3.515e-01    1.209e+00  
#densidad     ancianos       crimen  
#1.888e-03   -8.112e-02   -1.539e-03 



#Jor --> metodo stepwise (coincide con el forward)

data(data1)

stepAIC(
  object = lm(pje ~ 1, data = data1), #punto de partida
  scope = list(upper = lm(pje ~ ., data = data1)), #maximo modelo posible
  direction = "both", #m?todo de selecci?
  trace = FALSE #para no imprimir resultados parciales
)


#Call:
#  lm(formula = pje ~ pobreza + densidad + mujeres + ahorros + veteranos + 
#       ancianos + ingpc + crimen, data = data1)

#Coefficients:
#  (Intercept)      pobreza     densidad      mujeres      ahorros    veteranos  
#-3.767e+01    7.659e-01    1.888e-03    1.209e+00   -3.040e-05    3.515e-01  
#ancianos        ingpc       crimen  
#-8.112e-02    1.772e-04   -1.539e-03


#mejor subconjunto
library(leaps)

mejorsub <- regsubsets(
  x = pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen, 
  data = data
)

mejorsub_print <- summary(mejorsub)
mejorsub_print

#Selection Algorithm: exhaustive
#        edad ahorros ingpc pobreza veteranos mujeres densidad ancianos crimen
#1  ( 1 ) " "  " "     " "   "*"     " "       " "     " "      " "      " "   
#2  ( 1 ) " "  " "     " "   "*"     " "       " "     "*"      " "      " "   
#3  ( 1 ) " "  " "     " "   "*"     " "       "*"     "*"      " "      " "   
#4  ( 1 ) " "  "*"     " "   "*"     " "       "*"     "*"      " "      " "   
#5  ( 1 ) " "  "*"     " "   "*"     "*"       "*"     "*"      " "      " "   
#6  ( 1 ) " "  "*"     " "   "*"     "*"       "*"     "*"      "*"      " "   
#7  ( 1 ) " "  "*"     "*"   "*"     "*"       "*"     "*"      "*"      " "   
#8  ( 1 ) " "  "*"     "*"   "*"     "*"       "*"     "*"      "*"      "*" 

#mod 1 pje = b0 + b1pobreza
#mod 2 pje = bo + b1pobreza + b2densidad
#mod 3 pje = bo + b1pobreza + b2densidad +b3mujeres
#mod 4 pje = bo + b1pobreza + b2densidad +b3mujeres + b4ahorros
#mod 5 pje = bo + b1pobreza + b2densidad +b3mujeres + b4ahorros +b5 veteranos
#mod 6 pje = bo + b1pobreza + b2densidad +b3mujeres + b4ahorros +b5 veteranos + b6 ancianos
#mod 7 pje = bo + b1pobreza + b2densidad +b3mujeres + b4ahorros +b5 veteranos + b6 ancianos + b7 ingpc
#mod 8 pje = bo + b1pobreza + b2densidad +b3mujeres + b4ahorros +b5 veteranos + b6 ancianos + b7 ingpc + b8 crimen

#la variable edad NUNCA la incluyo en el modelo


#Criterios de selecci?n de modelos
mejorsub_print$cp
mejorsub_print$adjr2
mejorsub_print$bic

#Gr?ficos para comparar ajustes
plot(mejorsub, scale = "Cp")
plot(mejorsub, scale = "adjr2")
plot(mejorsub, scale = "bic")


library(purrr)

m0 <- lm(pje ~ 1, data = data1)
m1 <- lm(pje ~ pobreza, data = data1)
m2 <- lm(pje ~ pobreza + densidad, data = data1)
m3 <- lm(pje ~ pobreza + densidad + mujeres, data = data1)
m4 <- lm(pje ~ pobreza + densidad + mujeres + ahorros, data = data1)
m5 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos, data = data1)
m51 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + veteranos:ahorros, data = data1) # Agrego al M5 una interacción que entiendo podría ser significativa entre las VA (derivado del corrplot)
m6 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos, data = data1)
m61<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ancianos:ahorros +
        veteranos:ahorros, data = data1)
m7 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc, data = data1)
m71<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + ingpc:pobreza + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
           ingpc:ahorros + ahorros:edad + crimen:ancianos + veteranos:ahorros, data = data1)
m8 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen, data = data1)
m81<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen + ingpc:pobreza + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
           ingpc:ahorros + ahorros:edad + crimen:ancianos + veteranos:ahorros, data = data1)




criterios <- function(m, maxi) {
  
  SCE <- deviance(m)
  n <- length(residuals(m))
  p <- length(coefficients(m)) #incluye intercepto
  
  #CME
  cme <- SCE/(n-p) #igual al cuadrado de la residual SE
  
  #R2 ajustado
  r2aj <- 1 - ((n-1)/(n-p)) * (1-summary(m)$r.squared)
  #summary(m)$adj.r.squared
  #yi <- iris$Petal.Length
  #1 - (deviance(m) / (n-p)) / ((sum((yi - mean(yi))^2))/(n-1))
  
  #PRESS
  estpress <- sum((residuals(m)/(1-hatvalues(m)))^2) #qpcR::PRESS(m)
  
  #Mallows CP
  mcp <- SCE/(deviance(maxi)/(n-length(coefficients(maxi)))) + 2*p - n
  #olsrr::ols_mallows_cp(m, maxi)
  
  #AKAIKE
  akaike <- n * log(SCE/n) + 2*p #extractAIC(m)[2]
  
  #BIC
  schwarz <- n * log(SCE/n) + p*log(n) #extractAIC(m, k = log(n))[2]
  
  tibble(CME = cme, R2Aj = r2aj, PRESS = estpress, Cp = mcp, AIC = akaike, BIC = schwarz)
  
}


map_dfr(list(m1, m2, m3, m4, m5, m6, m7, m8), criterios, maxi = m8)

#A tibble: 8 x 6
#  CME   R2Aj   PRESS     Cp    AIC    BIC
#<dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
#1  77.7 0.251 210230. 297.   11771. 11783.
#2  74.1 0.285 204992. 160.   11645. 11662.
#3  71.7 0.308 197799.  68.7  11557. 11580.
#4  70.7 0.318 196164.  30.3  11519. 11548.
#5  70.3 0.322 195374.  17.2  11506. 11541.
#6  70.1 0.323 194791.  11.8  11501. 11542.
#7  70.1 0.324 194771.   9.91 11499. 11546.
#8  70.0 0.325 194674.   9    11498. 11551.

# a simple vista el mejor modelo es m7 o m6 (sin interaccion)
#se tendria que hacer una prueba con los productos

################################################

sct <- sum((data1$pje - mean(data1$pje))^2)
m0 <- lm(pje ~ 1, data = data1)
sce0 <- sct
scr0 <- 0
anova(m0)

m1 <- lm(pje ~ pobreza, data = data1)
sce1 <- deviance(m1)
scr1 <- sct - sce1 # en el modelo nulo la SCE es la variabilidad total
r1.0 <- scr1 - scr0
anova(m1)

m2 <- lm(pje ~ pobreza + densidad, data = data1)

sce2 <- deviance(m2)
scr2 <- sct - sce2
r2.1 <- scr2 - scr1
anova(m2)


m3 <- lm(pje ~ pobreza + densidad + mujeres, data = data1)

sce3 <- deviance(m3)
scr3 <- sct - sce3
r3.12 <- scr3 - scr2
anova(m3)

m4 <- lm(pje ~ pobreza + densidad + mujeres + ahorros, data = data1)

sce4 <- deviance(m4)
scr4 <- sct - sce4
r4.123 <- scr4 - scr3
anova(m4)

m5 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos, data = data1)

sce5 <- deviance(m5)
scr5 <- sct - sce5
r5.1234 <- scr5 - scr4
anova(m5)

m6 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos, data = data1)

sce6 <- deviance(m6)
scr6 <- sct - sce6
r6.12345 <- scr6 - scr5
anova(m6)

m7 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc, data = data1)

sce7 <- deviance(m7)
scr7 <- sct - sce7
r7.123456 <- scr7 - scr6
anova(m7)

m8 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen, data = data1)

sce8 <- deviance(m8)
scr8 <- sct - sce8
r8.1234567 <- scr8 - scr7
anova(m8)

res <- tribble(
  ~ "Modelo", ~ "SCR", ~"SCE", ~"R",
  "m0", scr0, sce0, NA,
  "m1", scr1, sce1, r1.0,
  "m2", scr2, sce2, r2.1,
  "m3", scr3, sce3, r3.12,
  "m4", scr4, sce4, r4.123,
  "m5", scr5, sce5, r5.1234,
  "m6", scr6, sce6, r6.12345,
  "m7", scr7, sce7, r7.123456,
  "m8", scr8, sce8, r8.1234567,
)
res


#A tibble: 9 x 4
#Modelo    SCR     SCE      R
#<chr>   <dbl>   <dbl>  <dbl>
#1 m0         0  280213.    NA 
#2 m1     70344. 209869. 70344.
#3 m2     80093. 200120.  9749.
#4 m3     86626. 193586.  6533.
#5 m4     89456. 190757.  2829.
#6 m5     90507. 189706.  1051.
#7 m6     91028. 189185.   521.
#8 m7     91300. 188913.   273.
#9 m8     91504. 188708.   204.

# a medida que voy agregando t?rminos la SCR aumenta y la SCE disminuye
# la diferencia entre 2 SCR sucesivas o SCE sucesivas es la contribucion de la SC secuencial
# x ej el valor 70344 es el aporte que hace el predictor x1 para explicar la respuesta comparado con un modelo que no tiene ningun predictor

  
r1.0+r2.1+ r3.12+r4.123+r5.1234+r6.12345+r7.123456+r8.1234567
#[1] 91504.45 

scr8
#[1] 91504.45

## pruebas de los modelos con interacciones

summary(m0)
summary(m1)
summary(m2)
summary(m3)
summary(m4)
summary(m5)
summary(m51)
summary(m6)
summary(m61)
summary(m7)
summary(m71)
summary(m8)
summary(m81)

# Aplico logaritmo al modelo mod1

mod1_mco_log<-lm(log(pje) ~ log(edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen), data=data )

summary(mod1_mco_log)


#interaccion

m7_2 <- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc+pobreza : densidad, data = data1)
summary(m7_2)

# Iru -> La interacción está arriba, junto a los modelos. Se sacaron los resultados en el summary.
