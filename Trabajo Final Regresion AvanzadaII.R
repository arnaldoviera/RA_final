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
library(psych)
library(glmnet)
library(dplyr)
library(leaps)
library(UsingR)
library(tidyverse)
library(MASS)
library(glmnet)
library(leaps)
library(purrr)
library(corrplot)



#===================================================================
#::ESTE PEDAZO DE CÓDIGO ES IMPORTANTE BORRAR ANTES DE COMPARTIRLO::

#Cargamos el dataset / Cadauno usa su directorio
setwd("C:/Users/vieraa/Documents/GitHub/RA_final" )   
#setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
#setwd("C:/Users/jgiberti/Documents/Univ. Austral - Maestria en Ciencias de Datos/10. Regresion Avanzada/TP/" )   
#setwd("C:/Users/isant/OneDrive/Desktop/MCD/Regresión Avanzada/TP Final")
#==================================================================

#cardo dataset de tarea
data <- fread("clinton.txt")

# CONSIGNA
#"A traves de un modelo de Regresión Lineal Multiple ajustado por M????nimos Cuadrados Ordinarios,
#estudiar el porcentaje de votos obtenidos por el candidato Bill Clinton en cada uno de los condados
#estadounidenses. Pueden incorporar como explicativas a cualquiera de las restantes variables presentes en
#la base"

#0 EDA


#Voy a realizar un EDA del dataset
glimpse(data)

#Tenemos las siguientes variables como caracteres: condado y estado.
#particularmente como double todas excepto: ahorros, ingpc y crimen que
#son de tipo integer.

str(data) 
#Tenemos 12 variables con un total de 2704 obseerevaciones, el tipo 
#de dato asociado es "data.table" y "data.frame"

#Analicemos la distribucion de frecuencia por estado
Frec_abs=table(data$estado)
Frec_abs

Frec_abs_acum=cumsum(Frec_abs)
Frec_abs_acum

Frec_rel=prop.table(Frec_abs_acum)*100
Frec_rel

Frec_rel_abs = cumsum(Frec_rel)*100
Frec_rel_abs

library(gmodels)
CrossTable(data$estado, prop.t=F, prop.chisq = F)

#Datos numericos
summary(data)

#Esta funcion nos muestra un panorama más completo de las variables. 
describe(data)

#Missings ---- No tenemos 
sapply(data, function(x) sum(is.na(x)))

#Realicemos algunos histogramas de las variablees mas importantes

histograma_edad <- ggplot(data, aes(x=edad)) +
  ggtitle("Distribucion de la variable Edad") +
  geom_histogram(color="#28324a", fill="#3c78d8")
histograma_edad

#Claramentee es asimetrica hacia la derecha
histograma_ahorros <- ggplot(data, aes(x=ahorros)) +
  ggtitle("Distribucion de la variable Ahorros") +
  geom_histogram(color="#28324a", fill="#3c78d8")
histograma_ahorros

#ingpc
histograma_ingpc <- ggplot(data, aes(x=ingpc)) +
  ggtitle("Distribucion de la variable ingpc") +
  geom_histogram(color="#28324a", fill="#3c78d8")
histograma_ingpc

histograma_pobreza <- ggplot(data, aes(x=pobreza)) +
  ggtitle("Distribucion de la variable Pobreza") +
  geom_histogram(color="#28324a", fill="#3c78d8")
histograma_pobreza

#Graficos de dispersion
ggplot(data = data, aes(x = ahorros, y = edad)) + 
  geom_point(aes(color = estado), size = 2, alpha = 0.7) +
  xlab('Ahorros') + 
  ylab('Edad') +
  ggtitle('Grafico de Dispersion entre Ahorros y Edad') + 
  theme_minimal()

#Graficos de dispersion
ggplot(data = data, aes(x = pobreza, y = mujeres)) + 
  geom_point(aes(color = estado), size = 2, alpha = 0.7) +
  xlab('Pobreza') + 
  ylab('Mujeres') +
  ggtitle('Grafico de Dispersion entre pobreza y mujeres') + 
  theme_minimal()

ggplot(data = data, aes(x = pobreza, y = crimen)) + 
  geom_point(aes(color = estado), size = 2, alpha = 0.7) +
  xlab('Pobreza') + 
  ylab('Crimen') +
  ggtitle('Grafico de Dispersion entre pobreza y crimen') + 
  theme_minimal()

#Grafico de BoxPlot

ggplot(data, aes(y=pje)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=edad)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=ahorros)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=ingpc)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=pobreza)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=veteranos)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=densidad)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=ancianos)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

ggplot(data, aes(y=crimen)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) 

#Practicamente en todas las variables de interes, tenemos 
#muchos outliers, podriamos darle un tratamiento para intentar
#mejorar los modelos (es decir no quitarlos)

#Por otro lado, viendo los modelos propuestos creo que los mejores
#hasta ahora son los siguientes: mod_9, mod_10 , mod_11 , mod_12, 
#mod_14, mod_15

#Si les parece puedo probar darle un tratamiento y replicar lo que 
#se hizo con el dataset data_dummy_2. 

#Grafico de Barra: Crimen x Estado
ggplot(data = data,
       mapping = aes(x = estado, y = crimen)) +
  geom_bar(stat='identity')



grafico1<-ggpairs(data, title="correlogram with ggpairs()") 
grafico1

grafico2<-ggcorr(data, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")
grafico2



multi.hist(x = data1, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = NULL)

#Visualizo que relacion tiene cada variable con la respuesta
mfrow = c(2, 2)
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


data1 %>%
  ggplot(aes(ingpc, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "ingpc", y = "PJE")


data1 %>%
  ggplot(aes(pobreza, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Pobreza", y = "PJE")


data1 %>%
  ggplot(aes(veteranos, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Veteranos", y = "PJE")


data1 %>%
  ggplot(aes(mujeres, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Mujeres", y = "PJE")


data1 %>%
  ggplot(aes(densidad, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Densidad", y = "PJE")


data1 %>%
  ggplot(aes(ancianos, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Ancianos", y = "PJE")


data1 %>%
  ggplot(aes(crimen, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Crimen", y = "PJE")

mfrow = c(1, 1)





#CONSIGNA 1: regresión lineal múltiple


#MOD 1 - Modelo con todas las cuantitativas + Regiones Según EDA "corplot" le sacamos ingpc y edad 

mod_1<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + ancianos + crimen , data = data)
summary(mod_1)   

#Residual standard error: 8.375 on 2696 degrees of freedom
#Multiple R-squared:  0.3251,	Adjusted R-squared:  0.3234 
#F-statistic: 185.5 on 7 and 2696 DF,  p-value: < 2.2e-16

#######################################
#######ACA IRIA COMENTARIOS DE QUÉ CORNO VIMOS EN EL MODELO 1################


## Paso estados a datos dummy
data_dummy <- data

#data_dummy<- cbind(data,clasif_estados) ##se pega bien

#data_dummy$Regiones <- as.factor(data_dummy$Regiones) 

#data_dummy$Este <- as.factor(data_dummy$Este) 


#data_dummy <- cbind(data_dummy, dummy(data_dummy$Regiones, sep ="_"))

#data_dummy$estado <- NULL


data_dummy <- cbind(data_dummy, dummy(data$estado, sep ="_"))


##Borro lo que no necesito:
data_dummy$estado <- NULL
data_dummy$condado <- NULL
data_dummy$edad<- NULL # ya no se ten????a en cuenta en el mod 1
data_dummy$ingpc<- NULL# ya no se ten????a en cuenta en el mod 1
#data_dummy$`Descrip Estado` <- NULL
#data_dummy$Regiones <- NULL

#borro 1 dummy por cada clase de variables que transformÃÂ©:

#data_dummy$data_dummy_1 <- NULL #a la mierda regiÃÂ³n 1
data_dummy$data_dummy_AL <- NULL #a la mierda Alabam



#MOD 2 - vamos a incluir todas las variables, que le qudan al dataset, para este modelo 2

mod_2<- lm(pje ~ ., data = data_dummy)
summary(mod_2)   

#Residual standard error: 6.878 on 2648 degrees of freedom
#Multiple R-squared:  0.553,	Adjusted R-squared:  0.5437 
#F-statistic: 59.56 on 55 and 2648 DF,  p-value: < 2.2e-16 


#MOD3 - Vemos con método backward cuales quedan


#mÃ©todo backward para experimento

stepAIC(
  object = lm(pje ~ ., data = data_dummy), #punto de partida
  scope = list(upper = lm(pje ~ 1, data = data_dummy)), #mÃ¡ximo modelo posible
  direction = "backward", #mÃ©todo de selecciÃ³n
  trace = F #para no imprimir resultados parciales
)

mod_3 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + ##42 de las 56 variables
              densidad + crimen + data_dummy_AR + data_dummy_AZ + data_dummy_CO + 
              data_dummy_CT + data_dummy_DC + data_dummy_FL + data_dummy_IA + 
              data_dummy_ID + data_dummy_IL + data_dummy_IN + data_dummy_KS + 
              data_dummy_LA + data_dummy_MA + data_dummy_ME + data_dummy_MN + 
              data_dummy_MS + data_dummy_MT + data_dummy_NC + data_dummy_ND + 
              data_dummy_NE + data_dummy_NH + data_dummy_NM + data_dummy_NV + 
              data_dummy_NY + data_dummy_OH + data_dummy_OK + data_dummy_OR + 
              data_dummy_PA + data_dummy_RI + data_dummy_SD + data_dummy_TN + 
              data_dummy_TX + data_dummy_UT + data_dummy_VT + data_dummy_WI + 
              data_dummy_WY, data = data_dummy)

summary(mod_3)

anova(mod_3)
## conclusiones del ANOVA hay varias que no suman en significancia

#MOD4 IDEM 3 con variables no significativas según anova

mod_4 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres +
              densidad + data_dummy_AR + data_dummy_AZ  + 
              data_dummy_CT + data_dummy_DC + data_dummy_FL + data_dummy_IA + 
              data_dummy_ID + data_dummy_IL + data_dummy_IN + data_dummy_KS + 
              data_dummy_LA + data_dummy_MA + data_dummy_ME + data_dummy_MN + 
              data_dummy_MS + data_dummy_MT + data_dummy_NC + data_dummy_ND + 
              data_dummy_NE + data_dummy_NH + data_dummy_OH + data_dummy_OK + 
              data_dummy_OR + data_dummy_RI + data_dummy_SD + data_dummy_TN + data_dummy_TX + data_dummy_UT + data_dummy_VT + data_dummy_WI + 
              data_dummy_WY, data = data_dummy)

summary(mod_4)

#Residual standard error: 6.911 on 2667 degrees of freedom
#Multiple R-squared:  0.5455,	Adjusted R-squared:  0.5393 
#F-statistic:  88.9 on 36 and 2667 DF,  p-value: < 2.2e-16

anova(mod_4)

#MOD5 idem 4 pero sin las variables que tienen menos de 3 estrellas

mod_5 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + ##42 de las 56 variables
              densidad + data_dummy_AR +  
               data_dummy_DC + data_dummy_FL + data_dummy_IA +   data_dummy_ID + 
              data_dummy_IL +  data_dummy_KS + data_dummy_MA +  data_dummy_MN + 
              data_dummy_MS +  data_dummy_NC +   data_dummy_NE +
              data_dummy_OK +  data_dummy_TN + data_dummy_TX + 
              data_dummy_UT + data_dummy_VT + data_dummy_WI + 
              data_dummy_WY, data = data_dummy)

summary(mod_5)

#Residual standard error: 7.021 on 2679 degrees of freedom
#Multiple R-squared:  0.5288,	Adjusted R-squared:  0.5246 
#F-statistic: 125.3 on 24 and 2679 DF,  p-value: < 2.2e-16

anova(mod_5)

###nos quedamos con el mod_5 como modelo directo 

plot(mod_5)



#MOD5_A idem 5 pero sin la observación 1602 según gráficos de diagnóstico en plot(mod_5)

excluir <- c(1602)
data_dummy_1 <- slice(data_dummy, -excluir)


mod_5_A <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + ##42 de las 56 variables
              densidad + data_dummy_AR +  
              data_dummy_DC + data_dummy_FL + data_dummy_IA +   data_dummy_ID + 
              data_dummy_IL +  data_dummy_KS + data_dummy_MA +  data_dummy_MN + 
              data_dummy_MS +  data_dummy_NC +   data_dummy_NE +
              data_dummy_OK +  data_dummy_TN + data_dummy_TX + 
              data_dummy_UT + data_dummy_VT + data_dummy_WI + 
              data_dummy_WY, data = data_dummy_1)

summary(mod_5_A)

#Residual standard error: 6.954 on 2678 degrees of freedom
#Multiple R-squared:  0.5362,	Adjusted R-squared:  0.5321 
#F-statistic:   129 on 24 and 2678 DF,  p-value: < 2.2e-16

plot(mod_5_A)

######listo nos quedamos con el mod_5_a

#MOD_5_A1 agregamos iteraciones

mod_5_A1 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + densidad +
                 ahorros : veteranos + ahorros : mujeres+ pobreza : veteranos + pobreza : mujeres +
                 ahorros : densidad + veteranos : mujeres + pobreza : densidad + ahorros : pobreza +
                 veteranos : densidad + mujeres : densidad + 
                 data_dummy_AR + data_dummy_FL + data_dummy_IA +   
                 data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
                 data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
                 data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
                 data_dummy_VT + data_dummy_WI + data_dummy_WY, data = data_dummy_1)


summary(mod_5_A1)
#Residual standard error: 6.912 on 2669 degrees of freedom
#Multiple R-squared:  0.5434,	Adjusted R-squared:  0.5377 
#F-statistic: 96.25 on 33 and 2669 DF,  p-value: < 2.2e-16

anova(mod_5_A1)


#mod_5_A1.1 saco todas las variables que tienen menos de 3 estrellas

mod_5_A1.1 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + densidad +
               pobreza : veteranos +  
               data_dummy_AR + data_dummy_FL + data_dummy_IA +   
               data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
               data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
               data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
               data_dummy_VT + data_dummy_WI + data_dummy_WY, data = data_dummy_1)

summary(mod_5_A1.1)

#Residual standard error: 6.942 on 2678 degrees of freedom
#Multiple R-squared:  0.5379,	Adjusted R-squared:  0.5338 
#F-statistic: 129.9 on 24 and 2678 DF,  p-value: < 2.2e-16

anova(mod_5_A1.1)


mod_5_A1.2 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + densidad +
                   pobreza : veteranos + veteranos:densidad +
                   data_dummy_AR + data_dummy_FL + data_dummy_IA +   
                   data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
                   data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
                   data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
                   data_dummy_VT + data_dummy_WI + data_dummy_WY, data = data_dummy_1)

summary(mod_5_A1.2)
anova(mod_5_A1.2)

#### listo, fin: nos quedamos con este modelo que incluye la iteración con veterano:pobreza

confint(mod_5_A1.1)



summary(mod_5_A1.1)$r.sq
#0.5378976
summary(mod_5_A1.1)$sigma
#6.941563
deviance(mod_5_A1.1) #sce
#129040.2



#Validacion de los supuestos:
#normalidad
#homocedasticidad
plot(mod_5_A1) 
#ok comentario en el documento


par(mfrow = c(2, 2))
plot(mod_5_A1.1)
par(mfrow = c(1, 1))



par(mfrow = c(2, 2))
plot(mod_5_A1)
par(mfrow = c(1, 1))


par(mfrow = c(2, 2))
plot(mod_5_A1.2)
par(mfrow = c(1, 1))

cooks.distance(mod_5_A1)

plot(cooks.distance(mod_5_A1))


influence.measures(mod_5_A1.1)
#identifica a algunas obs como influyentes

vif(lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + 
         densidad + pobreza:veteranos + data_dummy_AR  + 
         data_dummy_FL + data_dummy_IA + data_dummy_ID + data_dummy_IL + 
         data_dummy_KS + data_dummy_MA + data_dummy_MN + data_dummy_MS + 
         data_dummy_NC + data_dummy_NE + data_dummy_OK + data_dummy_TN + 
         data_dummy_TX + data_dummy_UT + data_dummy_VT + data_dummy_WI + 
         data_dummy_WY, data = data_dummy_1))

#ahorros           pobreza         veteranos           mujeres 
#1.433482         18.470366          7.417733          1.157438 
#densidad     data_dummy_AR     data_dummy_FL     data_dummy_IA 
#1.102612          1.052546          1.036064          1.103291 
#data_dummy_ID     data_dummy_IL     data_dummy_KS     data_dummy_MA 
#1.023807          1.079775          1.127817          1.044561 
#data_dummy_MN     data_dummy_MS     data_dummy_NC     data_dummy_NE 
#1.052795          1.090232          1.065086          1.122315 
#data_dummy_OK     data_dummy_TN     data_dummy_TX     data_dummy_UT 
#1.065315          1.038511          1.118331          1.023149 
#data_dummy_VT     data_dummy_WI     data_dummy_WY pobreza:veteranos 
#1.009307          1.048553          1.038206         18.678979 
#hay valores de VIF > 5/10. Son problematicos


vif(lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + 
         densidad + pobreza:veteranos + veteranos:densidad + data_dummy_AR  + 
         data_dummy_FL + data_dummy_IA + data_dummy_ID + data_dummy_IL + 
         data_dummy_KS + data_dummy_MA + data_dummy_MN + data_dummy_MS + 
         data_dummy_NC + data_dummy_NE + data_dummy_OK + data_dummy_TN + 
         data_dummy_TX + data_dummy_UT + data_dummy_VT + data_dummy_WI + 
         data_dummy_WY, data = data_dummy_1))

#Medidas de diagn0stico:


#mejorsub <- regsubsets(
#  x = pje ~ ahorros + pobreza + veteranos + mujeres + 
#    +          densidad + pobreza:veteranos + data_dummy_AR + data_dummy_DC + 
#    +          data_dummy_FL + data_dummy_IA + data_dummy_ID + data_dummy_IL + 
#    +          data_dummy_KS + data_dummy_MA + data_dummy_MN + data_dummy_MS + 
#    +          data_dummy_NC + data_dummy_NE + data_dummy_OK + data_dummy_TN + 
#    +          data_dummy_TX + data_dummy_UT + data_dummy_VT + data_dummy_WI + 
#    +          data_dummy_WY, data = data_dummy_1
#)

#mejorsub_ <- summary(mejorsub)
#mejorsub_

#ahorros pobreza veteranos mujeres densidad data_dummy_AR data_dummy_DC  data_dummy_FL data_dummy_IA data_dummy_ID data_dummy_IL data_dummy_KS    data_dummy_MA data_dummy_MN data_dummy_MS data_dummy_NC data_dummy_NE   data_dummy_OK data_dummy_TN data_dummy_TX data_dummy_UT data_dummy_VT   data_dummy_WI data_dummy_WY pobreza:veteranos 
#1  ( 1 ) " "     "*"     " "       " "     " "      " "           " "             " "           " "           " "           " "           " "               " "           " "           " "           " "           " "            " "           " "           " "           " "           " "               " "           " "           " "
#2  ( 1 ) " "     "*"     " "       " "     "*"      " "           " "             " "           " "           " "           " "           " "               " "           " "           " "           " "           " "            " "           " "           " "           " "           " "               " "           " "           " "
#3  ( 1 ) " "     "*"     " "       " "     "*"      "*"           " "             " "           " "           " "           " "           " "               " "           " "           " "           " "           " "            " "           " "           " "           " "           " "               " "           " "           " "
#4  ( 1 ) " "     "*"     " "       " "     "*"      "*"           " "             " "           " "           " "           " "           " "               " "           " "           " "           " "           "*"            " "           " "           " "           " "           " "               " "           " "           " "
#5  ( 1 ) " "     "*"     " "       " "     "*"      "*"           " "             " "           " "           " "           " "           "*"               " "           " "           " "           " "           "*"            " "           " "           " "           " "           " "               " "           " "           " "
#6  ( 1 ) " "     "*"     " "       " "     "*"      "*"           " "             " "           " "           " "           " "           "*"               " "           " "           " "           " "           "*"            " "           " "           "*"           " "           " "               " "           " "           " "
#7  ( 1 ) " "     "*"     " "       " "     "*"      "*"           " "             " "           " "           " "           " "           "*"               " "           " "           " "           " "           "*"            " "           " "           "*"           "*"           " "               " "           " "           " "
#8  ( 1 ) " "     "*"     " "       "*"     "*"      "*"           " "             " "           " "           " "           " "           "*"               " "           " "           " "           " "           "*"            " "           " "           "*"           "*"           " "               " "           " "           " "




#mejorsub_ <- regsubsets(
#  x = pje ~ . , 
#  data = data_dummy_1
#)

#resumen <- summary(mejorsub_)
#resumen

#Length Class      Mode     
#which  208    -none-     logical  
#rsq      8    -none-     numeric  
#rss      8    -none-     numeric  
#adjr2    8    -none-     numeric  
#cp       8    -none-     numeric  
#bic      8    -none-     numeric  
#outmat 200    -none-     character
#obj     28    regsubsets list    


#Criterios de selecci?n de modelos
#mejorsub_$cp
#mejorsub_$adjr2
#mejorsub_$bic

#Gr?ficos para comparar ajustes
#plot(mejorsub_, scale = "Cp")
#plot(mejorsub_, scale = "adjr2")
#plot(mejorsub_, scale = "bic")
#NO FUNCA


#m0 <- lm(pje ~ 1, data = data_dummy_1)
#m1 <- lm(pje ~ pobreza, data = data_dummy_1)
#m2 <- lm(pje ~ pobreza + densidad, data = data_dummy_1)
#m3 <- lm(pje ~ pobreza + densidad + data_dummy_AR, data = data_dummy_1)
#m4 <- lm(pje ~ pobreza + densidad + data_dummy_AR + data_dummy_NE, data = data_dummy_1)
#m5 <- lm(pje ~ pobreza + densidad + data_dummy_AR + data_dummy_NE + data_dummy_KS, data = data_dummy_1)
#m6 <- lm(pje ~ pobreza + densidad + data_dummy_AR + data_dummy_NE + data_dummy_KS + data_dummy_TX , data = data_dummy_1)
#m7 <- lm(pje ~ pobreza + densidad + data_dummy_AR + data_dummy_NE + data_dummy_KS + data_dummy_TX + data_dummy_UT, data = data_dummy_1)
#m8 <- lm(pje ~ pobreza + densidad + data_dummy_AR + data_dummy_NE + data_dummy_KS + data_dummy_TX + data_dummy_UT + mujeres, data = data_dummy_1)


#Seleccion del modelo a utilizar

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


map_dfr(list(mod_1, mod_2, mod_3, mod_4, mod_5, mod_5_A, mod_5_A1, mod_5_A1.1, mod_5_A1.2), criterios, maxi = mod_5_A1.2)

#A tibble: 8 x 6
#  CME   R2Aj   PRESS     Cp    AIC    BIC
#<dbl> <dbl>   <dbl>  <dbl>  <dbl>  <dbl>

#1 70.1 0.323 194939. 1254.   11501. 11549.
#2  47.3 0.544 133391.   18.7  10484. 10814.
#3  47.3 0.544    Inf     3.36 10469. 10722.
#4  47.8 0.539    Inf    24.8  10491. 10709.
#5  49.3 0.525 137985.   98.2  10564. 10712.
#6  48.4 0.532 132178.   45.3  10509. 10656.
#7  47.8 0.538 131781.   21.7  10485. 10686.
#8  48.2 0.534 131922.   35.6  10499. 10647.
#9  48.0 0.536 131436.   26    10489. 10643.



#Realizo las pruebas de hipotesis del modelo

# SCT
sct <- sum((data_dummy_1$pje - mean(data_dummy_1$pje))^2)

# comparo dos modelos
anova(mod_5_A1.1, mod_5_A1)

#Analysis of Variance Table
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1   2678 129040                                  
#2   2669 127510  9    1530.6 3.5599 0.0002081 ***

#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

anova(mod_5_A1.1, mod_5_A1.2)
#Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
#1   2678 129040                                  
#2   2677 128483  1    556.94 11.604 0.0006678 ***
  
# Test
anova(mod_5_A1.2)
 
vif(lm(mod_5_A1.2))


# Matriz del modelo
matriz <- model.matrix(mod_5_A1.2)
head(matriz)

lm(data_dummy_1$pje ~ matriz) # sin constante porque ya est? en la matriz
anova(mod_5_A1.2)

TukeyHSD(aov(mod_5_A1.2), "ver")
#TERMINARLO; VER PORQUE NO FUNCIONA
?TukeyHSD





########################################
########################################


mod_df <- 
  tibble(
    id = 1:nrow(data_dummy_1),
    residuos = mod_5_A$residuals,
    leverage = hatvalues(mod_5_A),
    res_est = rstandard(mod_5_A),
    res_stu = rstudent(mod_5_A),
    res_press = residuos / (1 - leverage),
    cook = cooks.distance(mod_5_A)
  )

View(mod_df)

#fwrite (mod_4_df, "mod1_mco_df en xls_1.xlsx") 

influencePlot(mod_5_A)

#StudRes        Hat      CookD
#890  -4.796084 0.01934474 0.01730979
#948  -4.836059 0.01184324 0.01069147
#1070 -0.843276 0.28257634 0.01077390
#1546 -1.784302 0.31405468 0.05601765
#2006 -2.691606 0.15120531 0.04952246

anova(mod_5_A1.2)
summary(mod_5_A1.2)

###################################################################################
###################################################################################

#creo variable de clase a partir de pje


data_dummy_1$pje_class <- cut(data_dummy_1$pje, # Vector de entrada (numerico)
                        breaks=  c(min(data_dummy_1$pje), 50.000000, max(data_dummy_1$pje)),        # NÃÂºmero o vector con los cortes
                        labels = c(0,1),    	# Etiquetas para cada grupo
)   

data$pje_class <- cut(data_dummy$pje, # Vector de entrada (numerico)
                            breaks=  c(min(data_dummy$pje), 50.000000, max(data_dummy$pje)),        # NÃÂºmero o vector con los cortes
                            labels = c(0,1),  	# Etiquetas para cada grupo
)   


#Graficos de disp con la variable rta dicotomica

par(mfrow = c(1, 2))
ggplot(data_dummy_1, aes(x = ahorros, y = pje, color = pje_class)) + geom_point()
ggplot(data_dummy_1, aes(x = pobreza, y = pje, color = pje_class)) + geom_point()
ggplot(data_dummy_1, aes(x = veteranos, y = pje, color = pje_class)) + geom_point()
ggplot(data_dummy_1, aes(x = mujeres, y = pje, color = pje_class)) + geom_point()


par(mfrow = c(1, 1))


# otra manera de mirarlos
ggplot(data, aes(x = ahorros, y = pje, color = estado)) + geom_point()

ggplot(data, aes(x = ahorros, y = pje, color = estado)) + geom_point()
ggplot(data, aes(x = pobreza, y = pje, color = estado)) + geom_point()
ggplot(data, aes(x = veteranos, y = pje, color =estado)) + geom_point()
ggplot(data, aes(x = mujeres, y = pje, color = estado)) + geom_point()

##voy a probar con el mod_5_A1.2  suponiendo que son las variables que nos quedamos
install.packages("lmtest")
library(lmtest)


mod_5_A1_1_glm <- glm(pje_class ~ ahorros + pobreza + veteranos + mujeres + densidad +
                   pobreza : veteranos +  
                   data_dummy_AR + data_dummy_FL + data_dummy_IA +   
                   data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
                   data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
                   data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
                   data_dummy_VT + data_dummy_WI + data_dummy_WY, family = binomial(link = "logit"), data = data_dummy_1)

summary(mod_5_A1_1_glm)
#Null deviance: 2251.8  on 2701  degrees of freedom
#Residual deviance: 1484.6  on 2677  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 1534.6

#Number of Fisher Scoring iterations: 16

mod_5_A1_2_glm <- glm(pje_class ~ ahorros + pobreza + veteranos + mujeres + densidad +
            pobreza : veteranos + veteranos:densidad +
            data_dummy_AR + data_dummy_FL + data_dummy_IA +   
            data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
            data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
            data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
            data_dummy_VT + data_dumm y_WI + data_dummy_WY, family = binomial(link = "logit"), data = data_dummy_1)

summary(mod_5_A1_2_glm)
#Null deviance: 2251.8  on 2701  degrees of freedom
#Residual deviance: 1484.6  on 2676  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 1536.6

#Number of Fisher Scoring iterations: 16

lrtest(mod_5_A1_1_glm, mod_5_A1_2_glm)

#Df  LogLik Df  Chisq Pr(>Chisq)
#1  25 -742.30                     
#2  26 -742.29  1 0.0107     0.9178
#NO rechazamos la hip?tesis nula y concluimos que el modelo que incorpora la 2da interaccion no es estad?sticamente significativo.


anova(mod_5_A1_1_glm, mod_5_A1_2_glm) #Funci?n anova para comparar ajustes de modelos
2 * (logLik(mod_5_A1_2_glm) - logLik(mod_5_A1_1_glm)) #Definici?n de estad?stica RV
deviance(mod_5_A1_1_glm) - deviance(mod_5_A1_2_glm) #Deviance = -2*logLik

#Resid. Df Resid. Dev Df Deviance
#1      2677     1484.6            
#2      2676     1484.6  1 0.010652
# 2 * (logLik(mod_5_A1_2_glm) - logLik(mod_5_A1_1_glm)) #Definici?n de estad?stica RV
#'log Lik.' 0.01065199 (df=26)
# deviance(mod_5_A1_1_glm) - deviance(mod_5_A1_2_glm) #Deviance = -2*logLik
#[1] 0.01065199

#comparo el modelo sin interaccion y con 1 interaccion

mod_5_A_glm <- glm(pje_class ~ ahorros + pobreza + veteranos + mujeres + ##42 de las 56 variables
                densidad + data_dummy_AR +  
                data_dummy_DC + data_dummy_FL + data_dummy_IA +   data_dummy_ID + 
                data_dummy_IL +  data_dummy_KS + data_dummy_MA +  data_dummy_MN + 
                data_dummy_MS +  data_dummy_NC +   data_dummy_NE +
                data_dummy_OK +  data_dummy_TN + data_dummy_TX + 
                data_dummy_UT + data_dummy_VT + data_dummy_WI + 
                data_dummy_WY, family = binomial(link = "logit"), data = data_dummy_1)

summary(mod_5_A_glm) #OJO QUE ACA ESTA EL ESTADO DC

#Analisis de los residuos


residualPlots(mod_5_A_glm, type = "deviance", cex = 0.6)
residualPlots(mod_5_A_glm, type = "pearson", cex = 0.6)

residualPlot(mod_5_A_glm, variable = "fitted", type = "pearson", plot = TRUE, smooth=TRUE)

             
             
distancias.cook <- cooks.distance(mod_5_A_glm)
#Se consideran valores influyentes aquellos cuya distancia de cook sea mayor que 1
table(distancias.cook > 1)
#FALSE 
#2701
#En nuestro caso, no hay ninguna distancia de cook significativa.

medidas.infl <- influence.measures(mod_5_A_glm)


hat.valores <- hatvalues(mod_5_A_glm)         
table(hat.valores > 1)

par(mfrow = c(1, 2))
plot(mod_5_A_glm, which = 4)
plot(mod_5_A_glm, which = 6)
#no dibujo a 252 obs por tener leverage = 1

influenceIndexPlot(mod_5_A_glm, id.cex = 0.7, id.n = 3)

vif(mod_5_A_glm)
#ausencia de colinealidad


#interpretacion de los coeficientes
exp(coefficients(mod_5_A_glm)) #dificil explicar

log.odds <- predict(mod_5_A_glm)
log.odds

exp(log.odds)/(1+exp(log.odds))



lrtest(mod_5_A_glm, mod_5_A1_2_glm)
#Df  LogLik Df  Chisq Pr(>Chisq)
#1  25 -743.39                     
#2  26 -742.29  1 2.1989     0.1381
#tampoco es significativo, es decir, la inclusion de la interaccion no aporta ??


library(vcd)
predicciones <- ifelse(test = mod_5_A_glm$fitted.values > 0.5, yes = 1, no = 0)

matriz_confusion <- table(mod_5_A_glm$model$pje_class, predicciones,
                          dnn = c("observaciones", "predicciones"))

matriz_confusion

#              predicciones
#observaciones    0    1
#            0 2244   62
#            1  236  160



mosaic(matriz_confusion, shade = T, colorize = T,
       gp = gpar(fill = matrix(c("green3", "red2", "red2", "green3"), 2, 2)))

#El modelo generado es capaz de diferenciar bien entre pje mayor a 50 y los menor a 50,
# se han identificado 160 de los 396 correctamente. 
#Si fuese capaz, la probabilidad calculada por el modelo para aquellos pje que s? son son mayores a 50?? deber?an estar por encima del 0.5.
 
library(pROC)
curvaROC <- roc(
  response = data_dummy_1$pje_class,
  predictor = fitted.values(mod_5_A_glm),
  quiet = TRUE
)
plot(curvaROC, print.auc = TRUE)
