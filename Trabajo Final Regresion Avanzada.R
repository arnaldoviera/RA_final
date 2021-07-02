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
#"A traves de un modelo de Regresión Lineal Multiple ajustado por Mínimos Cuadrados Ordinarios,
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



grafico1<-ggpairs(data1, title="correlogram with ggpairs()") 
grafico1

grafico2<-ggcorr(data1, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 3, label_color = "white")
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
data_dummy$edad<- NULL # ya no se tenía en cuenta en el mod 1
data_dummy$ingpc<- NULL# ya no se tenía en cuenta en el mod 1
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


#mÃ©todo backward para eexperimento

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

mod_4 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + ##42 de las 56 variables
              densidad + data_dummy_AR + data_dummy_AZ  + 
              data_dummy_CT + data_dummy_DC + data_dummy_FL + data_dummy_IA + 
              data_dummy_ID + data_dummy_IL + data_dummy_IN + data_dummy_KS + 
              data_dummy_LA + data_dummy_MA + data_dummy_ME + data_dummy_MN + 
              data_dummy_MS + data_dummy_MT + data_dummy_NC + data_dummy_ND + 
              data_dummy_NE + data_dummy_NH + 
              data_dummy_OH + data_dummy_OK + data_dummy_OR + 
              data_dummy_RI + data_dummy_SD + data_dummy_TN + 
              data_dummy_TX + data_dummy_UT + data_dummy_VT + data_dummy_WI + 
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
                 data_dummy_AR + data_dummy_DC + data_dummy_FL + data_dummy_IA +   
                 data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
                 data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
                 data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
                 data_dummy_VT + data_dummy_WI + data_dummy_WY, data = data_dummy_1)


summary(mod_5_A1)
#Residual standard error: 6.905 on 2668 degrees of freedom
#Multiple R-squared:  0.5444,	Adjusted R-squared:  0.5386 
#F-statistic: 93.77 on 34 and 2668 DF,  p-value: < 2.2e-16

anova(mod_5_A1)


#mod_5_A1.1 saco todas las variables que tienen menos de 3 estrellas

mod_5_A1.1 <- lm(formula = pje ~ ahorros + pobreza + veteranos + mujeres + densidad +
               pobreza : veteranos +  
               data_dummy_AR + data_dummy_DC + data_dummy_FL + data_dummy_IA +   
               data_dummy_ID + data_dummy_IL + data_dummy_KS + data_dummy_MA + 
               data_dummy_MN + data_dummy_MS + data_dummy_NC + data_dummy_NE +
               data_dummy_OK + data_dummy_TN + data_dummy_TX +  data_dummy_UT + 
               data_dummy_VT + data_dummy_WI + data_dummy_WY, data = data_dummy_1)

summary(mod_5_A1.1)

#Residual standard error: 6.936 on 2677 degrees of freedom
#Multiple R-squared:  0.5388,	Adjusted R-squared:  0.5345 
#F-statistic: 125.1 on 25 and 2677 DF,  p-value: < 2.2e-16


anova(mod_5_A1.1)


#### listo, fin: nos quedamos con este modelo que incluye la iteración con veterano:pobreza



#antes de pasar al punto2 de regresión logística hay que validar los supuestos:
#normalidad
#homocedasticidad
























#############de acá abajo es historia#################

#MOD 444 - Modelo con todas las cuantitativas + BINARIA

mod_44<- lm(pje ~ edad+pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen +Este, data = data_dummy)
summary(mod_2)   

#Residual standard error: 8.201 on 2693 degrees of freedom
#Multiple R-squared:  0.3537,	Adjusted R-squared:  0.3513 
#F-statistic: 147.4 on 10 and 2693 DF,  p-value: < 2.2e-16


#MOD 3 - Modelo con todas las cuantitativas + BINARIA + Todas interacciones


mod_3<- lm(pje ~ edad+pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen +Este + ingpc:pobreza + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
             ingpc:ahorros + ahorros:edad + crimen:ancianos + veteranos:ahorros, data = data_dummy)
data_dummy$ingpc
summary(mod_3)   



#Residual standard error: 8.058 on 2684 degrees of freedom
#Multiple R-squared:  0.3781,	Adjusted R-squared:  0.3737 
#F-statistic: 85.88 on 19 and 2684 DF,  p-value: < 2.2e-16


#MOD 4 - Modelo con todas las cuantitativas + BINARIA + Todas interacciones con ingp eliminado


mod_4<- lm(pje ~ edad+pobreza + densidad + mujeres + ahorros + veteranos + ancianos + crimen +Este + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
             + ahorros:edad + crimen:ancianos + veteranos:ahorros, data = data_dummy)
summary(mod_4)   

#Residual standard error: 8.083 on 2687 degrees of freedom
#Multiple R-squared:  0.3734,	Adjusted R-squared:  0.3697 
#F-statistic: 100.1 on 16 and 2687 DF,  p-value: < 2.2e-16


#MOD 5 - Modelo con todas las cuantitativas + BINARIA + interacciones significativas


mod_5<- lm(pje ~ edad + ingpc + pobreza + densidad + mujeres + ahorros + veteranos + ancianos + crimen + Este + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
             + ahorros:edad + crimen:ancianos + veteranos:ahorros + ingpc:pobreza, data = data_dummy)
summary(mod_5)   

#Residual standard error: 8.083 on 2687 degrees of freedom
#Multiple R-squared:  0.3734,	Adjusted R-squared:  0.3697 
#F-statistic: 100.1 on 16 and 2687 DF,  p-value: < 2.2e-16

#no sirve

#MOD 6 - Modelo con algunas cuantitativas (saco veteranos, pobreza y ancianos) + BINARIA + interacciones significativas

mod_6<- lm(pje ~ edad + ingpc + densidad + mujeres + ahorros + crimen + Este + crimen:densidad +
             + ahorros:edad, data = data_dummy)
summary(mod_6)

#Residual standard error: 8.875 on 2694 degrees of freedom
#Multiple R-squared:  0.2427,	Adjusted R-squared:  0.2401 
#F-statistic: 95.91 on 9 and 2694 DF,  p-value: < 2.2e-16

# EN los modelos 5 y 6 eliminÃ© variables altamente correlacionadas y agreguÃ© algunas interacciones. 
# Performaron mal. 


#MOD 7 - Modelo con las cuantitativas significativas + BINARIA 

mod_7<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + Este, data = data_dummy)
summary(mod_7)

#Residual standard error: 8.206 on 2697 degrees of freedom
#Multiple R-squared:  0.3519,	Adjusted R-squared:  0.3504 
#F-statistic:   244 on 6 and 2697 DF,  p-value: < 2.2e-16

# IncluÃ­ las VA con los betas mÃ¡s significativos del primer anÃ¡lisis LM. Es un buen modelo porque tiene
# menos de 2pp de diferencia en el R2 y muchas variables menos. Me parece que es uno de los mÃ¡s potables.

#Diagnostico
par(mfrow = c(2, 2))
plot(mod_7)
par(mfrow = c(1, 1))

# Las medidas de diagnÃ³stico no son excelentes pero si aceptables. Seguimos viendo los outliers.

# Probamos igual pero eliminando los outliers.

#MOD 8 - Modelo con las cuantitativas significativas + BINARIA. ExclusiÃ³n de outliers. 

excluir <- c(1602, 2184, 948)
data_dummy_ol <- slice(data_dummy, -excluir)

mod_8<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + Este, data = data_dummy_ol)
summary(mod_8)

#Residual standard error: 8.095 on 2694 degrees of freedom
#Multiple R-squared:  0.3652,	Adjusted R-squared:  0.3638 
#F-statistic: 258.4 on 6 and 2694 DF,  p-value: < 2.2e-16

#Da mejor el R2, no demasiado significativo igual, A COSTA DE ELIMINAR 3 VARS (tenemos que ver que opinamos de esto).

#MOD 9 Arnaldo

## Paso estados a datos dummy sin cagar el resto de los dataset


data_dummy_2 <- cbind(data,clasif_estados) ##se pega bien

data_dummy_2$Regiones <- as.factor(data_dummy_2$Regiones) 

data_dummy_2$Este <- as.factor(data_dummy_2$Este) 


data_dummy_2 <- cbind(data_dummy_2, dummy(data_dummy_2$Regiones, sep ="_"))

data_dummy_2 <- cbind(data_dummy_2, dummy(data_dummy_2$estado, sep ="_"))

data_dummy_2$condado <- NULL
data_dummy_2$estado <- NULL
data_dummy_2$estado <- NULL
data_dummy_2$data_dummy_2_1 <- NULL
data_dummy_2$data_dummy_2_AL <- NULL
data_dummy_2$`Descrip Estado`<- NULL


#mÃ©todo backward para eexperimento

stepAIC(
  object = lm(pje ~ ., data = data_dummy_2), #punto de partida
  scope = list(upper = lm(pje ~ 1, data = data_dummy_2)), #mÃ¡ximo modelo posible
  direction = "backward", #mÃ©todo de selecciÃ³n
  trace = F #para no imprimir resultados parciales
)

#COPIAR Y PEGAR LA FORMULA


mod_9  <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + crimen + Regiones + Este + data_dummy_2_AR + data_dummy_2_CA + data_dummy_2_CO + 
                data_dummy_2_DC + data_dummy_2_DE + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                data_dummy_2_KS + data_dummy_2_LA + data_dummy_2_MA + data_dummy_2_ME + data_dummy_2_MI + data_dummy_2_MN + data_dummy_2_MO + 
                data_dummy_2_MS + data_dummy_2_NC + data_dummy_2_NE + data_dummy_2_NJ + data_dummy_2_NM + data_dummy_2_NY + data_dummy_2_OH + 
                data_dummy_2_OR + data_dummy_2_PA + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_2)
summary(mod_9)

#Residual standard error: 6.855 on 2663 degrees of freedom
#Multiple R-squared:  0.5535,	Adjusted R-squared:  0.5468 
#F-statistic: 82.52 on 40 and 2663 DF,  p-value: < 2.2e-16

plot(mod_9)

anova(mod_9)

excluir <- c(1602, 2184, 948,1297, 1871,1765,2356)
data_dummy_21 <- slice(data_dummy_2, -excluir)
mod_10  <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                 densidad + crimen + Regiones + Este + data_dummy_2_AR + data_dummy_2_CA + data_dummy_2_CO + 
                 data_dummy_2_DC + data_dummy_2_DE + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                 data_dummy_2_KS + data_dummy_2_LA + data_dummy_2_MA + data_dummy_2_ME + data_dummy_2_MI + data_dummy_2_MN + data_dummy_2_MO + 
                 data_dummy_2_MS + data_dummy_2_NC + data_dummy_2_NE + data_dummy_2_NJ + data_dummy_2_NM + data_dummy_2_NY + data_dummy_2_OH + 
                 data_dummy_2_OR + data_dummy_2_PA + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_21)
summary(mod_10)

#Residual standard error: 6.76 on 2656 degrees of freedom
#Multiple R-squared:  0.5633,	Adjusted R-squared:  0.5567 
#F-statistic: 85.64 on 40 and 2656 DF,  p-value: < 2.2e-16

plot(mod_10)


# MOD 11: variables incluidas en el anova del mod 9

mod_11  <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                 densidad + Regiones + Este + data_dummy_2_AR + data_dummy_2_CA + 
                 data_dummy_2_DC + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                 data_dummy_2_KS + data_dummy_2_MA + data_dummy_2_MN + data_dummy_2_MO + 
                 data_dummy_2_MS + data_dummy_2_NE + data_dummy_2_OH + data_dummy_2_PA + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_2)
summary(mod_11)

#Residual standard error: 6.953 on 2674 degrees of freedom
#Multiple R-squared:  0.5386,	Adjusted R-squared:  0.5336 
#F-statistic: 107.6 on 29 and 2674 DF,  p-value: < 2.2e-16

anova(mod_11)

# MOD 12: variables reducidas

mod_12 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Regiones + Este + data_dummy_2_AR + data_dummy_2_CA + 
                data_dummy_2_DC + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                data_dummy_2_KS + data_dummy_2_MA + data_dummy_2_MN + data_dummy_2_MO + 
                data_dummy_2_MS + data_dummy_2_NE + data_dummy_2_OH + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_2)
summary(mod_12)

# Residual standard error: 6.955 on 2675 degrees of freedom
# Multiple R-squared:  0.5382,	Adjusted R-squared:  0.5334 
# F-statistic: 111.3 on 28 and 2675 DF,  p-value: < 2.2e-16

# MOD 13: variables reducidas (saco este)

mod_13 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Regiones + data_dummy_2_AR + data_dummy_2_CA + 
                data_dummy_2_DC + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                data_dummy_2_KS + data_dummy_2_MA + data_dummy_2_MN + data_dummy_2_MO + 
                data_dummy_2_MS + data_dummy_2_NE + data_dummy_2_OH + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_2)
summary(mod_13)

#Residual standard error: 7.216 on 2676 degrees of freedom
#Multiple R-squared:  0.5028,	Adjusted R-squared:  0.4978 
#F-statistic: 100.2 on 27 and 2676 DF,  p-value: < 2.2e-16

# mod 14: variables reducidas (saco regiones)

mod_14 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Este + data_dummy_2_AR + data_dummy_2_CA + 
                data_dummy_2_DC + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                data_dummy_2_KS + data_dummy_2_MA + data_dummy_2_MN + data_dummy_2_MO + 
                data_dummy_2_MS + data_dummy_2_NE + data_dummy_2_OH + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_2)
summary(mod_14)

# Residual standard error: 6.961 on 2678 degrees of freedom
# Multiple R-squared:  0.5369,	Adjusted R-squared:  0.5326 
# F-statistic: 124.2 on 25 and 2678 DF,  p-value: < 2.2e-16

plot(mod_14)

# excluimos outliers

excluir <- c(1602, 948)
data_dummy_22 <- slice(data_dummy_2, -excluir)

# mod 15 -> Ã­dem anterior sin outliers


mod_15 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Este + data_dummy_2_AR + data_dummy_2_CA + 
                data_dummy_2_DC + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                data_dummy_2_KS + data_dummy_2_MA + data_dummy_2_MN + data_dummy_2_MO + 
                data_dummy_2_MS + data_dummy_2_NE + data_dummy_2_OH + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_22)
summary(mod_15)

#Residual standard error: 6.889 on 2676 degrees of freedom
#Multiple R-squared:  0.5451,	Adjusted R-squared:  0.5408 
#F-statistic: 128.3 on 25 and 2676 DF,  p-value: < 2.2e-16

plot(mod_15)

##########

anova(mod_9, mod_15)





#Diagnostico
par(mfrow = c(2, 2))
plot(mod_8)
par(mfrow = c(1, 1))

#Los supuestos andan...

# guardo coeficientes en un data.frame para despu?s
coeficientes <- tribble(
  ~"Modelo", ~"Intercepto", ~"Pendiente",
  "MCO", coef(mod_4)[1], coef(mod_4)[2] 
)
summary(mod_4)


#Gr?ficos de diagn?stico:
par(mfrow = c(2, 2))
plot(mod_4)
par(mfrow = c(1, 1))

#Del grafico de los residuos sin estandarizar se observa que a las obs 890 y 948 como muy alejada del patron del resto 
#La curva QQ esta bien salvo 2 extremos, las obs 948, 890 y 2184
#El leverage de la obs 1602 es muy elevado

#Medidas de diagn?stico:


mod4_df <- 
  tibble(
    id = 1:nrow(data),
    residuos = mod_4$residuals,
    leverage = hatvalues(mod_4),
    res_est = rstandard(mod_4),
    res_stu = rstudent(mod_4),
    res_press = residuos / (1 - leverage),
    cook = cooks.distance(mod_4)
  )

View(mod_4_df)

#fwrite (mod_4_df, "mod1_mco_df en xls_1.xlsx") ##cambiÃÂÃÂ© por fwrite (no entiendo para quÃÂÃÂ© hacemos el excel)

influencePlot(mod_4)

#studRes         Hat       CookD
#368  -3.1016834 0.127000549 0.082062846
#948  -4.5465255 0.018156198 0.022321587
#1070 -0.5303747 0.200262290 0.004144618
#1602 -1.0391714 0.974523633 2.429782783
#2184  4.0328505 0.003390942 0.003236762

influenceIndexPlot(mod_4)

#obs 1602 y 2184 pesan mucho



#mejor subconjunto del modelo 4

mejorsub <- regsubsets(
  x = pje ~ edad + ahorros + ingpc + pobreza + veteranos + mujeres + densidad + ancianos + crimen +
    Este + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
    + ahorros:edad + crimen:ancianos + veteranos:ahorros, 
  data = data_dummy
)

mejorsub_1 <- summary(mejorsub)
mejorsub_1

#        edad ahorros ingpc pobreza veteranos mujeres densidad ancianos crimen Este    edad:veteranos edad:ancianos ahorros:ancianos densidad:crimen edad:ahorros
#1  ( 1 ) " "  " "     " "   "*"     " "       " "     " "      " "      " "    " "          " "            " "           " "              " "             " "
#2  ( 1 ) " "  " "     " "   "*"     " "       " "     " "      " "      " "    "*"          " "            " "           " "              " "             " "
#3  ( 1 ) " "  " "     " "   "*"     " "       " "     "*"      " "      " "    "*"          " "            " "           " "              " "             " "
#4  ( 1 ) " "  " "     " "   "*"     " "       "*"     "*"      " "      " "    "*"          " "            " "           " "              " "             " "
#5  ( 1 ) " "  " "     " "   "*"     " "       "*"     "*"      " "      " "    "*"          " "            " "           " "              "*"             " " 
#6  ( 1 ) " "  " "     " "   "*"     "*"       "*"     "*"      " "      " "    "*"          " "            " "           " "              "*"             " "
#7  ( 1 ) " "  " "     " "   "*"     " "       "*"     "*"      " "      " "    "*"          "*"            " "           " "              "*"             "*"
#8  ( 1 ) " "  " "     " "   "*"     " "       "*"     "*"      "*"      " "    "*"          "*"            " "           "*"              "*"             " "  

#MOD 1 -> pobreza
#MOD 2 -> pobreza+ Este
#MOD 3 -> pobreza+ Este + Densidad
#MOD 4 -> pobreza+ Este + Densidad + mujeres 
#MOD 5 -> pobreza+ Este + Densidad + mujeres + densidad x crimen
#MOD 6 -> pobreza+ Este + Densidad + mujeres + densidad x crimen + veteranos
#MOD 7 -> pobreza+ Este + Densidad + mujeres + densidad x crimen + edad x veteranos + edad x ahorros -- se tiene que agregar la edad
#MOD 8 -> pobreza+ Este + Densidad + mujeres + densidad x crimen + edad x veteranos + ancianos+ edad x ahorros -- se tiene que agregar la edad



#metodo forward

as.data.frame(data_dummy)

data(data_dummy)

stepAIC(
  object = lm(pje ~ 1, data = data_dummy), #punto de partida
  scope = list(upper = lm(pje ~ ., data = data_dummy)), #maximo modelo posible
  direction = "forward", #mÃÂÃÂÃÂÃÂ©todo de selecciÃÂÃÂÃÂÃÂ³n
  trace = FALSE #para no imprimir resultados parciales
)

mod_forward <- lm(formula = pje ~ pobreza + Este + densidad + mujeres + veteranos + 
                    ahorros + data_dummy_4 + ingpc, data = data_dummy)

summary(mod_forward)

#Call:
#  lm(formula = pje ~ pobreza + `Este/Oeste` + densidad + mujeres + 
#       veteranos + ahorros + data_dummy_4 + ingpc, data = data_dummy)

#Coefficients:
#  (Intercept)            pobreza  `Este/Oeste`Oeste           densidad  
#-1.953e+01          8.128e-01         -3.281e+00          1.637e-03  
#mujeres          veteranos            ahorros       data_dummy_4  
#8.012e-01          4.948e-01         -2.814e-05         -1.820e+00  
#ingpc  
#2.172e-04 


#mÃ©todo backward

stepAIC(
  object = lm(pje ~ ., data = data_dummy), #punto de partida
  scope = list(upper = lm(pje ~ 1, data = data_dummy)), #mÃ¡ximo modelo posible
  direction = "backward", #mÃ©todo de selecciÃ³n
  trace = F #para no imprimir resultados parciales
)


mod_backward  <- lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                      densidad + Este + data_dummy_4, data = data_dummy)
summary(mod_backward)







#mejor subconjunto


mod_mejorsub2 <- regsubsets(
  x = pje ~ . , 
  data = data_dummy_largo
)

resumen <- summary(mod_mejorsub)
resumen



#Criterios de selecci?n de modelos
mejorsub_1$cp
mejorsub_1$adjr2
mejorsub_1$bic

#Gr?ficos para comparar ajustes
plot(mejorsub, scale = "Cp")
plot(mejorsub, scale = "adjr2")
plot(mejorsub, scale = "bic")


#MOD 1 -> pobreza
#MOD 2 -> pobreza+ Este
#MOD 3 -> pobreza+ Este + Densidad
#MOD 4 -> pobreza+ Este + Densidad + mujeres 
#MOD 5 -> pobreza+ Este + Densidad + mujeres + densidad x crimen
#MOD 6 -> pobreza+ Este + Densidad + mujeres + densidad x crimen + veteranos
#MOD 7 -> pobreza+ Este + Densidad + mujeres + densidad x crimen + edad x veteranos + edad x ahorros -- se tiene que agregar la edad
#MOD 8 -> pobreza+ Este + Densidad + mujeres + densidad x crimen + edad x veteranos + ancianos+ edad x ahorros -- se tiene que agregar la edad


m0 <- lm(pje ~ 1, data = data_dummy)
m1 <- lm(pje ~ pobreza, data = data_dummy)
m2 <- lm(pje ~ pobreza + Este, data = data_dummy)
m3 <- lm(pje ~ pobreza + Este + densidad, data = data_dummy)
m4 <- lm(pje ~ pobreza + Este + densidad + mujeres, data = data_dummy)
m5 <- lm(pje ~ pobreza + Este + densidad + mujeres + crimen + densidad:crimen, data = data_dummy)
m6 <- lm(pje ~ pobreza + Este + densidad + mujeres + crimen + densidad:crimen + edad +veteranos + edad:veteranos, data = data_dummy)
m7 <- lm(pje ~ pobreza + Este + densidad + mujeres + crimen + densidad:crimen + edad +veteranos + edad:veteranos + ahorros + edad:ahorros, data = data_dummy)
m8 <- lm(pje ~ pobreza + Este + densidad + mujeres + crimen + densidad:crimen + edad +veteranos + edad:veteranos + ahorros + edad:ahorros + ancianos, data = data_dummy)



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
#1  77.7 0.251 210230. 483.  11771. 11783.
#2  72.8 0.297 197265. 286.  11599. 11616.
#3  70.0 0.324 193236. 172.  11493. 11517.
#4  68.5 0.339 188752. 112.  11436. 11465.
#5  67.7 0.347 185014.  77.3 11403. 11444.
#6  67.1 0.353 183395.  57.8 11384. 11443.
#7  65.9 0.364 182743.  11.7 11338. 11409.
#8  65.9 0.364 182587.  13   11339. 11416

# a simple vista el mejor modelo es m7 o m6 

#ELIJO M6
m6 <- lm(pje ~ pobreza + Este + densidad + mujeres + crimen + densidad:crimen + edad +veteranos + edad:veteranos, data = data_dummy)

summary(m6)

#Residual standard error: 8.192 on 2694 degrees of freedom
#Multiple R-squared:  0.3548,	Adjusted R-squared:  0.3527 
#F-statistic: 164.6 on 9 and 2694 DF,  p-value: < 2.2e-16


# guardo coeficientes en un data.frame para despu?s
coeficientes <- tribble(
  ~"Modelo", ~"Intercepto", ~"Pendiente",
  "MCO", coef(m6)[1], coef(m6)[2] 
)
summary(m6)

# los graficos de los residuos no los pude hacer porque estÃ¡ metida la variable "Este"
influencePlot(m6)

#studRes         Hat       CookD
#180   0.09545452 0.151931925 0.0001632943
#948  -4.17442434 0.012091110 0.0211983723
#1546 -2.55760944 0.124408191 0.0927520124
#1602 -0.65308088 0.973947894 1.5948479827
#2184  3.86877184 0.001587441 0.0023674894


influenceIndexPlot(m6)

#obs 1602 y 2184 pesan mucho




############################################################

#Pruebo eliminando solo 1602 y 2184 del mod_4


mod_4<- lm(pje ~ edad+pobreza + densidad + mujeres + ahorros + veteranos + ancianos + crimen +Este + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
             + ahorros:edad + crimen:ancianos + veteranos:ahorros, data = data_dummy)
summary(mod_4)   


excluir <- c(1602, 2184)
data2 <- slice(data_dummy, -excluir)
mod4_elim<-lm(pje ~ edad+ahorros+ingpc+pobreza+veteranos+mujeres+densidad+ancianos+crimen +Este + veteranos:edad + ancianos:edad + ancianos:ahorros + crimen:densidad +
                + ahorros:edad + crimen:ancianos + veteranos:ahorros, data = data2 )

summary(mod4_elim)

#Residual standard error: 8.061 on 2684 degrees of freedom
#Multiple R-squared:  0.3731,	Adjusted R-squared:  0.3692 
#F-statistic: 93.98 on 17 and 2684 DF,  p-value: < 2.2e-16

# No hay mucha modificacion entre esta salida (elimando 2 obs) con la salida SIN eliminar las observaciones

#Comparaci?n de coeficientes:

coeficientes <- 
  coeficientes %>% 
  rbind(list("MCO 2", coef(mod4_elim)[1], coef(mod4_elim)[2]))
coeficientes


#Modelo Intercepto Pendiente
#* <chr>       <dbl>     <dbl>
#1 MCO         -18.9    0.789 
#2 MCO 2       -21.2   -0.0239

#Hay un cambio en el intercepto y en la pendiente


#Gr?ficos de diagn?stico mod2:
#par(mfrow = c(2, 2))
#plot(mod4_elim)
#par(mfrow = c(1, 1))
#tampoco se puede graficar por estar incluida la var dicotomoca


#Medidas de diagn?stico mod2:
mod4_elim_df <- 
  tibble(
    id = setdiff(1:2704, excluir),
    residuos = mod4_elim$residuals,
    leverage = hatvalues(mod4_elim),
    res_est = rstandard(mod4_elim),
    res_stu = rstudent(mod4_elim),
    res_press = residuos / (1 - leverage),
    cook = cooks.distance(mod4_elim)
  )

#View(mod2_mco_df)

#write.xlsx(mod2_mco_df, "mod2_mco_df en xls_1.xlsx")

influencePlot(mod4_elim)

#StudRes        Hat        CookD
#180  -0.4080998 0.327621639 0.0045097652
#368  -3.1095937 0.127062175 0.0779412243
#890  -3.7437367 0.008689021 0.0067920062
#948  -4.5800765 0.019489402 0.0229931612
#1393  0.1962870 0.219321171 0.0006015538
#1546 -2.6547857 0.161287724 0.0751272079


influenceIndexPlot(mod4_elim)






#############empecé el punto 2##########


###################################################################################
###################################################################################

#creo variable de clase a partir de pje


data_dummy_22$pje_class <- cut(data_dummy_22$pje, # Vector de entrada (numÃÂ©rico)
                        breaks=  c(min(data_dummy_22$pje), 50.000000, max(data_dummy_22$pje)),        # NÃÂºmero o vector con los cortes
                        labels = c(0,1),                           		# Etiquetas para cada grupo
)   

data$pje_class <- cut(data_dummy$pje, # Vector de entrada (numÃÂ©rico)
                            breaks=  c(min(data_dummy$pje), 50.000000, max(data_dummy$pje)),        # NÃÂºmero o vector con los cortes
                            labels = c(0,1),                           		# Etiquetas para cada grupo
)   

##voy a probar con el mod_15 suponiendo que son las variables que nos quedamos



#mod_15 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Este + data_dummy_2_AR + data_dummy_2_CA + 
                data_dummy_2_DC + data_dummy_2_FL + data_dummy_2_IA + data_dummy_2_ID + data_dummy_2_IL + data_dummy_2_IN + 
                data_dummy_2_KS + data_dummy_2_MA + data_dummy_2_MN + data_dummy_2_MO + 
                data_dummy_2_MS + data_dummy_2_NE + data_dummy_2_OH + data_dummy_2_TN + data_dummy_2_UT + data_dummy_2_WA, data = data_dummy_22)

)

install.packages("lmtest")
library(lmtest)
M0 <- glm(pje_class ~ ., family = binomial(link = "logit"), data = data)
M1 <- glm(pje_class ~ ., family = binomial(link = "logit"), data = data_dummy_22)

lrtest(M0, M1)


