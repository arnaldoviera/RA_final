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



#Cargamos el dataset
#dos aca cada uno pone si ingreso de la data
#Cadauno usa su directorio
#setwd("C:/Users/vieraa/Documents/GitHub/RA_final" )   
#setwd("C:/Users/quintej/Desktop/MCD/Regresion Avanzada/" )   #establezco la carpeta donde voy a trabajar
#setwd("C:/Users/jgiberti/Documents/Univ. Austral - Maestria en Ciencias de Datos/10. Regresion Avanzada/TP/" )   
setwd("C:/Users/isant/OneDrive/Desktop/MCD/Regresión Avanzada/TP Final")

data <- fread("clinton.txt")

#yo en mi PC le cambie la columna este/oeste por este solo. Si es 1, es del este si es 0 es del oeste
clasif_estados <- read_delim("clasif_estados.csv", 
                             ";", escape_double = FALSE, trim_ws = TRUE)

#creo un dataset sacando los estados; me pregunto, vale la pena hacer un one hot encoding con los estados?
# IRU -> A mi me parece que, si Brooklyn estÃÂ¡ siendo una observaciÃÂ³n influyente (NO outlier), quizÃÂ¡s tenga sentido.
# De todas maneras, habrÃÂ?a que ver cÃÂ³mo performan ambos modelos y quedarnos con el mejor. 
# Podemos plantear ambos. 
# dejo la consigna: "A travÃÂ©s de un modelo de RegresiÃÂ³n Lineal MÃÂºltiple ajustado por MÃÂ?nimos Cuadrados Ordinarios,
#estudiar el porcentaje de votos obtenidos por el candidato Bill Clinton en cada uno de los condados
#estadounidenses. Pueden incorporar como explicativas a cualquiera de las restantes variables presentes en
#la base"



## Sacamos estado y condado de los dataset

data1 <- dplyr::select(data, pje:crimen)




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




## Paso estados a datos dummy
data_dummy <- data

data_dummy<- cbind(data,clasif_estados) ##se pega bien

data_dummy$Regiones <- as.factor(data_dummy$Regiones) 

data_dummy$Este <- as.factor(data_dummy$Este) 


data_dummy <- cbind(data_dummy, dummy(data_dummy$Regiones, sep ="_"))

#data_dummy_largo <- cbind(data, dummy_dummy(data$estado, sep ="_"))


##Borro lo que no necesito:
data_dummy$estado <- NULL
data_dummy$estado <- NULL
data_dummy$condado <- NULL
data_dummy$`Descrip Estado` <- NULL
data_dummy$Regiones <- NULL

#borro 1 dummy por cada clase de variables que transformÃ©:

data_dummy$data_dummy_1 <- NULL #a la mierda regiÃ³n 1

#MOD 1 - Modelo con todas las cuantitativas + Regiones

mod_1<- lm(pje ~ edad+pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen +data_dummy_2 + data_dummy_3 + data_dummy_4, data = data_dummy)
summary(mod_1)   

#Residual standard error: 8.28 on 2691 degrees of freedom
#Multiple R-squared:  0.3416,	Adjusted R-squared:  0.3387 
#F-statistic: 116.4 on 12 and 2691 DF,  p-value: < 2.2e-16


#MOD 2 - Modelo con todas las cuantitativas + BINARIA

mod_2<- lm(pje ~ edad+pobreza + densidad + mujeres + ahorros + veteranos + ancianos + ingpc + crimen +Este, data = data_dummy)
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

# EN los modelos 5 y 6 eliminé variables altamente correlacionadas y agregué algunas interacciones. 
# Performaron mal. 


#MOD 7 - Modelo con las cuantitativas significativas + BINARIA 

mod_7<- lm(pje ~ pobreza + densidad + mujeres + ahorros + veteranos + Este, data = data_dummy)
summary(mod_7)

#Residual standard error: 8.206 on 2697 degrees of freedom
#Multiple R-squared:  0.3519,	Adjusted R-squared:  0.3504 
#F-statistic:   244 on 6 and 2697 DF,  p-value: < 2.2e-16

# Incluí las VA con los betas más significativos del primer análisis LM. Es un buen modelo porque tiene
# menos de 2pp de diferencia en el R2 y muchas variables menos. Me parece que es uno de los más potables.

#Diagnostico
par(mfrow = c(2, 2))
plot(mod_7)
par(mfrow = c(1, 1))

# Las medidas de diagnóstico no son excelentes pero si aceptables. Seguimos viendo los outliers.

# Probamos igual pero eliminando los outliers.

#MOD 8 - Modelo con las cuantitativas significativas + BINARIA. Exclusión de outliers. 

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


ddl <- cbind(data,clasif_estados) ##se pega bien

ddl$Regiones <- as.factor(ddl$Regiones) 

ddl$Este <- as.factor(ddl$Este) 


ddl <- cbind(ddl, dummy(ddl$Regiones, sep ="_"))

ddl <- cbind(ddl, dummy(ddl$estado, sep ="_"))

ddl$condado <- NULL
ddl$estado <- NULL
ddl$estado <- NULL
ddl$ddl_1 <- NULL
ddl$ddl_AL <- NULL
ddl$`Descrip Estado`<- NULL

#método backward para eexperimento

stepAIC(
  object = lm(pje ~ ., data = ddl), #punto de partida
  scope = list(upper = lm(pje ~ 1, data = ddl)), #máximo modelo posible
  direction = "backward", #método de selección
  trace = F #para no imprimir resultados parciales
)

#COPIAR Y PEGAR LA FORMULA


mod_9  <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + crimen + Regiones + Este + ddl_AR + ddl_CA + ddl_CO + 
                ddl_DC + ddl_DE + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                ddl_KS + ddl_LA + ddl_MA + ddl_ME + ddl_MI + ddl_MN + ddl_MO + 
                ddl_MS + ddl_NC + ddl_NE + ddl_NJ + ddl_NM + ddl_NY + ddl_OH + 
                ddl_OR + ddl_PA + ddl_TN + ddl_UT + ddl_WA, data = ddl)
summary(mod_9)

#Residual standard error: 6.855 on 2663 degrees of freedom
#Multiple R-squared:  0.5535,	Adjusted R-squared:  0.5468 
#F-statistic: 82.52 on 40 and 2663 DF,  p-value: < 2.2e-16

plot(mod_9)

anova(mod_9)

excluir <- c(1602, 2184, 948,1297, 1871,1765,2356)
ddl1 <- slice(ddl, -excluir)
mod_10  <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                 densidad + crimen + Regiones + Este + ddl_AR + ddl_CA + ddl_CO + 
                 ddl_DC + ddl_DE + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                 ddl_KS + ddl_LA + ddl_MA + ddl_ME + ddl_MI + ddl_MN + ddl_MO + 
                 ddl_MS + ddl_NC + ddl_NE + ddl_NJ + ddl_NM + ddl_NY + ddl_OH + 
                 ddl_OR + ddl_PA + ddl_TN + ddl_UT + ddl_WA, data = ddl1)
summary(mod_10)

#Residual standard error: 6.76 on 2656 degrees of freedom
#Multiple R-squared:  0.5633,	Adjusted R-squared:  0.5567 
#F-statistic: 85.64 on 40 and 2656 DF,  p-value: < 2.2e-16

plot(mod_10)

# MOD 11: variables incluidas en el anova del mod 9

mod_11  <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Regiones + Este + ddl_AR + ddl_CA + 
                ddl_DC + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                ddl_KS + ddl_MA + ddl_MN + ddl_MO + 
                ddl_MS + ddl_NE + ddl_OH + ddl_PA + ddl_TN + ddl_UT + ddl_WA, data = ddl)
summary(mod_11)

#Residual standard error: 6.953 on 2674 degrees of freedom
#Multiple R-squared:  0.5386,	Adjusted R-squared:  0.5336 
#F-statistic: 107.6 on 29 and 2674 DF,  p-value: < 2.2e-16

anova(mod_11)

# MOD 12: variables reducidas

mod_12 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                 densidad + Regiones + Este + ddl_AR + ddl_CA + 
                 ddl_DC + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                 ddl_KS + ddl_MA + ddl_MN + ddl_MO + 
                 ddl_MS + ddl_NE + ddl_OH + ddl_TN + ddl_UT + ddl_WA, data = ddl)
summary(mod_12)

# Residual standard error: 6.955 on 2675 degrees of freedom
# Multiple R-squared:  0.5382,	Adjusted R-squared:  0.5334 
# F-statistic: 111.3 on 28 and 2675 DF,  p-value: < 2.2e-16

# MOD 13: variables reducidas (saco este)

mod_13 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Regiones + ddl_AR + ddl_CA + 
                ddl_DC + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                ddl_KS + ddl_MA + ddl_MN + ddl_MO + 
                ddl_MS + ddl_NE + ddl_OH + ddl_TN + ddl_UT + ddl_WA, data = ddl)
summary(mod_13)

#Residual standard error: 7.216 on 2676 degrees of freedom
#Multiple R-squared:  0.5028,	Adjusted R-squared:  0.4978 
#F-statistic: 100.2 on 27 and 2676 DF,  p-value: < 2.2e-16

# mod 14: variables reducidas (saco regiones)

mod_14 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Este + ddl_AR + ddl_CA + 
                ddl_DC + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                ddl_KS + ddl_MA + ddl_MN + ddl_MO + 
                ddl_MS + ddl_NE + ddl_OH + ddl_TN + ddl_UT + ddl_WA, data = ddl)
summary(mod_14)

# Residual standard error: 6.961 on 2678 degrees of freedom
# Multiple R-squared:  0.5369,	Adjusted R-squared:  0.5326 
# F-statistic: 124.2 on 25 and 2678 DF,  p-value: < 2.2e-16

plot(mod_14)

# excluimos outliers

excluir <- c(1602, 948)
ddl2 <- slice(ddl, -excluir)

# mod 15 -> ídem anterior sin outliers

mod_15 <-  lm(formula = pje ~ ahorros + ingpc + pobreza + veteranos + mujeres + 
                densidad + Este + ddl_AR + ddl_CA + 
                ddl_DC + ddl_FL + ddl_IA + ddl_ID + ddl_IL + ddl_IN + 
                ddl_KS + ddl_MA + ddl_MN + ddl_MO + 
                ddl_MS + ddl_NE + ddl_OH + ddl_TN + ddl_UT + ddl_WA, data = ddl2)
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

#View(mod_4_df)

#fwrite (mod_4_df, "mod1_mco_df en xls_1.xlsx") ##cambiÃÂ© por fwrite (no entiendo para quÃÂ© hacemos el excel)

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
  direction = "forward", #mÃÂÃÂ©todo de selecciÃÂÃÂ³n
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


#método backward

stepAIC(
  object = lm(pje ~ ., data = data_dummy), #punto de partida
  scope = list(upper = lm(pje ~ 1, data = data_dummy)), #máximo modelo posible
  direction = "backward", #método de selección
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

# los graficos de los residuos no los pude hacer porque está metida la variable "Este"
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







#===============
#TransformaciÃ³n: nuevo dataset con logaritmos

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



summary(Dlog)
data1_Dlog <- dplyr::select(Dlog, pje:crimen)

multi.hist(x = data1_Dlog, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = NULL)


#Visualizo que relacion tiene cada variable con la respuesta
mfrow = c(2, 2)
data1_Dlog %>%
  ggplot(aes(Dlog$edad, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Edad", y = "PJE")

data1_Dlog %>%
  ggplot(aes(Dlog$ahorros, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Ahorros", y = "PJE")


data1_Dlog %>%
  ggplot(aes(Dlog$ingpc, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log ingpc", y = "PJE")


data1_Dlog %>%
  ggplot(aes(Dlog$pobreza, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Pobreza", y = "PJE")


data1_Dlog %>%
  ggplot(aes(Dlog$veteranos, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Veteranos", y = "PJE")


data1_Dlog %>%
  ggplot(aes(Dlog$mujeres, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Mujeres", y = "PJE")

data1_Dlog %>%
  ggplot(aes(Dlog$ancianos, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Ancianos", y = "PJE")


data1_Dlog %>%
  ggplot(aes(Dlog$crimen, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Crimen", y = "PJE")



data1_Dlog %>%
  ggplot(aes(Dlog$densidad, pje)) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x = "Log Densidad", y = "PJE")






#===============
#TransformaciÃ³n: nuevodataset escalado

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

summary(DS)

data1_DS <- dplyr::select(DS, pje:crimen)


multi.hist(x = data1_DS, dcol = c("blue", "red"), dlty = c("dotted", "solid"),
           main = NULL)














###################################################################################
###################################################################################

#acÃ¡ tomo variables contÃ?nuas de interes y hago dymmys a partir de la misma variable.

#parÃ¡metros de las fÃ³rmulas

x1 <- 0.0000001
x3 <- 999999999
Quant <- 0.8

#pobreza

data_dummy$pobrezadummieMean <- cut(data_dummy$pobreza, # Vector de entrada (numÃ©rico)
                                    breaks=  c(x1, mean(data_dummy$pobreza),x3),        # NÃºmero o vector con los cortes
                                    labels = c(0,1),                           		# Etiquetas para cada grupo
)   
data_dummy$pobrezadummieMedian <- cut(data_dummy$pobreza,   # Vector de entrada (numÃ©rico)
                                      breaks=  c(x1, median(data_dummy$pobreza),x3),                     
                                      labels = c(0,1),                           
)   

data_dummy$pobrezadummieQuantile <- cut(data_dummy$pobreza,   # Vector de entrada (numÃ©rico)
                                        breaks=  c(x1, quantile(x=data_dummy$pobreza, probs=Quant) ,x3),                     
                                        labels = c(0,1),    
)   



#ahorros

data_dummy$ahorrosdummieMean <- cut(data_dummy$ahorros, 
                                    breaks=  c(x1, mean(data_dummy$ahorros),x3),        
                                    labels = c(0,1),                           		
)   
data_dummy$ahorrosdummieMedian <- cut(data_dummy$ahorros,   # Vector de entrada (numÃ©rico)
                                      breaks=  c(x1, median(data_dummy$ahorros),x3),                     
                                      labels = c(0,1),                           
)   

data_dummy$ahorrosdummieQuantile <- cut(data_dummy$ahorros,   # Vector de entrada (numÃ©rico)
                                        breaks=  c(x1, quantile(x=data_dummy$ahorros, probs=Quant) ,x3),                     
                                        labels = c(0,1),    
) 


#ingpc

data_dummy$ingpcdummieMean <- cut(data_dummy$ingpc, 
                                  breaks=  c(x1, mean(data_dummy$ingpc),x3),        
                                  labels = c(0,1),                           		
)   
data_dummy$ingpcdummieMedian <- cut(data_dummy$ingpc,   # Vector de entrada (numÃ©rico)
                                    breaks=  c(x1, median(data_dummy$ingpc),x3),                     
                                    labels = c(0,1),                           
)   

data_dummy$ingpcdummieQuantile <- cut(data_dummy$ingpc,   # Vector de entrada (numÃ©rico)
                                      breaks=  c(x1, quantile(x=data_dummy$ingpc, probs=Quant) ,x3),                     
                                      labels = c(0,1),    
)   

#veteranos

data_dummy$veteranosdummieMean <- cut(data_dummy$veteranos, 
                                      breaks=  c(x1, mean(data_dummy$veteranos),x3),        
                                      labels = c(0,1),                           		
)   
data_dummy$veteranosdummieMedian <- cut(data_dummy$veteranos,   # Vector de entrada (numÃ©rico)
                                        breaks=  c(x1, median(data_dummy$veteranos),x3),                     
                                        labels = c(0,1),                           
)   

data_dummy$veteranosdummieQuantile <- cut(data_dummy$veteranos,   # Vector de entrada (numÃ©rico)
                                          breaks=  c(x1, quantile(x=data_dummy$veteranos, probs=Quant) ,x3),                     
                                          labels = c(0,1),    
)   

#mujeres

data_dummy$mujeresdummieMean <- cut(data_dummy$mujeres, 
                                    breaks=  c(x1, mean(data_dummy$mujeres),x3),        
                                    labels = c(0,1),                           		
)   
data_dummy$mujeresdummieMedian <- cut(data_dummy$mujeres,   # Vector de entrada (numÃ©rico)
                                      breaks=  c(x1, median(data_dummy$mujeres),x3),                     
                                      labels = c(0,1),                           
)   

data_dummy$mujeresdummieQuantile <- cut(data_dummy$mujeres,   # Vector de entrada (numÃ©rico)
                                        breaks=  c(x1, quantile(x=data_dummy$mujeres, probs=Quant) ,x3),                     
                                        labels = c(0,1),    
)   

#ancianos

data_dummy$ancianosdummieMean <- cut(data_dummy$ancianos, 
                                     breaks=  c(x1, mean(data_dummy$ancianos),x3),        
                                     labels = c(0,1),                           		
)   
data_dummy$ancianosdummieMedian <- cut(data_dummy$ancianos,   # Vector de entrada (numÃ©rico)
                                       breaks=  c(x1, median(data_dummy$ancianos),x3),                     
                                       labels = c(0,1),                           
)   

data_dummy$ancianosdummieQuantile <- cut(data_dummy$ancianos,   # Vector de entrada (numÃ©rico)
                                         breaks=  c(x1, quantile(x=data_dummy$ancianos, probs=Quant) ,x3),                     
                                         labels = c(0,1),    
)   




#################################################################
#################################################################

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
