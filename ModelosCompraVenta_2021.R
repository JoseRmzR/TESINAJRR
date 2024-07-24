##Paquetes##
library(dplyr)
library(ggplot2)
library(bdpv)
library(pROC)
library(tidyverse)
library(haven)      #Lectura de datos
library(sjmisc)     #Utilidades para encuestas con etiquetas
library(sjPlot)     #Graficos y tablas con etiquetas
library(broom)      #Convierte en tibbles los resultados de modelos
library(survey)     #Analisis de encuestas complejas
library(srvyr)      #Funciones survey a la tidyverse
library(knitr)      #Tablas con kable
library(sandwich)   #Varianzas-covarianzas
library(lmtest)     #Varianzas-covarianzas
library(car)        #Inferencia de modelos

data_21 <- read.table("C:/Users/joeyt/Documents/Cursos/Tesina/DatosAnalisis_2021.csv",header = T, sep = ",")

data_21 <- rename(U_TVP = P5_1
                  ,U_Dis = P6_1
                  ,U_D_F = P6_4_G
                  ,U_Int = P7_1
                  ,U_I_F = P7_3_G
                  ,U_I_A = P7_6_G
                  ,U_R_S = P7_13
                  ,U_Ven = P7_17
                  ,U_Com = P7_19
                  ,U_Pag = P7_27
                  ,U_B_E = P7_32
                  ,U_Cel = P8
                  ,U_C_F = P8_16_G
                  ,U_Rad = P9_1
                  ,U_TVA = P9_8
                  ,.data = data_21)

attach(data_21)


## Ajuste de los datos tomando en cuenta los pesos
datadis_21 = data_21 %>% mutate(across(one_of("EDAD_G","DOMINIO","ESTRATO"
  ,"NIVEL_G","NIVEL_GV","U_D_F","U_I_F","U_I_A","U_C_F")
,as_factor)) %>% 
  
  svydesign(ids=UPM_DIS
            ,strata=EST_DIS
            ,weights=FAC_PER
            ,data=.
            ,nest=TRUE)


###COMPRAS###

##Modelo grande para las compras
full.modelcom_21 <- svyglm(U_Com~SEXO+EDAD_G+ESTRATO+DOMINIO+NIVEL_G+U_TVP
                          +U_Dis+U_D_F
                          +U_Int+U_I_F
                          +U_I_A
                          +U_R_S+U_Pag+U_B_E
                          +U_Cel+U_C_F
                          +U_Rad+U_TVA
                          ,design=datadis_21,family=quasibinomial())

summary(full.modelcom_21)

##Seleccion de variables con el metodo backwards para las compras
modbackcom_21 <- step(full.modelcom_21, direction="backward",trace=1)

summary(modbackcom_21)

Anova(modbackcom_21)


###Modelo final para las compras###


modcomp_21 <- svyglm(U_Com~EDAD_G+ESTRATO+DOMINIO+NIVEL_G
                     +U_TVP+U_Dis+U_R_S+U_B_E+U_Cel+U_Rad+U_TVA
                     ,design=datadis_21
                     ,family=binomial())

summary(modcomp_21)

Anova(modcomp_21)

##Odds Ratios del modelo de compras
tab_model(modcomp_21,df.method="wald",show.reflvl=TRUE,show.aic=TRUE,show.aicc=TRUE)

##Curva ROC
y_obs <- as.factor(data_21$U_Com)
par(pty = "s") #Hacer cuadrado el espacio ROC

roc_graph <- roc(y_obs, modcomp_21$fitted.values, plot = TRUE, legacy.axes = TRUE,
                 percent = TRUE, ylab = "Sensitividad \n (Porcentaje de verdaderos positivos)",
                 xlab = "1- especificidad \n (Porcentaje de falsos positivos)", col = "darkblue", lwd = 2,
                 print.auc = TRUE, auc.polygon = TRUE, auc.polygon.col = "aliceblue", grid=TRUE)
coordenada <- pROC::coords(roc_graph,"best",ret=c("threshold","specificity","sensitivity"))
points(x=coordenada$specificity, y=coordenada$sensitivity, pch=0,col="red")
roc_graph

coordenada

##Clasificacion de las compras
prediccomp_21 <- ifelse(test = modcomp_21$fitted.values > 0.2158, yes = 1, no = 0)

matrizconfcomp_21 <- table(modcomp_21$model$U_Com, prediccomp_21,
                           dnn = c("observaciones", "predicciones"))
matrizconfcomp_21


###VENTAS###

##Modelo grande para las ventas
full.modelvent_21 <- svyglm(U_Ven~SEXO+EDAD_G+ESTRATO+DOMINIO+NIVEL_G+U_TVP+U_Dis+U_D_F
                            +U_Int+U_I_F+U_I_A+U_R_S+U_Pag+U_B_E+U_Com
                            +U_Cel+U_C_F+U_Rad+U_TVA
                            ,design=datadis_21,family=quasibinomial())

summary(full.modelvent_21)

##Seleccion de variables con el metodo backwards para las ventas
modbackvent_21 <- step(full.modelvent_21, direction="backward",trace=1)

summary(modbackvent_21)

Anova(modbackvent_21)


###Modelo final para las ventas###


modvent_21 <- svyglm(U_Ven~SEXO+EDAD_G+DOMINIO+NIVEL_G
                       +U_R_S+U_Pag+U_B_E+U_Com+U_Rad
                       ,design=datadis_21
                       ,family=quasibinomial())

summary(modvent_21)

Anova(modvent_21)

##Odds Ratios del modelo de ventas
tab_model(modvent_21,df.method="wald",show.reflvl=TRUE,show.aic=TRUE,show.aicc=TRUE)

##Curva ROC
y_obs <- as.factor(data_21$U_Ven)
par(pty = "s") #Hacer cuadrado el espacio ROC

roc_graph <- roc(y_obs, modvent_21$fitted.values, plot = TRUE, legacy.axes = TRUE,
                 percent = TRUE, ylab = "Sensitividad \n (Porcentaje de verdaderos positivos)",
                 xlab = "1- especificidad \n (Porcentaje de falsos positivos)", col = "darkblue", lwd = 2,
                 print.auc = TRUE, auc.polygon = TRUE, auc.polygon.col = "aliceblue", grid=TRUE)
coordenada <- pROC::coords(roc_graph,"best",ret=c("threshold","specificity","sensitivity"))
points(x=coordenada$specificity, y=coordenada$sensitivity, pch=0,col="red")
roc_graph

coordenada

##Clasificacion de las ventas
predicvent_21 <- ifelse(test = modvent_21$fitted.values > 0.0773, yes = 1, no = 0)

matrizconfvent_21 <- table(modvent_21$model$U_Ven, predicvent_21,
                             dnn = c("observaciones", "predicciones"))
matrizconfvent_21
