#### TRANSFORMACIÓN DE INFORMACION

###Instalamos librerias necesarias
##install.packages("dplyr")

library(dplyr)
###################################

setwd("/") ####1 RAIZ
setwd("Users/D3nU4R/Desktop/tesis/survey analysis/") ####2 DIRECTORIO

####3 Leemos el archivo CSV limpio
survey <- read.csv("survey_limpio.csv", sep = ",", header = T)
summary(survey) #### chequeamos que lo muestre


################ influenciaEnRendimiento ################ NO HAY VALOR ATIPICO

df_perc_influenciaEnRendimiento <- as.data.frame(prop.table(table(survey$influenciaEnRendimiento)))
df_perc_influenciaEnRendimiento <- df_perc_influenciaEnRendimiento %>% arrange(-Freq)
boxplot(df_perc_influenciaEnRendimiento$Freq)
hist(df_perc_influenciaEnRendimiento$Freq)
qqnorm(df_perc_influenciaEnRendimiento$Freq)
################ e_cansancio ################ NO HAY VALOR ATIPICO la regresion lineal no pega
df_perc_e_cansancio <- as.data.frame(prop.table(table(survey$e_cansancio)))
df_perc_e_cansancio <- df_perc_e_cansancio %>% arrange(-Freq)
boxplot(df_perc_e_cansancio$Freq)
hist(df_perc_e_cansancio$Freq)
qqnorm(df_perc_e_cansancio$Freq)
################ e_dificultadDormir ################ NO HAY VALOR ATIPICO
df_perc_e_dificultadDormir <- as.data.frame(prop.table(table(survey$e_dificultadDormir)))
df_perc_e_dificultadDormir <- df_perc_e_dificultadDormir %>% arrange(-Freq)
boxplot(df_perc_e_dificultadDormir$Freq)
hist(df_perc_e_dificultadDormir$Freq)
qqnorm(df_perc_e_dificultadDormir$Freq)
################ e_noConsideracion ################ NO HAY VALOR ATIPICO
df_perc_e_noConsideracion <- as.data.frame(prop.table(table(survey$e_noConsideracion)))
df_perc_e_noConsideracion <- df_perc_e_noConsideracion %>% arrange(-Freq)
boxplot(df_perc_e_noConsideracion$Freq)
hist(df_perc_e_noConsideracion$Freq)
qqnorm(df_perc_e_noConsideracion$Freq)
################ e_noConsideracion ################ NO HAY VALOR ATIPICO
df_perc_e_irritabilidadExcesiva <- as.data.frame(prop.table(table(survey$e_irritabilidadExcesiva)))
df_perc_e_irritabilidadExcesiva <- df_perc_e_irritabilidadExcesiva %>% arrange(-Freq)
boxplot(df_perc_e_irritabilidadExcesiva$Freq)
hist(df_perc_e_irritabilidadExcesiva$Freq)
qqnorm(df_perc_e_irritabilidadExcesiva$Freq)
################ e_escasaConfianzaPropia ################ NO HAY VALOR ATIPICO
df_perc_e_escasaConfianzaPropia <- as.data.frame(prop.table(table(survey$e_escasaConfianzaPropia)))
df_perc_e_irritabilidadExcesiva <- df_perc_e_escasaConfianzaPropia %>% arrange(-Freq)
boxplot(df_perc_e_escasaConfianzaPropia$Freq)
hist(df_perc_e_escasaConfianzaPropia$Freq)
qqnorm(df_perc_e_escasaConfianzaPropia$Freq)
################ e_sensacionDeInutilidad ################ NO HAY VALOR ATIPICO
df_perc_e_sensacionDeInutilidad <- as.data.frame(prop.table(table(survey$e_sensacionDeInutilidad)))
df_perc_e_sensacionDeInutilidad <- df_perc_e_sensacionDeInutilidad %>% arrange(-Freq)
boxplot(df_perc_e_sensacionDeInutilidad$Freq)
hist(df_perc_e_sensacionDeInutilidad$Freq)
qqnorm(df_perc_e_sensacionDeInutilidad$Freq)
################ e_faltaDeEntusiasmo ################ NO HAY VALOR ATIPICO
df_perc_e_faltaDeEntusiasmo <- as.data.frame(prop.table(table(survey$e_faltaDeEntusiasmo)))
df_perc_e_faltaDeEntusiasmo <- df_perc_e_faltaDeEntusiasmo %>% arrange(-Freq)
boxplot(df_perc_e_faltaDeEntusiasmo$Freq)
hist(df_perc_e_faltaDeEntusiasmo$Freq)
qqnorm(df_perc_e_faltaDeEntusiasmo$Freq)
################ e_noControl ################ NO HAY VALOR ATIPICO
df_perc_e_noControl <- as.data.frame(prop.table(table(survey$e_noControl)))
df_perc_e_noControl <- df_perc_e_noControl %>% arrange(-Freq)
boxplot(df_perc_e_noControl$Freq)
hist(df_perc_e_noControl$Freq)
qqnorm(df_perc_e_noControl$Freq)
################ e_preocupacionExcesiva ################ NO HAY VALOR ATIPICO
df_perc_e_preocupacionExcesiva <- as.data.frame(prop.table(table(survey$e_preocupacionExcesiva)))
df_perc_e_preocupacionExcesiva <- df_perc_e_preocupacionExcesiva %>% arrange(-Freq)
boxplot(df_perc_e_preocupacionExcesiva$Freq)
hist(df_perc_e_preocupacionExcesiva$Freq)
qqnorm(df_perc_e_preocupacionExcesiva$Freq)
################ e_dificultadConsentracion ################ NO HAY VALOR ATIPICO
df_perc_e_dificultadConsentracion <- as.data.frame(prop.table(table(survey$e_dificultadConsentracion)))
df_perc_e_dificultadConsentracion <- df_perc_e_dificultadConsentracion %>% arrange(-Freq)
boxplot(df_perc_e_dificultadConsentracion$Freq)
hist(df_perc_e_dificultadConsentracion$Freq)
qqnorm(df_perc_e_dificultadConsentracion$Freq)
################ e_olvidosFrecuentes ################ NO HAY VALOR ATIPICO
df_perc_e_olvidosFrecuentes <- as.data.frame(prop.table(table(survey$e_olvidosFrecuentes)))
df_perc_e_olvidosFrecuentes <- df_perc_e_olvidosFrecuentes %>% arrange(-Freq)
boxplot(df_perc_e_olvidosFrecuentes$Freq)
hist(df_perc_e_olvidosFrecuentes$Freq)
qqnorm(df_perc_e_olvidosFrecuentes$Freq)
################ e_aislamiento ################ NO HAY VALOR ATIPICO
df_perc_e_aislamiento <- as.data.frame(prop.table(table(survey$e_aislamiento)))
df_perc_e_aislamiento <- df_perc_e_aislamiento %>% arrange(-Freq)
boxplot(df_perc_e_aislamiento$Freq)
hist(df_perc_e_aislamiento$Freq)
qqnorm(df_perc_e_aislamiento$Freq)
################ e_escasoDesempeno ################ NO HAY VALOR ATIPICO
df_perc_e_escasoDesempeno <- as.data.frame(prop.table(table(survey$e_escasoDesempeno)))
df_perc_e_escasoDesempeno <- df_perc_e_escasoDesempeno %>% arrange(-Freq)
boxplot(df_perc_e_escasoDesempeno$Freq)
hist(df_perc_e_escasoDesempeno$Freq)
qqnorm(df_perc_e_escasoDesempeno$Freq)
################ e_dificultadAceptarResp ################ NO HAY VALOR ATIPICO
df_perc_e_dificultadAceptarResp <- as.data.frame(prop.table(table(survey$e_dificultadAceptarResp)))
df_perc_e_dificultadAceptarResp <- df_perc_e_dificultadAceptarResp %>% arrange(-Freq)
boxplot(df_perc_e_dificultadAceptarResp$Freq)
hist(df_perc_e_dificultadAceptarResp$Freq)
qqnorm(df_perc_e_dificultadAceptarResp$Freq)
################ e_dificultadAceptarResp ################ NO HAY VALOR ATIPICO
df_perc_e_mantenerRespon <- as.data.frame(prop.table(table(survey$e_mantenerRespon)))
df_perc_e_mantenerRespon <- df_perc_e_mantenerRespon %>% arrange(-Freq)
boxplot(df_perc_e_mantenerRespon$Freq)
hist(df_perc_e_mantenerRespon$Freq)
qqnorm(df_perc_e_mantenerRespon$Freq)
################ e_mantenerRespon ################ NO HAY VALOR ATIPICO
df_perc_e_mantenerRespon <- as.data.frame(prop.table(table(survey$e_mantenerRespon)))
df_perc_e_mantenerRespon <- df_perc_e_mantenerRespon %>% arrange(-Freq)
boxplot(df_perc_e_mantenerRespon$Freq)
hist(df_perc_e_mantenerRespon$Freq)
qqnorm(df_perc_e_mantenerRespon$Freq)
################ e_abandonarClases ################ SI  HAY VALOR ATIPICO
df_perc_e_abandonarClases <- as.data.frame(prop.table(table(survey$e_abandonarClases)))
df_perc_e_abandonarClases <- df_perc_e_abandonarClases %>% arrange(-Freq)
boxplot(df_perc_e_abandonarClases$Freq)
hist(df_perc_e_abandonarClases$Freq)
qqnorm(df_perc_e_abandonarClases$Freq)


