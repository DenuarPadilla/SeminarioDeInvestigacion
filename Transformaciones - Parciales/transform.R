#### TRANSFORMACIÓN DE INFORMACION

###Instalamos librerias necesarias
#install.packages("dplyr")

library(dplyr)
###################################

setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv/") ####2 DIRECTORIO

####3 
survey <- read.csv("survey_limpio.csv", sep = ",", header = T)
summary(survey) #### chequeamos que lo mLeemos el archivo CSV limpiouestre


################ e_escasaConfianzaPropia ################ NO HAY VALOR ATIPICO
##                          Var1        Freq
##1                     De acuerdo 0.296089385
##2                  En desacuerdo 0.245810056
##3                 Muy de acuerdo 0.245810056
##4 Ni de acuerdo ni en desacuerdo 0.178770950
##5              Muy en desacuerdo 0.027932961
##6                                0.005586592
##Var1      Freq
##1    3 0.5449438
##2    1 0.2752809
##3    2 0.1797753
length(names(survey))
df_perc_e_escasaConfianzaPropia <- as.data.frame(prop.table(table(survey$e_escasaConfianzaPropia)))
df_perc_e_escasaConfianzaPropia <- df_perc_e_escasaConfianzaPropia %>% arrange(-Freq)
df_perc_e_escasaConfianzaPropia
df_perc_e_escasaConfianzaPropia[df_perc_e_escasaConfianzaPropia$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_escasaConfianzaPropia[df_perc_e_escasaConfianzaPropia$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_escasaConfianzaPropia[df_perc_e_escasaConfianzaPropia$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_escasaConfianzaPropia <- df_perc_e_escasaConfianzaPropia %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_escasaConfianzaPropia,by=c("e_escasaConfianzaPropia"="Var1"))
survey <- survey[,!names(survey)%in% c("e_escasaConfianzaPropia")]
names(survey)[33] <- "e_escasaConfianzaPropia"
names(survey)
df_perc_e_escasaConfianzaPropia <- as.data.frame(prop.table(table(survey$e_escasaConfianzaPropia)))
df_perc_e_escasaConfianzaPropia <- df_perc_e_escasaConfianzaPropia %>% arrange(-Freq)
df_perc_e_escasaConfianzaPropia
survey$e_escasaConfianzaPropia <- as.factor(survey$e_escasaConfianzaPropia)
summary(survey$e_escasaConfianzaPropia)
boxplot(df_perc_e_escasaConfianzaPropia$Freq)
hist(df_perc_e_escasaConfianzaPropia$Freq)
qqnorm(df_perc_e_escasaConfianzaPropia$Freq)
################ e_sensacionDeInutilidad ################ NO HAY VALOR ATIPICO
##Var1        Freq
##1                     De acuerdo 0.251396648
##2                 Muy de acuerdo 0.245810056
##3 Ni de acuerdo ni en desacuerdo 0.223463687
##4                  En desacuerdo 0.195530726
##5              Muy en desacuerdo 0.078212291
##6                                0.005586592
##Var1      Freq
##1    3 0.5000000
##2    1 0.2752809
##3    2 0.2247191
df_perc_e_sensacionDeInutilidad <- as.data.frame(prop.table(table(survey$e_sensacionDeInutilidad)))
df_perc_e_sensacionDeInutilidad <- df_perc_e_sensacionDeInutilidad %>% arrange(-Freq)
df_perc_e_sensacionDeInutilidad
df_perc_e_sensacionDeInutilidad[df_perc_e_sensacionDeInutilidad$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_sensacionDeInutilidad[df_perc_e_sensacionDeInutilidad$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_sensacionDeInutilidad[df_perc_e_sensacionDeInutilidad$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_sensacionDeInutilidad <- df_perc_e_sensacionDeInutilidad %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_sensacionDeInutilidad,by=c("e_sensacionDeInutilidad"="Var1"))
survey <- survey[,!names(survey)%in% c("e_sensacionDeInutilidad")]
names(survey)[33] <- "e_sensacionDeInutilidad"
names(survey)
df_perc_e_sensacionDeInutilidad <- as.data.frame(prop.table(table(survey$e_sensacionDeInutilidad)))
df_perc_e_sensacionDeInutilidad <- df_perc_e_sensacionDeInutilidad %>% arrange(-Freq)
df_perc_e_sensacionDeInutilidad
survey$e_sensacionDeInutilidad <- as.factor(survey$e_sensacionDeInutilidad)
summary(survey$e_sensacionDeInutilidad)
boxplot(df_perc_e_sensacionDeInutilidad$Freq)
hist(df_perc_e_sensacionDeInutilidad$Freq)
qqnorm(df_perc_e_sensacionDeInutilidad$Freq)
################ e_faltaDeEntusiasmo ################ NO HAY VALOR ATIPICO
##Var1       Freq
##1                     De acuerdo 0.31284916
##2                 Muy de acuerdo 0.28491620
##3 Ni de acuerdo ni en desacuerdo 0.21229050
##4                  En desacuerdo 0.13966480
##5              Muy en desacuerdo 0.05027933
##Var1      Freq
##1    3 0.5977654
##2    2 0.2122905
##3    1 0.1899441
df_perc_e_faltaDeEntusiasmo <- as.data.frame(prop.table(table(survey$e_faltaDeEntusiasmo)))
df_perc_e_faltaDeEntusiasmo <- df_perc_e_faltaDeEntusiasmo %>% arrange(-Freq)
df_perc_e_faltaDeEntusiasmo
df_perc_e_faltaDeEntusiasmo[df_perc_e_faltaDeEntusiasmo$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_faltaDeEntusiasmo[df_perc_e_faltaDeEntusiasmo$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_faltaDeEntusiasmo[df_perc_e_faltaDeEntusiasmo$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_faltaDeEntusiasmo <- df_perc_e_faltaDeEntusiasmo %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_faltaDeEntusiasmo,by=c("e_faltaDeEntusiasmo"="Var1"))
survey <- survey[,!names(survey)%in% c("e_faltaDeEntusiasmo")]
names(survey)[33] <- "e_faltaDeEntusiasmo"
names(survey)
df_perc_e_faltaDeEntusiasmo <- as.data.frame(prop.table(table(survey$e_faltaDeEntusiasmo)))
df_perc_e_faltaDeEntusiasmo <- df_perc_e_faltaDeEntusiasmo %>% arrange(-Freq)
df_perc_e_faltaDeEntusiasmo
survey$e_faltaDeEntusiasmo <- as.factor(survey$e_faltaDeEntusiasmo)
summary(survey$e_faltaDeEntusiasmo)
boxplot(df_perc_e_faltaDeEntusiasmo$Freq)
hist(df_perc_e_faltaDeEntusiasmo$Freq)
qqnorm(df_perc_e_faltaDeEntusiasmo$Freq)
################ e_noControl ################ NO HAY VALOR ATIPICO
##Var1       Freq
##1                 Muy de acuerdo 0.31843575
##2                     De acuerdo 0.30167598
##3 Ni de acuerdo ni en desacuerdo 0.21229050
##4                  En desacuerdo 0.12290503
##5              Muy en desacuerdo 0.04469274
##Var1      Freq
##1    3 0.6201117
##2    2 0.2122905
##3    1 0.1675978
df_perc_e_noControl <- as.data.frame(prop.table(table(survey$e_noControl)))
df_perc_e_noControl <- df_perc_e_noControl %>% arrange(-Freq)
df_perc_e_noControl
df_perc_e_noControl[df_perc_e_noControl$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_noControl[df_perc_e_noControl$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_noControl[df_perc_e_noControl$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_noControl <- df_perc_e_noControl %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_noControl,by=c("e_noControl"="Var1"))
survey <- survey[,!names(survey)%in% c("e_noControl")]
names(survey)[33] <- "e_noControl"
names(survey)
df_perc_e_noControl <- as.data.frame(prop.table(table(survey$e_noControl)))
df_perc_e_noControl <- df_perc_e_noControl %>% arrange(-Freq)
df_perc_e_noControl
survey$e_noControl <- as.factor(survey$e_noControl)
summary(survey$e_noControl)
boxplot(df_perc_e_noControl$Freq)
hist(df_perc_e_noControl$Freq)
qqnorm(df_perc_e_noControl$Freq)
################ e_preocupacionExcesiva ################ NO HAY VALOR ATIPICO
##Var1        Freq
##1                     De acuerdo 0.413407821
##2                 Muy de acuerdo 0.379888268
##3 Ni de acuerdo ni en desacuerdo 0.150837989
##4              Muy en desacuerdo 0.027932961
##5                  En desacuerdo 0.022346369
##6                                0.005586592
##Var1      Freq
##1    3 0.7977528
##2    2 0.1516854
##3    1 0.0505618
df_perc_e_preocupacionExcesiva <- as.data.frame(prop.table(table(survey$e_preocupacionExcesiva)))
df_perc_e_preocupacionExcesiva <- df_perc_e_preocupacionExcesiva %>% arrange(-Freq)
df_perc_e_preocupacionExcesiva
df_perc_e_preocupacionExcesiva[df_perc_e_preocupacionExcesiva$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_preocupacionExcesiva[df_perc_e_preocupacionExcesiva$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_preocupacionExcesiva[df_perc_e_preocupacionExcesiva$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_preocupacionExcesiva <- df_perc_e_preocupacionExcesiva %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_preocupacionExcesiva,by=c("e_preocupacionExcesiva"="Var1"))
survey <- survey[,!names(survey)%in% c("e_preocupacionExcesiva")]
names(survey)[33] <- "e_preocupacionExcesiva"
names(survey)
df_perc_e_preocupacionExcesiva <- as.data.frame(prop.table(table(survey$e_preocupacionExcesiva)))
df_perc_e_preocupacionExcesiva <- df_perc_e_preocupacionExcesiva %>% arrange(-Freq)
df_perc_e_preocupacionExcesiva
survey$e_preocupacionExcesiva <- as.factor(survey$e_preocupacionExcesiva)
summary(survey$e_preocupacionExcesiva)
boxplot(df_perc_e_preocupacionExcesiva$Freq)
hist(df_perc_e_preocupacionExcesiva$Freq)
qqnorm(df_perc_e_preocupacionExcesiva$Freq)
################ e_dificultadConsentracion ################ NO HAY VALOR ATIPICO
##Var1        Freq
##1                     De acuerdo 0.357541899
##2                 Muy de acuerdo 0.346368715
##3 Ni de acuerdo ni en desacuerdo 0.206703911
##4                  En desacuerdo 0.061452514
##5              Muy en desacuerdo 0.022346369
##6                                0.005586592
##Var1       Freq
##1    3 0.70786517
##2    2 0.20786517
##3    1 0.08426966
df_perc_e_dificultadConsentracion <- as.data.frame(prop.table(table(survey$e_dificultadConsentracion)))
df_perc_e_dificultadConsentracion <- df_perc_e_dificultadConsentracion %>% arrange(-Freq)
df_perc_e_dificultadConsentracion
df_perc_e_dificultadConsentracion[df_perc_e_dificultadConsentracion$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_dificultadConsentracion[df_perc_e_dificultadConsentracion$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_dificultadConsentracion[df_perc_e_dificultadConsentracion$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_dificultadConsentracion <- df_perc_e_dificultadConsentracion %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_dificultadConsentracion,by=c("e_dificultadConsentracion"="Var1"))
survey <- survey[,!names(survey)%in% c("e_dificultadConsentracion")]
names(survey)[33] <- "e_dificultadConsentracion"
names(survey)
df_perc_e_dificultadConsentracion <- as.data.frame(prop.table(table(survey$e_dificultadConsentracion)))
df_perc_e_dificultadConsentracion <- df_perc_e_dificultadConsentracion %>% arrange(-Freq)
df_perc_e_dificultadConsentracion
survey$e_dificultadConsentracion <- as.factor(survey$e_dificultadConsentracion)
boxplot(df_perc_e_dificultadConsentracion$Freq)
hist(df_perc_e_dificultadConsentracion$Freq)
qqnorm(df_perc_e_dificultadConsentracion$Freq)
################ e_olvidosFrecuentes ################ NO HAY VALOR ATIPICO
##Var1       Freq
##1                     De acuerdo 0.39664804
##2                 Muy de acuerdo 0.29050279
##3 Ni de acuerdo ni en desacuerdo 0.19553073
##4                  En desacuerdo 0.08938547
##5              Muy en desacuerdo 0.02793296
##Var1      Freq
##1    3 0.6871508
##2    2 0.1955307
##3    1 0.1173184
df_perc_e_olvidosFrecuentes <- as.data.frame(prop.table(table(survey$e_olvidosFrecuentes)))
df_perc_e_olvidosFrecuentes <- df_perc_e_olvidosFrecuentes %>% arrange(-Freq)
df_perc_e_olvidosFrecuentes
df_perc_e_olvidosFrecuentes[df_perc_e_olvidosFrecuentes$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_olvidosFrecuentes[df_perc_e_olvidosFrecuentes$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_olvidosFrecuentes[df_perc_e_olvidosFrecuentes$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_olvidosFrecuentes <- df_perc_e_olvidosFrecuentes %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_olvidosFrecuentes,by=c("e_olvidosFrecuentes"="Var1"))
survey <- survey[,!names(survey)%in% c("e_olvidosFrecuentes")]
names(survey)[33] <- "e_olvidosFrecuentes"
names(survey)
df_perc_e_olvidosFrecuentes <- as.data.frame(prop.table(table(survey$e_olvidosFrecuentes)))
df_perc_e_olvidosFrecuentes <- df_perc_e_olvidosFrecuentes %>% arrange(-Freq)
df_perc_e_olvidosFrecuentes
survey$e_olvidosFrecuentes <- as.factor(survey$e_olvidosFrecuentes)
summary(survey$e_preocupacionExcesiva)
boxplot(df_perc_e_olvidosFrecuentes$Freq)
hist(df_perc_e_olvidosFrecuentes$Freq)
qqnorm(df_perc_e_olvidosFrecuentes$Freq)
################ e_aislamiento ################ NO HAY VALOR ATIPICO
##Var1        Freq
##1                 Muy de acuerdo 0.307262570
##2 Ni de acuerdo ni en desacuerdo 0.217877095
##3                     De acuerdo 0.212290503
##4                  En desacuerdo 0.184357542
##5              Muy en desacuerdo 0.072625698
##6                                0.005586592
##Var1      Freq
##1    3 0.5224719
##2    1 0.2584270
##3    2 0.2191011
df_perc_e_aislamiento <- as.data.frame(prop.table(table(survey$e_aislamiento)))
df_perc_e_aislamiento <- df_perc_e_aislamiento %>% arrange(-Freq)
df_perc_e_aislamiento
df_perc_e_aislamiento[df_perc_e_aislamiento$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_aislamiento[df_perc_e_aislamiento$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_aislamiento[df_perc_e_aislamiento$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_aislamiento <- df_perc_e_aislamiento %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_aislamiento,by=c("e_aislamiento"="Var1"))
survey <- survey[,!names(survey)%in% c("e_aislamiento")]
names(survey)[33] <- "e_aislamiento"
names(survey)
df_perc_e_aislamiento <- as.data.frame(prop.table(table(survey$e_aislamiento)))
df_perc_e_aislamiento <- df_perc_e_aislamiento %>% arrange(-Freq)
df_perc_e_aislamiento
survey$e_aislamiento <- as.factor(survey$e_aislamiento)
summary(survey$e_aislamiento)
boxplot(df_perc_e_aislamiento$Freq)
hist(df_perc_e_aislamiento$Freq)
qqnorm(df_perc_e_aislamiento$Freq)
################ e_escasoDesempeno ################ NO HAY VALOR ATIPICO
##Var1       Freq
##1                     De acuerdo 0.29050279
##2 Ni de acuerdo ni en desacuerdo 0.29050279
##3                 Muy de acuerdo 0.21229050
##4                  En desacuerdo 0.17318436
##5              Muy en desacuerdo 0.03351955
##Var1      Freq
##1    3 0.5027933
##2    2 0.2905028
##3    1 0.2067039
df_perc_e_escasoDesempeno <- as.data.frame(prop.table(table(survey$e_escasoDesempeno)))
df_perc_e_escasoDesempeno <- df_perc_e_escasoDesempeno %>% arrange(-Freq)
df_perc_e_escasoDesempeno
df_perc_e_escasoDesempeno[df_perc_e_escasoDesempeno$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_escasoDesempeno[df_perc_e_escasoDesempeno$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_escasoDesempeno[df_perc_e_escasoDesempeno$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_escasoDesempeno <- df_perc_e_escasoDesempeno %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_escasoDesempeno,by=c("e_escasoDesempeno"="Var1"))
survey <- survey[,!names(survey)%in% c("e_escasoDesempeno")]
names(survey)[33] <- "e_escasoDesempeno"
names(survey)
df_perc_e_escasoDesempeno <- as.data.frame(prop.table(table(survey$e_escasoDesempeno)))
df_perc_e_escasoDesempeno <- df_perc_e_escasoDesempeno %>% arrange(-Freq)
df_perc_e_escasoDesempeno
survey$e_escasoDesempeno <- as.factor(survey$e_escasoDesempeno)
summary(survey$e_escasoDesempeno)
boxplot(df_perc_e_escasoDesempeno$Freq)
hist(df_perc_e_escasoDesempeno$Freq)
qqnorm(df_perc_e_escasoDesempeno$Freq)
################ e_dificultadAceptarResp ################ NO HAY VALOR ATIPICO
##Var1      Freq
##1    1 0.3966480
##2    3 0.3519553
##3    2 0.2513966
df_perc_e_dificultadAceptarResp <- as.data.frame(prop.table(table(survey$e_dificultadAceptarResp)))
df_perc_e_dificultadAceptarResp <- df_perc_e_dificultadAceptarResp %>% arrange(-Freq)
df_perc_e_dificultadAceptarResp
df_perc_e_dificultadAceptarResp[df_perc_e_dificultadAceptarResp$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_dificultadAceptarResp[df_perc_e_dificultadAceptarResp$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_dificultadAceptarResp[df_perc_e_dificultadAceptarResp$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_dificultadAceptarResp <- df_perc_e_dificultadAceptarResp %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_dificultadAceptarResp,by=c("e_dificultadAceptarResp"="Var1"))
survey <- survey[,!names(survey)%in% c("e_dificultadAceptarResp")]
names(survey)[33] <- "e_dificultadAceptarResp"
names(survey)
df_perc_e_dificultadAceptarResp <- as.data.frame(prop.table(table(survey$e_dificultadAceptarResp)))
df_perc_e_dificultadAceptarResp <- df_perc_e_dificultadAceptarResp %>% arrange(-Freq)
df_perc_e_dificultadAceptarResp
survey$e_dificultadAceptarResp <- as.factor(survey$e_dificultadAceptarResp)
summary(survey$e_dificultadAceptarResp)
boxplot(df_perc_e_dificultadAceptarResp$Freq)
hist(df_perc_e_dificultadAceptarResp$Freq)
qqnorm(df_perc_e_dificultadAceptarResp$Freq)

################ e_mantenerRespon ################ NO HAY VALOR ATIPICO
##Var1       Freq
##1 Ni de acuerdo ni en desacuerdo 0.29608939
##2                  En desacuerdo 0.25698324
##3                     De acuerdo 0.24581006
##4                 Muy de acuerdo 0.14525140
##5              Muy en desacuerdo 0.05586592
##Var1      Freq
##1    3 0.3910615
##2    1 0.3128492
##3    2 0.2960894
df_perc_e_mantenerRespon <- as.data.frame(prop.table(table(survey$e_mantenerRespon)))
df_perc_e_mantenerRespon <- df_perc_e_mantenerRespon %>% arrange(-Freq)
df_perc_e_mantenerRespon
df_perc_e_mantenerRespon[df_perc_e_mantenerRespon$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_mantenerRespon[df_perc_e_mantenerRespon$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_mantenerRespon[df_perc_e_mantenerRespon$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_mantenerRespon <- df_perc_e_mantenerRespon %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_mantenerRespon,by=c("e_mantenerRespon"="Var1"))
survey <- survey[,!names(survey)%in% c("e_mantenerRespon")]
names(survey)[33] <- "e_mantenerRespon"
names(survey)
df_perc_e_mantenerRespon <- as.data.frame(prop.table(table(survey$e_mantenerRespon)))
df_perc_e_mantenerRespon <- df_perc_e_mantenerRespon %>% arrange(-Freq)
df_perc_e_mantenerRespon
survey$e_mantenerRespon <- as.factor(survey$e_mantenerRespon)
summary(survey$e_mantenerRespon)
boxplot(df_perc_e_mantenerRespon$Freq)
hist(df_perc_e_mantenerRespon$Freq)
qqnorm(df_perc_e_mantenerRespon$Freq)
################ e_abandonarClases ################ SI  HAY VALOR ATIPICO
df_perc_e_abandonarClases <- as.data.frame(prop.table(table(survey$e_abandonarClases)))
df_perc_e_abandonarClases <- df_perc_e_abandonarClases %>% arrange(-Freq)
df_perc_e_abandonarClases
df_perc_e_abandonarClases[df_perc_e_abandonarClases$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_abandonarClases[df_perc_e_abandonarClases$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_abandonarClases[df_perc_e_abandonarClases$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_abandonarClases <- df_perc_e_abandonarClases %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_abandonarClases,by=c("e_abandonarClases"="Var1"))
survey <- survey[,!names(survey)%in% c("e_abandonarClases")]
names(survey)[33] <- "e_abandonarClases"
names(survey)
df_perc_e_abandonarClases <- as.data.frame(prop.table(table(survey$e_abandonarClases)))
df_perc_e_abandonarClases <- df_perc_e_abandonarClases %>% arrange(-Freq)
df_perc_e_abandonarClases
boxplot(df_perc_e_abandonarClases$Freq)
hist(df_perc_e_abandonarClases$Freq)
qqnorm(df_perc_e_abandonarClases$Freq)
survey$e_abandonarClases <- as.factor(survey$e_abandonarClases)
