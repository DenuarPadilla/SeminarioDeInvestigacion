#### VALORES NULOS

###Instalamos librerias necesarias
#install.packages("dplyr")

library(dplyr)
###################################

setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv/") ####2 DIRECTORIO

####3 Leemos el archivo CSV limpio
survey <- read.csv("survey_limpio_transfor_EC.csv", sep = ",", header = T)


na.summary <- c() #Variable donde guardaremos los valores nulos

for (myname in names(survey)) {
  #print(myname)
  s <- as.data.frame(prop.table(table(is.na(survey[,myname]))))
  operacion <- s %>% filter(Var1 == TRUE) %>% select(Freq)
  
  df_temp <- data.frame(
    column.name = c(myname),
    na.percentage = ifelse(length(operacion$Freq) == 0, 0, operacion$Freq[1])
  )
  
  na.summary <- rbind(na.summary, df_temp)
  
}
table(survey$influenciaEnRendimiento)
#Columnas con valores nulos y su respectivo porcentaje

na.summary %>% arrange(-na.percentage) %>% filter(na.percentage > 0)


x <- survey %>% filter(!is.na(influenciaEnRendimiento))

media <- median(as.numeric(x$influenciaEnRendimiento))

#REEMPLAZAMOS EL VALOR DE UNA COLUMNA EN ESPECIFICO, DONDE SU VALOR ES NA
survey[is.na(survey$influenciaEnRendimiento),"influenciaEnRendimiento"] <- media

survey$anioAcademico
summary(survey)










library(dplyr)
###################################

setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv/") ####2 DIRECTORIO

survey <- read.csv("survey_limpio_transfor_EC.v2.csv", sep = ",", header = T)

df_perc_teeemp <- as.data.frame(prop.table(table(survey$)))
df_perc_teeemp <- df_perc_teeemp %>% arrange(-Freq)
df_perc_teeemp
boxplot(df_perc_teeemp$Freq)
hist(df_perc_teeemp$Freq)
qqnorm(df_perc_teeemp$Freq)
