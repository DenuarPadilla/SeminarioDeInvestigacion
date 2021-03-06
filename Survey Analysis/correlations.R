###Instalamos nueva librer�a: ggplot2
#install.packages("ggplot2")

library(dplyr)
library(ggplot2)
###################################

setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv/") ####2 DIRECTORIO

survey <- read.csv("survey_limpio_transfor_EC.v2.csv", sep = ",", header = T)

prop.table(table(survey$influenciaEnRendimiento, survey$ocupacion),1)
prop.table(table(survey$anioAcademico, survey$expAnsiedad),1)
prop.table(table(survey$anioAcademico, survey$periodoAnsiedad),1)
prop.table(table(survey$edad, survey$expAnsiedad),1)

ggplot(survey) + 
  aes(x = expAnsiedad, fill = factor(edad)) +
  geom_bar(position = "stack") + 
  theme(axis.text.x = element_text(angle = 45))

  chisq.test(table(survey$edad, survey$expAnsiedad))

summary(survey)
#fill, stack

#Cuando se quiere escribir hip�tesis tenemos que escribir dos tipos:

#H_0 : Hip�tesis NULA
  #las categorias de indice y trabaja son independientes
#H_A : alternativa
  #las categorias son dependientes

#La conclusion que se quiera dar, siempre debe estar en funci�n de nuestra hip�tesis NULA 

#�Cuando rechazamos o aceptamos la hipotesis nula? 
  #aceptamos la H_0 cuando el p-value de nuestra prueba estadistica chisq.test es menor a 0.05

#Conclusi�n: Seg�n nuestr p-value rechazamos nuestra hip�tesis nula, por lo tanto las variables son dependientes.

