setwd("/") ####7
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv") ####8

survey <- read.csv("survey.csv",header = T,sep=",",encoding ="UTF-8") ####9
my.names <- names(survey) ####10
columnas_a_tratar <- my.names[!(my.names %in% c("Marca.temporal"))] ####12
df <- data.frame(columna.name = columnas_a_tratar)####15
df ####16
write.csv(df,"column_name.csv",row.names = FALSE)####17

