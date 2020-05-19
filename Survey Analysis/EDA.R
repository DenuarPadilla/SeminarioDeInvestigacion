setwd("/") ####1
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv/") ####2
survey <- read.csv("survey.csv",header = T,sep=",",encoding ="UTF-8") ####3
str(survey) ####5
summary(survey) ####6
colum_names <- read.csv("column_name_tratado.csv",header=T,sep=";")#####18
colum_names####19
survey <- survey[,!(names(survey) %in% c("Marca.temporal"))] ####20
names(survey) ####22
colum_names$traslation <- as.character(colum_names$traslation)####23
names(survey) <- colum_names$traslation####24
names(survey) ####25
head(survey)####26
write.csv(survey,"survey_limpio.csv",row.names = FALSE)####27 solo lo hice para ver como se guardaba
