##install.packages("dplyr")
library(dplyr)
library(ggplot2)
setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap5/survey analysis/csv/")
survey <- read.csv("survey_limpio_transfor_EC.v2.csv", sep = ",", header = T)
names(survey)


###################### INFLUENCIA EN RENDIMIENTO #####################
survey[survey$influenciaEnRendimiento %in% c("1"),"influenciaEnRendimiento"] <- "Disminuye"
survey[survey$influenciaEnRendimiento %in% c("3"),"influenciaEnRendimiento"] <- "Aumenta"
survey[survey$influenciaEnRendimiento %in% c("2"),"influenciaEnRendimiento"] <- "Se mantiene"
survey$expAnsiedad


#####CORRELACIONES DE VARIABLES CATEGORICAS
#ocupacion con influencia rendimiento

#################################Esto ahorita no
prop.table(table(survey$influenciaEnRendimiento, survey$realizaPractica),2)



ggplot(survey) + 
  aes(x= influenciaEnRendimiento, fill = factor(realizaPractica)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45))


chisq.test(table(survey$influenciaEnRendimiento, survey$realizaPractica))
###################################################################
prop.table(table(survey$influenciaEnRendimiento, survey$expAnsiedad),2)

prop.table(table(survey$expAnsiedad))

ggplot(survey) + 
  aes(x=influenciaEnRendimiento , fill = factor(expAnsiedad)) +
  geom_bar(position = "stack") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#6d717a", "#dd3243"))



chisq.test(table(survey$influenciaEnRendimiento, survey$expAnsiedad))
#########################cansancio vs anio academico####################################
survey[survey$e_cansancio %in% c("1"),"e_cansancio"] <- "Desacuerdo"
survey[survey$e_cansancio %in% c("3"),"e_cansancio"] <- "Acuerdo"
survey[survey$e_cansancio %in% c("2"),"e_cansancio"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_cansancio),2)

prop.table(table(survey$anioAcademico))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_cansancio)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$e_cansancio))
df_perc_c <- as.data.frame(prop.table(table(survey$anioAcademico, survey$e_preocupacionExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)
#########################e_preocupacionExcesiva vs anio academico####################################
survey[survey$e_preocupacionExcesiva %in% c("1"),"e_preocupacionExcesiva"] <- "Desacuerdo"
survey[survey$e_preocupacionExcesiva %in% c("3"),"e_preocupacionExcesiva"] <- "Acuerdo"
survey[survey$e_preocupacionExcesiva %in% c("2"),"e_preocupacionExcesiva"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_preocupacionExcesiva),1)

prop.table(table(survey$e_preocupacionExcesiva))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_preocupacionExcesiva)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$e_preocupacionExcesiva))
df_perc_c <- as.data.frame(prop.table(table(survey$anioAcademico, survey$e_preocupacionExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)
#########################e_dificultadConsentracion vs anio academico####################################
survey[survey$e_dificultadConsentracion %in% c("1"),"e_dificultadConsentracion"] <- "Desacuerdo"
survey[survey$e_dificultadConsentracion %in% c("3"),"e_dificultadConsentracion"] <- "Acuerdo"
survey[survey$e_dificultadConsentracion %in% c("2"),"e_dificultadConsentracion"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_dificultadConsentracion),1)

prop.table(table(survey$e_dificultadConsentracion))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_dificultadConsentracion)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$e_dificultadConsentracion))
df_perc_c <- as.data.frame(prop.table(table(survey$anioAcademico, survey$e_dificultadConsentracion)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)
#########################olvidos frecuentes vs anio academico####################################
survey[survey$e_olvidosFrecuentes %in% c("1"),"e_olvidosFrecuentes"] <- "Desacuerdo"
survey[survey$e_olvidosFrecuentes %in% c("3"),"e_olvidosFrecuentes"] <- "Acuerdo"
survey[survey$e_olvidosFrecuentes %in% c("2"),"e_olvidosFrecuentes"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_olvidosFrecuentes),1)

prop.table(table(survey$e_olvidosFrecuentes))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_olvidosFrecuentes)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$e_olvidosFrecuentes))
df_perc_c <- as.data.frame(prop.table(table(survey$anioAcademico, survey$e_olvidosFrecuentes)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)
#########################dificultad de dormir vs anio academico####################################
survey[survey$e_dificultadDormir %in% c("1"),"e_dificultadDormir"] <- "Desacuerdo"
survey[survey$e_dificultadDormir %in% c("3"),"e_dificultadDormir"] <- "Acuerdo"
survey[survey$e_dificultadDormir %in% c("2"),"e_dificultadDormir"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_dificultadDormir),1)

prop.table(table(survey$anioAcademico))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_dificultadDormir)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$c_trabajosGrupo))
df_perc_c <- as.data.frame(prop.table(table(survey$anioAcademico)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)
#########################no control vs anio academico####################################
survey[survey$e_noControl %in% c("1"),"e_noControl"] <- "Desacuerdo"
survey[survey$e_noControl %in% c("3"),"e_noControl"] <- "Acuerdo"
survey[survey$e_noControl %in% c("2"),"e_noControl"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_noControl),1)

prop.table(table(survey$e_noControl))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_noControl)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$e_noControl))
df_perc_c <- as.data.frame(prop.table(table(survey$e_noControl)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)

#########################irritabilidad excesiva vs anio academico####################################
survey[survey$e_irritabilidadExcesiva %in% c("1"),"e_irritabilidadExcesiva"] <- "Desacuerdo"
survey[survey$e_irritabilidadExcesiva %in% c("3"),"e_irritabilidadExcesiva"] <- "Acuerdo"
survey[survey$e_irritabilidadExcesiva %in% c("2"),"e_irritabilidadExcesiva"] <- "Ni acuerdo ni desacuerdo"
prop.table(table(survey$anioAcademico, survey$e_irritabilidadExcesiva),1)

prop.table(table(survey$e_irritabilidadExcesiva))
ggplot(survey) + 
  aes(x=anioAcademico, fill = factor(e_irritabilidadExcesiva)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) + 
  scale_fill_manual(values=c("#dd3243", "#2ea530","#6d717a"))
chisq.test(table(survey$anioAcademico,survey$e_irritabilidadExcesiva))
df_perc_c <- as.data.frame(prop.table(table(survey$e_irritabilidadExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)


chisq.test(table(survey$anioAcademico, survey$e_preocupacionExcesiva))
#PROPORCIOONES DE PARTES DE LA carrera
table(survey$anioAcademico)
df_perc_partes_carrera <- as.data.frame(prop.table(table(survey$anioAcademico)))
df_perc_partes_carrera <- df_perc_partes_carrera %>% arrange(-Freq)
datos <- df_perc_partes_carrera[with(df_perc_partes_carrera, order(df_perc_partes_carrera$Var1)), ] # Orden directo 
datos
df_perc_partes_carrera$CARRERA <- df_perc_partes_carrera$Var1
df_perc_partes_carrera$RESPUESTAS <- df_perc_partes_carrera$Freq
ggplot(data = df_perc_partes_carrera)+geom_bar(mapping = aes(x='Total', y=RESPUESTAS, fill=CARRERA), stat="identity") + coord_flip()

########FOR PARA GRAFICOS CAUSAS VS YEARS
efectos <- c("e_escasaConfianzaPropia", "e_sensacionDeInutilidad", "e_faltaDeEntusiasmo", "e_noControl", "e_preocupacionExcesiva", 
             "e_dificultadConsentracion", "e_olvidosFrecuentes", "e_aislamiento", "e_escasoDesempeno", "e_dificultadAceptarResp",  
             "e_mantenerRespon", "e_abandonarClases", "e_cansancio", "e_dificultadDormir", "e_noConsideracion", "e_irritabilidadExcesiva")

causas <- c("c_exposicionesConferencias", "c_cantidadEvaluaciones", "c_multitudTareas", "c_intervencionAula", "c_ambienteDesagradable", "c_trabajosPlazo", "c_trabajosGrupo")
years <- names(summary(as.factor(survey$anioAcademico)))
df_to_chart <- data.frame(CAUSAS=causas)
for (year in years) {
  where <- survey$anioAcademico %in% c(year)
  columnData = c()
  for (causa in causas) {
    columna <- names(survey) %in% c(causa)
    prom <- mean(as.numeric(survey[where, columna]))/3
    columnData <- c(columnData, prom)
  }
  df_to_chart[[year]] <- columnData
}
promedios <- df_to_chart[, !(names(df_to_chart) %in% 'CAUSAS')]
column_causas = c()
colunm_proms = c()
colunm_parts = c()
for (carrera_part in names(promedios)) {
  promedios_en_carrera_part <- promedios[[carrera_part]]
  
  for(causa in causas){
    column_causas = c(column_causas, causa)
  }
  for(p in promedios_en_carrera_part){
    colunm_parts = c(colunm_parts, carrera_part)
    colunm_proms = c(colunm_proms, p)
  }
}
to_chart_complete = data.frame(CAUSAS= column_causas, CARRERA_PART = colunm_parts, PROMEDIO = colunm_proms)

########################################
###CHART EVOLUCION DE CAUSA EN LA CARRERA
df_c <-  to_chart_complete[to_chart_complete$CAUSAS %in% c('c_trabajosGrupo'), !(names(to_chart_complete) %in% 'CAUSAS')]
library(ggplot2)
df_c$c_trabajosGrupo <- df_c[, names(df_c) %in% "CARRERA_PART"]
ggplot(data = df_c) + geom_bar(mapping = aes(x = c_trabajosGrupo, y = PROMEDIO), stat = "identity") + lims ( y  =  c ( 0 , 1 ))
########################################
########################################









#####################CORRELACION DE CADA CAUSA CON SUS YEARS
##VERIFICAMOS QUE LA VARIABLE NUMERICA SEA NORMAL
df_perc_c <- as.data.frame(prop.table(table(survey$c_intervencionAula)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
c_intervencionAula <- df_perc_c


#NORMALIDAD GENERAL
shapiro.test(c_intervencionAula$Freq)
#H_0: hay normalidad en la variable numerica correspondiente a la causa "exposiciones y/o conferencias"
#H_A: No hay normalidad en la variable
#W = 0.78936, p-value = 0.08934   
#COMO EL VALOR DE P-VALUE ES MAYOR A 0.05 NO PODEMOS RECHAZAR LA HIPOTESIS NULA DE QUE NUESTROS DATOS TENGAN UNA DISTRIBUCION NORMAL



#NORMALIDAD GRUPO 1
carrera_parte_1 <- survey %>% filter(anioAcademico == "carrera parte 1") %>% select(e_irritabilidadExcesiva)
df_perc_c <- as.data.frame(prop.table(table(carrera_parte_1$e_irritabilidadExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 1
shapiro.test(df_perc_c$Freq)

#NORMALIDAD GRUPO 2
carrera_parte_2 <- survey %>% filter(anioAcademico == "carrera parte 2") %>% select(e_irritabilidadExcesiva)
df_perc_c <- as.data.frame(prop.table(table(carrera_parte_2$e_irritabilidadExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 2
shapiro.test(df_perc_c$Freq)

#NORMALIDAD GRUPO 3
carrera_parte_3 <- survey %>% filter(anioAcademico == "carrera parte 3") %>% select(e_irritabilidadExcesiva)
df_perc_c <- as.data.frame(prop.table(table(carrera_parte_3$e_irritabilidadExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 3
shapiro.test(df_perc_c$Freq)


#NORMALIDAD GRUPO 4
carrera_parte_4 <- survey %>% filter(anioAcademico == "carrera parte 4") %>% select(e_irritabilidadExcesiva)
df_perc_c <- as.data.frame(prop.table(table(carrera_parte_4$e_irritabilidadExcesiva)))
df_perc_c <- df_perc_c %>% arrange(-Freq)
boxplot(df_perc_c$Freq)
qqnorm(df_perc_c$Freq)
qqline(df_perc_c$Freq)
#NORMALIDAD GRUPO 4
shapiro.test(df_perc_c$Freq)




#Prueba de homocedasticidad
var.test(carrera_parte_1$e_irritabilidadExcesiva, carrera_parte_2$e_irritabilidadExcesiva)
t.test(carrera_parte_1$e_irritabilidadExcesiva, carrera_parte_2$e_irritabilidadExcesiva,
       alternative = "two.sided",
       paried = FALSE,
       var.equal = TRUE)




#Prueba de homocedasticidad
var.test(carrera_parte_3$e_irritabilidadExcesiva, carrera_parte_4$e_irritabilidadExcesiva)
t.test(carrera_parte_3$e_irritabilidadExcesiva, carrera_parte_4$e_irritabilidadExcesiva,
       alternative = "two.sided",
       paried = FALSE,
       var.equal = TRUE)


#UNIR EN MITADES
grupo_parte_1_2 <- rbind(carrera_parte_1, carrera_parte_2)
grupo_parte_3_4 <- rbind(carrera_parte_3, carrera_parte_4)


#Prueba de homocedasticidad
var.test(grupo_parte_1_2$e_irritabilidadExcesiva, grupo_parte_3_4$e_irritabilidadExcesiva)
t.test(grupo_parte_1_2$e_irritabilidadExcesiva, grupo_parte_3_4$e_irritabilidadExcesiva,
       alternative = "two.sided",
       paried = FALSE,
       var.equal = TRUE)
#p-value = 0.2491












##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
#############CULSTERIZACION
install.packages("corrplot")
install.packages("factoextra")
library(corrplot)
library(factoextra)

efectos <- c("e_escasaConfianzaPropia", "e_sensacionDeInutilidad", "e_faltaDeEntusiasmo", "e_noControl", "e_preocupacionExcesiva", 
             "e_dificultadConsentracion", "e_olvidosFrecuentes", "e_aislamiento", "e_escasoDesempeno", "e_dificultadAceptarResp",  
             "e_mantenerRespon", "e_abandonarClases", "e_cansancio", "e_dificultadDormir", "e_noConsideracion", "e_irritabilidadExcesiva")

efectosInd <- c("e_escasaConfianzaPropia", "e_sensacionDeInutilidad", "e_faltaDeEntusiasmo", "e_preocupacionExcesiva", 
              "e_olvidosFrecuentes" , "e_escasoDesempeno", "e_dificultadAceptarResp", "e_mantenerRespon", "e_cansancio", "e_dificultadDormir")

causas <- c("c_exposicionesConferencias", "c_cantidadEvaluaciones", "c_multitudTareas", "c_intervencionAula", "c_ambienteDesagradable",
            "c_trabajosPlazo", "c_trabajosGrupo")

causasC <- c("c_exposicionesConferencias", "c_cantidadEvaluaciones", "c_multitudTareas", "c_intervencionAula", "c_ambienteDesagradable",
            "c_trabajosPlazo", "c_trabajosGrupo")

causasInd <- c("c_exposicionesConferencias", "c_cantidadEvaluaciones", "c_multitudTareas", "c_ambienteDesagradable", "c_trabajosPlazo", "c_trabajosGrupo")



numeric_crr <- survey[, names(survey) %in%  c(efectosInd)]
numeric_crr

############################PAPER###########################
survey[survey$e_cansancio %in% c("Desacuerdo"),"e_cansancio"] <- "1"
survey[survey$e_cansancio %in% c("Acuerdo"),"e_cansancio"] <- "3"
survey[survey$e_cansancio %in% c("Ni acuerdo ni desacuerdo"),"e_cansancio"] <- "2"

efectosJ <- c("e_escasaConfianzaPropia", "e_faltaDeEntusiasmo", "e_preocupacionExcesiva", "e_dificultadAceptarResp", "e_cansancio")

efectosD <- c("e_sensacionDeInutilidad", "e_olvidosFrecuentes" , "e_escasoDesempeno", "e_mantenerRespon", "e_dificultadDormir")

numeric_crr <- survey[, names(survey) %in%  c(efectosJ)]

x <- cor(numeric_crr, method = c("pearson", "kendall", "spearman"))

corrplot(x, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
prop.table(corrplot(x, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45))

res <- prcomp(numeric_crr, scale=F)
fviz_eig(res)
fviz_pca_ind(res,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)


fviz_pca_var(res,
             col.var = "contrib",
             repel = TRUE
)

cat_efectos <- kmeans(res$x[,1:2], centers = 2, nstart = 25)
fviz_cluster(cat_efectos, data = res$x[,1:2])
table(cat_efectos$cluster)
prop.table(table(cat_efectos$cluster))
cat_efectos$cluster

survey <- cbind(survey, cat_efectos$cluster)


prop.table(table(survey$`cat_efectos$cluster`))
############################################################



x <- cor(numeric_crr, method = c("pearson", "kendall", "spearman"))
corrplot(x, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)
res <- prcomp(numeric_crr, scale=F)
fviz_eig(res)
fviz_pca_ind(res,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
             )


fviz_pca_var(res,
             col.var = "contrib",
             repel = TRUE
             )


cat_efectos <- kmeans(res$x[,1:2], centers = 2, nstart = 25)
fviz_cluster(k3, data = res$x[,1:2])

k3$cluster

survey <- cbind(survey, k3$cluster)


prop.table(table(survey$`k3$cluster`))

###############################
df_perc_jornada <- as.data.frame(prop.table(table(survey$jornadaCLases)))
df_perc_jornada <- df_perc_jornada %>% arrange(-Freq)
df_perc_jornada
df_perc_jornada[df_perc_jornada$Var1 %in% c("Mixta Matutina - Vespertina","Matutina"),"categoria"] <- "ManianaTarde"
df_perc_jornada[df_perc_jornada$Var1 %in% c("Mixta Vespertina - Nocturna","Nocturna"),"categoria"] <- "TardeNoche"
df_perc_jornada[df_perc_jornada$Var1 %in% c("Vespertina"),"categoria"] <- "Tarde"
df_perc_jornada <- df_perc_jornada %>% select(Var1,categoria)
df_perc_jornada
survey <- left_join(survey,df_perc_jornada,by=c("jornadaCLases"="Var1"))
survey <- survey[,!names(survey)%in% c("jornadaCLases")]
names(survey)[32] <- "jornadaCLases"
names(survey)
df_perc_jornada <- as.data.frame(prop.table(table(survey$jornadaCLases)))
df_perc_jornada <- df_perc_jornada %>% arrange(-Freq)
df_perc_jornada
boxplot(df_perc_jornada$Freq)
hist(df_perc_jornada$Freq)
qqnorm(df_perc_jornada$Freq)


##############################################################################################################################
##############################################################################################################################
#REGRESION LOGISTICA
#install.packages("lattice")
library(dplyr)
library(lattice)
library(ggplot2)
library(caret)
names(survey)
efectosInd <- c("e_escasaConfianzaPropia", "e_faltaDeEntusiasmo", "e_preocupacionExcesiva", "e_escasoDesempeno", "e_dificultadAceptarResp")

survey$disminuye_rendimiento <- 0
survey[survey$influenciaEnRendimiento %in% '1', "disminuye_rendimiento"] <- 1

prop.table(table(survey$disminuye_rendimiento))


features <- c("disminuye_rendimiento", "jornadaCLases", efectosInd, "edad", "expAnsiedad")
set <- survey[, names(survey) %in% features]
names(set)

set$disminuye_rendimiento <- as.factor(set$disminuye_rendimiento)

model <- glm(disminuye_rendimiento ~ ., data = set, family = "binomial")

importances <- varImp(model)
importances$col <- row.names(importances)
importances <- importances %>% arrange(-Overall)
importances


ggplot(set) + 
  aes(x=e_escasoDesempeno, fill = factor(disminuye_rendimiento)) +
  geom_bar(position = "fill") +
  theme(axis.text.x = element_text(angle = 45)) +
  scale_fill_manual(values=c("#999999", "#E69F00"))

x

##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################
##############################################################################################################################


##   EEEEEE            FFFFFFFF           EEEEEEEE           CCCCCCCCCCCC            TTTTTTTTTT            OOOOOOOOOOO             SSSSSSSS

########FOR PARA GRAFICOS CAUSAS VS YEARS
causas <- c("e_cansancio", "e_dificultadDormir", "e_noConsideracion", "e_irritabilidadExcesiva", "e_escasaConfianzaPropia", "e_sensacionDeInutilidad", "e_faltaDeEntusiasmo",
            "e_noControl", "e_preocupacionExcesiva", "e_dificultadConsentracion", "e_olvidosFrecuentes", "e_aislamiento", "e_escasoDesempeno", "e_dificultadAceptarResp"
            , "e_mantenerRespon", "e_abandonarClases")
years <- names(summary(as.factor(survey$anioAcademico)))
df_to_chart <- data.frame(EFECTOS=causas)

for (year in years) {
  where <- survey$anioAcademico %in% c(year)
  columnData = c()
  for (causa in causas) {
    columna <- names(survey) %in% c(causa)
    prom <- mean(as.numeric(survey[where, columna]))
    columnData <- c(columnData, prom)
  }
  df_to_chart[[year]] <- columnData
}
df_to_chart




promedios <- df_to_chart[, !(names(df_to_chart) %in% 'EFECTOS')]
column_causas = c()
colunm_proms = c()
colunm_parts = c()
for (carrera_part in names(promedios)) {
  promedios_en_carrera_part <- promedios[[carrera_part]]
  
  for(causa in causas){
    column_causas = c(column_causas, causa)
  }
  for(p in promedios_en_carrera_part){
    colunm_parts = c(colunm_parts, carrera_part)
    colunm_proms = c(colunm_proms, p)
  }
}
to_chart_complete = data.frame(EFECTOS= column_causas, CARRERA_PART = colunm_parts, PROMEDIO = colunm_proms)



########################################
###CHART EVOLUCION DE CAUSA EN LA CARRERA
df_c <-  to_chart_complete[to_chart_complete$EFECTOS %in% c('e_cansancio'), !(names(to_chart_complete) %in% 'EFECTOS')]
library(ggplot2)
df_c$e_cansancio <- df_c[, names(df_c) %in% "CARRERA_PART"]
ggplot(data = df_c) + geom_bar(mapping = aes(x = e_cansancio, y = PROMEDIO), stat = "identity") + lims ( y  =  c ( 0 , 1 ))








