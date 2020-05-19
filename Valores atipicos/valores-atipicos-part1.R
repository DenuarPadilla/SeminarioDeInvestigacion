library(dplyr)
###################################

setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/") ####2 DIRECTORIO

####3 Leemos el archivo CSV limpio
survey <- read.csv("survey_limpio.csv", sep = ",", header = T)


################ EJEMPLO JORNADA ################ HAY VALOR ATIPICO
#Var1       Freq
#1 Mixta Matutina - Vespertina 0.42458101
#2 Mixta Vespertina - Nocturna 0.20670391
#3                    Matutina 0.18435754
#4                    Nocturna 0.11731844
#5                  Vespertina 0.06703911

df_perc_jornada <- as.data.frame(prop.table(table(survey$jornadaCLases)))

df_perc_jornada <- df_perc_jornada %>% arrange(-Freq)

boxplot(df_perc_jornada$Freq)

hist(df_perc_jornada$Freq)

qqnorm(df_perc_jornada$Freq)

################ CANTIDAD DE EVALUACIONES ################ NO HAY VALOR ATIPICO
#Var1        Freq
#1                 Muy de acuerdo 0.402234637
#2                     De acuerdo 0.240223464
#3                  En desacuerdo 0.184357542
#4 Ni de acuerdo ni en desacuerdo 0.134078212
#5              Muy en desacuerdo 0.033519553
#6                                0.005586592
df_perc_cant_evaluaciones <- as.data.frame(prop.table(table(survey$c_cantidadEvaluaciones)))

df_perc_cant_evaluaciones <- df_perc_cant_evaluaciones %>% arrange(-Freq)

boxplot(df_perc_cant_evaluaciones$Freq)

hist(df_perc_cant_evaluaciones$Freq)

qqnorm(df_perc_cant_evaluaciones$Freq)

################ EDAD ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1     23 - 26 años 0.47486034
#2     19 - 22 años 0.37988827
#3     27 - 30 años 0.06145251
#4 mayor de 30 años 0.04469274
#5 menor de 18 años 0.03910615

df_perc_edad <- as.data.frame(prop.table(table(survey$edad)))

df_perc_edad <- df_perc_edad %>% arrange(-Freq)

boxplot(df_perc_edad$Freq)

hist(df_perc_edad$Freq)

qqnorm(df_perc_edad$Freq)

################ GENERO ################ NO HAY VALORES ATIPICOS
# Var1      Freq
#1 Hombre 0.5083799
#2  Mujer 0.4916201
df_perc_genero <- as.data.frame(prop.table(table(survey$genero)))

df_perc_genero <- df_perc_genero %>% arrange(-Freq)

boxplot(df_perc_genero$Freq)

hist(df_perc_genero$Freq)

qqnorm(df_perc_genero$Freq)

################ OCUPACION ################ NO HAY VALORES ATIPICOS
#  Var1       Freq
#1 Estudio 0.67597765
#2 Ambas 0.26256983
#3 Trabajo 0.06145251

df_perc_ocupacion <- as.data.frame(prop.table(table(survey$ocupacion)))

df_perc_ocupacion <- df_perc_ocupacion %>% arrange(-Freq)

boxplot(df_perc_ocupacion$Freq)

hist(df_perc_ocupacion$Freq)

qqnorm(df_perc_ocupacion$Freq)

################ ESTADO CIVIL ################ NO HAY VALORES ATIPICOS
#Var1      Freq
#1 Soltero 0.8212291
#2    Otro 0.1061453
#3  Casado 0.0726257

df_perc_estado_civil <- as.data.frame(prop.table(table(survey$estadoCivil)))

df_perc_estado_civil <- df_perc_estado_civil %>% arrange(-Freq)

boxplot(df_perc_estado_civil$Freq)

hist(df_perc_estado_civil$Freq)

qqnorm(df_perc_estado_civil$Freq)

################ REALIZA PRACTICA ################ NO HAY VALORES ATIPICOS
#Var1      Freq
#1   No 0.8715084
#2   Sí 0.1284916

df_perc_realiza_practica <- as.data.frame(prop.table(table(survey$realizaPractica)))

df_perc_realiza_practica <- df_perc_realiza_practica %>% arrange(-Freq)

boxplot(df_perc_realiza_practica$Freq)

hist(df_perc_realiza_practica$Freq)

qqnorm(df_perc_realiza_practica$Freq)

################ AÑO ACADEMICO ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1 ultimo año 0.31284916
#2    3er año 0.23463687
#3    4to año 0.18994413
#4    2do año 0.15642458
#5    5to año 0.05586592
#6    1er año 0.05027933

df_perc_anio_academico <- as.data.frame(prop.table(table(survey$anioAcademico)))

df_perc_anio_academico <- df_perc_anio_academico %>% arrange(-Freq)

boxplot(df_perc_anio_academico$Freq)

hist(df_perc_anio_academico$Freq)

qqnorm(df_perc_anio_academico$Freq)

################ PERIODO ANSIEDAD ################ NO HAY VALORES ATIPICOS
#         Var1       Freq
#1 En los examenes y proyectos finales 0.72625698
#2            Al final de cada parcial 0.24022346
#3               Al iniciar el periodo 0.03351955

df_perc_periodo_ansiedad <- as.data.frame(prop.table(table(survey$periodoAnsiedad)))

df_perc_periodo_ansiedad <- df_perc_periodo_ansiedad %>% arrange(-Freq)

boxplot(df_perc_periodo_ansiedad$Freq)

hist(df_perc_periodo_ansiedad$Freq)

qqnorm(df_perc_periodo_ansiedad$Freq)

################ MULTITUD DE TAREAS ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1                 Muy de acuerdo 0.33519553
#2                     De acuerdo 0.28491620
#3                  En desacuerdo 0.17318436
#4 Ni de acuerdo ni en desacuerdo 0.14525140
#5              Muy en desacuerdo 0.06145251

df_perc_c_multitudTareas <- as.data.frame(prop.table(table(survey$c_multitudTareas)))

df_perc_c_multitudTareas <- df_perc_c_multitudTareas %>% arrange(-Freq)

boxplot(df_perc_c_multitudTareas$Freq)

hist(df_perc_c_multitudTareas$Freq)

qqnorm(df_perc_c_multitudTareas$Freq)

################ EXPOSICIONES CONFERENCIAS ################ SI HAY VALORES ATIPICOS
#Var1       Freq
#1                     De acuerdo 0.36312849
#2                 Muy de acuerdo 0.24581006
#3 Ni de acuerdo ni en desacuerdo 0.18435754
#4                  En desacuerdo 0.16759777
#5              Muy en desacuerdo 0.03910615 ----- Valor atipico

df_perc_c_exposicionesConferencias <- as.data.frame(prop.table(table(survey$c_exposicionesConferencias)))

df_perc_c_exposicionesConferencias <- df_perc_c_exposicionesConferencias %>% arrange(-Freq)

boxplot(df_perc_c_exposicionesConferencias$Freq)

hist(df_perc_c_exposicionesConferencias$Freq)

qqnorm(df_perc_c_exposicionesConferencias$Freq)

################ INTERVENCION AULA ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1                     De acuerdo 0.31284916
#2 Ni de acuerdo ni en desacuerdo 0.24022346
#3                 Muy de acuerdo 0.21229050
#4                  En desacuerdo 0.16759777
#5              Muy en desacuerdo 0.06703911

df_perc_c_intervencionAula <- as.data.frame(prop.table(table(survey$c_intervencionAula)))

df_perc_c_intervencionAula <- df_perc_c_intervencionAula %>% arrange(-Freq)

boxplot(df_perc_c_intervencionAula$Freq)

hist(df_perc_c_intervencionAula$Freq)

qqnorm(df_perc_c_intervencionAula$Freq)

################ AMBIENTE DESAGRADABLE ################ SI HAY VALORES ATIPICOS
#Var1        Freq
#1                     De acuerdo 0.245810056
#2 Ni de acuerdo ni en desacuerdo 0.234636872
#3                 Muy de acuerdo 0.173184358
#4              Muy en desacuerdo 0.173184358
#5                  En desacuerdo 0.167597765
#6                                0.005586592 ----- Valor Atipico

df_perc_c_ambienteDesagradable <- as.data.frame(prop.table(table(survey$c_ambienteDesagradable)))

df_perc_c_ambienteDesagradable <- df_perc_c_ambienteDesagradable %>% arrange(-Freq)

boxplot(df_perc_c_ambienteDesagradable$Freq)

hist(df_perc_c_ambienteDesagradable$Freq)

qqnorm(df_perc_c_ambienteDesagradable$Freq)

################ TRABAJOS PLAZO ################ NO HAY VALORES ATIPICOS
#Var1      Freq
#1                 Muy de acuerdo 0.3184358
#2                     De acuerdo 0.2793296
#3                  En desacuerdo 0.1899441
#4 Ni de acuerdo ni en desacuerdo 0.1117318
#5              Muy en desacuerdo 0.1005587

df_perc_c_trabajosPlazo <- as.data.frame(prop.table(table(survey$c_trabajosPlazo)))

df_perc_c_trabajosPlazo <- df_perc_c_trabajosPlazo %>% arrange(-Freq)

boxplot(df_perc_c_trabajosPlazo$Freq)

hist(df_perc_c_trabajosPlazo$Freq)

qqnorm(df_perc_c_trabajosPlazo$Freq)

################ AMBIENTE DESAGRADABLE ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1 Ni de acuerdo ni en desacuerdo 0.33519553
#2                     De acuerdo 0.28491620
#3                 Muy de acuerdo 0.24022346
#4                  En desacuerdo 0.08938547
#5              Muy en desacuerdo 0.05027933

df_perc_c_trabajosGrupo <- as.data.frame(prop.table(table(survey$c_trabajosGrupo)))

df_perc_c_trabajosGrupo <- df_perc_c_trabajosGrupo %>% arrange(-Freq)

boxplot(df_perc_c_trabajosGrupo$Freq)

hist(df_perc_c_trabajosGrupo$Freq)

qqnorm(df_perc_c_trabajosGrupo$Freq)
