#### TRANSFORMACI脫N DE INFORMACION

###Instalamos librerias necesarias
#install.packages("dplyr")


########################## CAUSAS ############################

################ CAUSA - EXPOSICIONES CONFERENCIAS ################ SI HAY VALORES ATIPICOS
#Var1       Freq
#1                     De acuerdo 0.36312849
#2                 Muy de acuerdo 0.24581006
#3 Ni de acuerdo ni en desacuerdo 0.18435754
#4                  En desacuerdo 0.16759777
#5              Muy en desacuerdo 0.03910615 ----- Valor atipico
################## Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.6089385
#2                  En desacuerdo 0.2067039
#3 Ni de acuerdo ni en desacuerdo 0.1843575

df_perc_c_exposicionesConferencias <- as.data.frame(prop.table(table(survey$c_exposicionesConferencias)))
df_perc_c_exposicionesConferencias <- df_perc_c_exposicionesConferencias %>% arrange(-Freq)
df_perc_c_exposicionesConferencias
df_perc_c_exposicionesConferencias[df_perc_c_exposicionesConferencias$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_exposicionesConferencias[df_perc_c_exposicionesConferencias$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_exposicionesConferencias[df_perc_c_exposicionesConferencias$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_exposicionesConferencias <- df_perc_c_exposicionesConferencias %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_exposicionesConferencias,by=c("c_exposicionesConferencias"="Var1"))
survey <- survey[,!names(survey)%in% c("c_exposicionesConferencias")]
names(survey)[33] <- "c_exposicionesConferencias"
names(survey)
df_perc_c_exposicionesConferencias <- as.data.frame(prop.table(table(survey$c_exposicionesConferencias)))
df_perc_c_exposicionesConferencias <- df_perc_c_exposicionesConferencias %>% arrange(-Freq)
df_perc_c_exposicionesConferencias
survey$c_exposicionesConferencias <- as.factor(survey$c_exposicionesConferencias)
boxplot(df_perc_c_exposicionesConferencias$Freq)
hist(df_perc_c_exposicionesConferencias$Freq)
qqnorm(df_perc_c_exposicionesConferencias$Freq)


################ CAUSA CANTIDAD DE EVALUACIONES ################ NO HAY VALOR ATIPICO
#Var1        Freq
#1                 Muy de acuerdo 0.402234637
#2                     De acuerdo 0.240223464
#3                  En desacuerdo 0.184357542
#4 Ni de acuerdo ni en desacuerdo 0.134078212
#5              Muy en desacuerdo 0.033519553
#6                                0.005586592

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.6460674
#2                     desacuerdo 0.2191011
#3 Ni de acuerdo ni en desacuerdo 0.1348315

df_perc_c_cantidadEvaluaciones <- as.data.frame(prop.table(table(survey$c_cantidadEvaluaciones)))
df_perc_c_cantidadEvaluaciones <- df_perc_c_cantidadEvaluaciones %>% arrange(-Freq)
df_perc_c_cantidadEvaluaciones
df_perc_c_cantidadEvaluaciones[df_perc_c_cantidadEvaluaciones$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_cantidadEvaluaciones[df_perc_c_cantidadEvaluaciones$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_cantidadEvaluaciones[df_perc_c_cantidadEvaluaciones$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_cantidadEvaluaciones <- df_perc_c_cantidadEvaluaciones %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_cantidadEvaluaciones,by=c("c_cantidadEvaluaciones"="Var1"))
survey <- survey[,!names(survey)%in% c("c_cantidadEvaluaciones")]
names(survey)[33] <- "c_cantidadEvaluaciones"
names(survey)
df_perc_c_cantidadEvaluaciones <- as.data.frame(prop.table(table(survey$c_cantidadEvaluaciones)))
df_perc_c_cantidadEvaluaciones <- df_perc_c_cantidadEvaluaciones %>% arrange(-Freq)
df_perc_c_cantidadEvaluaciones
survey$c_cantidadEvaluaciones <- as.factor(survey$c_cantidadEvaluaciones)
boxplot(df_perc_c_cantidadEvaluaciones$Freq)
hist(df_perc_c_cantidadEvaluaciones$Freq)
qqnorm(df_perc_c_cantidadEvaluaciones$Freq)


################ CAUSA - MULTITUD DE TAREAS ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1                 Muy de acuerdo 0.33519553
#2                     De acuerdo 0.28491620
#3                  En desacuerdo 0.17318436
#4 Ni de acuerdo ni en desacuerdo 0.14525140
#5              Muy en desacuerdo 0.06145251

#Despu茅s de la tranformaci贸n
#Var1      Freq
#1                     De acuerdo 0.6201117
#2                     desacuerdo 0.2346369
#3 Ni de acuerdo ni en desacuerdo 0.1452514

df_perc_c_multitudTareas <- as.data.frame(prop.table(table(survey$c_multitudTareas)))
df_perc_c_multitudTareas <- df_perc_c_multitudTareas %>% arrange(-Freq)
df_perc_c_multitudTareas
df_perc_c_multitudTareas[df_perc_c_multitudTareas$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_multitudTareas[df_perc_c_multitudTareas$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_multitudTareas[df_perc_c_multitudTareas$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_multitudTareas <- df_perc_c_multitudTareas %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_multitudTareas,by=c("c_multitudTareas"="Var1"))
survey <- survey[,!names(survey)%in% c("c_multitudTareas")]
names(survey)[33] <- "c_multitudTareas"
names(survey)
df_perc_c_multitudTareas <- as.data.frame(prop.table(table(survey$c_multitudTareas)))
df_perc_c_multitudTareas <- df_perc_c_multitudTareas %>% arrange(-Freq)
df_perc_c_multitudTareas
survey$c_multitudTareas <- as.factor(survey$c_multitudTareas)
boxplot(df_perc_c_multitudTareas$Freq)
hist(df_perc_c_multitudTareas$Freq)
qqnorm(df_perc_c_multitudTareas$Freq)

################ CAUSA - INTERVENCION AULA ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1                     De acuerdo 0.31284916
#2 Ni de acuerdo ni en desacuerdo 0.24022346
#3                 Muy de acuerdo 0.21229050
#4                  En desacuerdo 0.16759777
#5              Muy en desacuerdo 0.06703911

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.5251397
#2 Ni de acuerdo ni en desacuerdo 0.2402235
#3                     desacuerdo 0.2346369

df_perc_c_intervencionAula <- as.data.frame(prop.table(table(survey$c_intervencionAula)))
df_perc_c_intervencionAula <- df_perc_c_intervencionAula %>% arrange(-Freq)
df_perc_c_intervencionAula
df_perc_c_intervencionAula[df_perc_c_intervencionAula$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_intervencionAula[df_perc_c_intervencionAula$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_intervencionAula[df_perc_c_intervencionAula$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_intervencionAula <- df_perc_c_intervencionAula %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_intervencionAula,by=c("c_intervencionAula"="Var1"))
survey <- survey[,!names(survey)%in% c("c_intervencionAula")]
names(survey)[33] <- "c_intervencionAula"
names(survey)
df_perc_c_intervencionAula <- as.data.frame(prop.table(table(survey$c_intervencionAula)))
df_perc_c_intervencionAula <- df_perc_c_intervencionAula %>% arrange(-Freq)
df_perc_c_intervencionAula
survey$c_intervencionAula <- as.factor(survey$c_intervencionAula)
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
df_perc_c_ambienteDesagradable
df_perc_c_ambienteDesagradable[df_perc_c_ambienteDesagradable$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_ambienteDesagradable[df_perc_c_ambienteDesagradable$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_ambienteDesagradable[df_perc_c_ambienteDesagradable$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_ambienteDesagradable <- df_perc_c_ambienteDesagradable %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_ambienteDesagradable,by=c("c_ambienteDesagradable"="Var1"))
survey <- survey[,!names(survey)%in% c("c_ambienteDesagradable")]
names(survey)[33] <- "c_ambienteDesagradable"
names(survey)
df_perc_c_ambienteDesagradable <- as.data.frame(prop.table(table(survey$c_ambienteDesagradable)))
df_perc_c_ambienteDesagradable <- df_perc_c_ambienteDesagradable %>% arrange(-Freq)
df_perc_c_ambienteDesagradable
survey$c_ambienteDesagradable <- as.factor(survey$c_ambienteDesagradable)
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

#Despu茅s de la transofrmaci贸n
#Var1      Freq
#1                     De acuerdo 0.5977654
#2                     desacuerdo 0.2905028
#3 Ni de acuerdo ni en desacuerdo 0.1117318

df_perc_c_trabajosPlazo <- as.data.frame(prop.table(table(survey$c_trabajosPlazo)))
df_perc_c_trabajosPlazo <- df_perc_c_trabajosPlazo %>% arrange(-Freq)
df_perc_c_trabajosPlazo
df_perc_c_trabajosPlazo[df_perc_c_trabajosPlazo$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_trabajosPlazo[df_perc_c_trabajosPlazo$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_trabajosPlazo[df_perc_c_trabajosPlazo$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_trabajosPlazo <- df_perc_c_trabajosPlazo %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_trabajosPlazo,by=c("c_trabajosPlazo"="Var1"))
survey <- survey[,!names(survey)%in% c("c_trabajosPlazo")]
names(survey)[33] <- "c_trabajosPlazo"
names(survey)
df_perc_c_trabajosPlazo <- as.data.frame(prop.table(table(survey$c_trabajosPlazo)))
df_perc_c_trabajosPlazo <- df_perc_c_trabajosPlazo %>% arrange(-Freq)
df_perc_c_trabajosPlazo
survey$c_trabajosPlazo <- as.factor(survey$c_trabajosPlazo)
boxplot(df_perc_c_trabajosPlazo$Freq)
hist(df_perc_c_trabajosPlazo$Freq)
qqnorm(df_perc_c_trabajosPlazo$Freq)


################ CAUSA - TRABAJOS GRUPO ################ NO HAY VALORES ATIPICOS
#Var1       Freq
#1 Ni de acuerdo ni en desacuerdo 0.33519553
#2                     De acuerdo 0.28491620
#3                 Muy de acuerdo 0.24022346
#4                  En desacuerdo 0.08938547
#5              Muy en desacuerdo 0.05027933

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.5251397
#2 Ni de acuerdo ni en desacuerdo 0.3351955
#3                     desacuerdo 0.1396648

df_perc_c_trabajosGrupo <- as.data.frame(prop.table(table(survey$c_trabajosGrupo)))
df_perc_c_trabajosGrupo <- df_perc_c_trabajosGrupo %>% arrange(-Freq)
df_perc_c_trabajosGrupo
df_perc_c_trabajosGrupo[df_perc_c_trabajosGrupo$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_c_trabajosGrupo[df_perc_c_trabajosGrupo$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_c_trabajosGrupo[df_perc_c_trabajosGrupo$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_c_trabajosGrupo <- df_perc_c_trabajosGrupo %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_c_trabajosGrupo,by=c("c_trabajosGrupo"="Var1"))
survey <- survey[,!names(survey)%in% c("c_trabajosGrupo")]
names(survey)[33] <- "c_trabajosGrupo"
names(survey)
df_perc_c_trabajosGrupo <- as.data.frame(prop.table(table(survey$c_trabajosGrupo)))
df_perc_c_trabajosGrupo <- df_perc_c_trabajosGrupo %>% arrange(-Freq)
df_perc_c_trabajosGrupo
survey$c_trabajosGrupo <- as.factor(survey$c_trabajosGrupo)
boxplot(df_perc_c_trabajosGrupo$Freq)
hist(df_perc_c_trabajosGrupo$Freq)
qqnorm(df_perc_c_trabajosGrupo$Freq)

#################### EFECTOS ########################


################# EFECTO - CANSANCIO ##################
#Var1       Freq
#1                 Muy de acuerdo 0.39664804
#2                     De acuerdo 0.37430168
#3 Ni de acuerdo ni en desacuerdo 0.12849162
#4                  En desacuerdo 0.07262570
#5              Muy en desacuerdo 0.02793296

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.7709497
#2 Ni de acuerdo ni en desacuerdo 0.1284916
#3                     desacuerdo 0.1005587

df_perc_e_cansancio <- as.data.frame(prop.table(table(survey$e_cansancio)))
df_perc_e_cansancio <- df_perc_e_cansancio %>% arrange(-Freq)
df_perc_e_cansancio
df_perc_e_cansancio[df_perc_e_cansancio$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_cansancio[df_perc_e_cansancio$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_cansancio[df_perc_e_cansancio$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_cansancio <- df_perc_e_cansancio %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_cansancio,by=c("e_cansancio"="Var1"))
survey <- survey[,!names(survey)%in% c("e_cansancio")]
names(survey)[33] <- "e_cansancio"
names(survey)
df_perc_e_cansancio <- as.data.frame(prop.table(table(survey$e_cansancio)))
df_perc_e_cansancio <- df_perc_e_cansancio %>% arrange(-Freq)
df_perc_e_cansancio
survey$e_cansancio <- as.factor(survey$e_cansancio)
boxplot(df_perc_e_cansancio$Freq)
hist(df_perc_e_cansancio$Freq)
qqnorm(df_perc_e_cansancio$Freq)


################### EFECTO - DIFICULTADA PARA DORMIR ###############
#1                 Muy de acuerdo 0.396648045
#2                     De acuerdo 0.296089385
#3 Ni de acuerdo ni en desacuerdo 0.162011173
#4                  En desacuerdo 0.111731844
#5              Muy en desacuerdo 0.027932961
#6                                0.005586592

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.6966292
#2 Ni de acuerdo ni en desacuerdo 0.1629213
#3                     desacuerdo 0.1404494

df_perc_e_dificultadDormir <- as.data.frame(prop.table(table(survey$e_dificultadDormir)))
df_perc_e_dificultadDormir <- df_perc_e_dificultadDormir %>% arrange(-Freq)
df_perc_e_dificultadDormir
df_perc_e_dificultadDormir[df_perc_e_dificultadDormir$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_dificultadDormir[df_perc_e_dificultadDormir$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_dificultadDormir[df_perc_e_dificultadDormir$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_dificultadDormir <- df_perc_e_dificultadDormir %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_dificultadDormir,by=c("e_dificultadDormir"="Var1"))
survey <- survey[,!names(survey)%in% c("e_dificultadDormir")]
names(survey)[33] <- "e_dificultadDormir"
names(survey)
df_perc_e_dificultadDormir <- as.data.frame(prop.table(table(survey$e_dificultadDormir)))
df_perc_e_dificultadDormir <- df_perc_e_dificultadDormir %>% arrange(-Freq)
df_perc_e_dificultadDormir
survey$e_dificultadDormir <- as.factor(survey$e_dificultadDormir)
boxplot(df_perc_e_dificultadDormir$Freq)
hist(df_perc_e_dificultadDormir$Freq)
qqnorm(df_perc_e_dificultadDormir$Freq)


################# EFECTO - NO SE SIENTE TOMADO EN CONSIDERACI? ###############
#Var1       Freq
#1                     De acuerdo 0.32402235
#2 Ni de acuerdo ni en desacuerdo 0.25139665
#3                 Muy de acuerdo 0.20670391
#4                  En desacuerdo 0.16201117
#5              Muy en desacuerdo 0.05586592

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.5307263
#2 Ni de acuerdo ni en desacuerdo 0.2513966
#3                     desacuerdo 0.2178771

df_perc_e_noConsideracion <- as.data.frame(prop.table(table(survey$e_noConsideracion)))
df_perc_e_noConsideracion <- df_perc_e_noConsideracion %>% arrange(-Freq)
df_perc_e_noConsideracion
df_perc_e_noConsideracion[df_perc_e_noConsideracion$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_noConsideracion[df_perc_e_noConsideracion$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_noConsideracion[df_perc_e_noConsideracion$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_noConsideracion <- df_perc_e_noConsideracion %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_noConsideracion,by=c("e_noConsideracion"="Var1"))
survey <- survey[,!names(survey)%in% c("e_noConsideracion")]
names(survey)[33] <- "e_noConsideracion"
names(survey)
df_perc_e_noConsideracion <- as.data.frame(prop.table(table(survey$e_noConsideracion)))
df_perc_e_noConsideracion <- df_perc_e_noConsideracion %>% arrange(-Freq)
df_perc_e_noConsideracion
survey$e_noConsideracion <- as.factor(survey$e_noConsideracion)
boxplot(df_perc_e_noConsideracion$Freq)
hist(df_perc_e_noConsideracion$Freq)
qqnorm(df_perc_e_noConsideracion$Freq)


######################## EFECTO - IRRITABILIDAD EXCESIVA #################
#Var1        Freq
#1                     De acuerdo 0.374301676
#2                 Muy de acuerdo 0.245810056
#3 Ni de acuerdo ni en desacuerdo 0.223463687
#4                  En desacuerdo 0.106145251
#5              Muy en desacuerdo 0.044692737
#6                                0.005586592

#Despu茅s de la transformaci贸n
#Var1      Freq
#1                     De acuerdo 0.6235955
#2 Ni de acuerdo ni en desacuerdo 0.2247191
#3                     desacuerdo 0.1516854

df_perc_e_irritabilidadExcesiva <- as.data.frame(prop.table(table(survey$e_irritabilidadExcesiva)))
df_perc_e_irritabilidadExcesiva <- df_perc_e_irritabilidadExcesiva %>% arrange(-Freq)
df_perc_e_irritabilidadExcesiva
df_perc_e_irritabilidadExcesiva[df_perc_e_irritabilidadExcesiva$Var1 %in% c("En desacuerdo","Muy en desacuerdo"),"categoria"] <- "1"
df_perc_e_irritabilidadExcesiva[df_perc_e_irritabilidadExcesiva$Var1 %in% c("De acuerdo","Muy de acuerdo"),"categoria"] <- "3"
df_perc_e_irritabilidadExcesiva[df_perc_e_irritabilidadExcesiva$Var1 %in% c("Ni de acuerdo ni en desacuerdo"),"categoria"] <- "2"
df_perc_e_irritabilidadExcesiva <- df_perc_e_irritabilidadExcesiva %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_e_irritabilidadExcesiva,by=c("e_irritabilidadExcesiva"="Var1"))
survey <- survey[,!names(survey)%in% c("e_irritabilidadExcesiva")]
names(survey)[33] <- "e_irritabilidadExcesiva"
names(survey)
df_perc_e_irritabilidadExcesiva <- as.data.frame(prop.table(table(survey$e_irritabilidadExcesiva)))
df_perc_e_irritabilidadExcesiva <- df_perc_e_irritabilidadExcesiva %>% arrange(-Freq)
df_perc_e_irritabilidadExcesiva
survey$e_irritabilidadExcesiva <- as.factor(survey$e_irritabilidadExcesiva)
boxplot(df_perc_e_irritabilidadExcesiva$Freq)
hist(df_perc_e_irritabilidadExcesiva$Freq)
qqnorm(df_perc_e_irritabilidadExcesiva$Freq)

################### Transformaci贸n de variable "A帽o acad茅mico" #####################
df_perc_anioAcademico <- as.data.frame(prop.table(table(survey$anioAcademico)))
df_perc_anioAcademico <- df_perc_anioAcademico %>% arrange(-Freq)
df_perc_anioAcademico
df_perc_anioAcademico[df_perc_anioAcademico$Var1 %in% c("ultimo a駉","5to a駉"),"categoria"] <- "carrera parte 4"
df_perc_anioAcademico[df_perc_anioAcademico$Var1 %in% c("1er a駉","2do a駉"),"categoria"] <- "carrera parte 1"
df_perc_anioAcademico[df_perc_anioAcademico$Var1 %in% c("3er a駉"),"categoria"] <- "carrera parte 2"
df_perc_anioAcademico[df_perc_anioAcademico$Var1 %in% c("4to a駉"),"categoria"] <- "carrera parte 3"
df_perc_anioAcademico <- df_perc_anioAcademico %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_anioAcademico,by=c("anioAcademico"="Var1"))
survey <- survey[,!names(survey)%in% c("anioAcademico")]
names(survey)[33] <- "anioAcademico"
names(survey)
df_perc_anioAcademico <- as.data.frame(prop.table(table(survey$anioAcademico)))
df_perc_anioAcademico <- df_perc_anioAcademico %>% arrange(-Freq)
df_perc_anioAcademico
survey$anioAcademico <- as.factor(survey$anioAcademico)
boxplot(df_perc_anioAcademico$Freq)
hist(df_perc_anioAcademico$Freq)
qqnorm(df_perc_anioAcademico$Freq)

################### Transformaci贸n de variable "periodo ansiedad" #####################
df_perc_periodo_ansiedad <- as.data.frame(prop.table(table(survey$periodoAnsiedad)))
df_perc_periodo_ansiedad <- df_perc_periodo_ansiedad %>% arrange(-Freq)
df_perc_periodo_ansiedad
df_perc_periodo_ansiedad[df_perc_periodo_ansiedad$Var1 %in% c("Al iniciar el periodo","Al final de cada parcial"),"categoria"] <- "Primeros parciales"
df_perc_periodo_ansiedad[df_perc_periodo_ansiedad$Var1 %in% c("En los examenes y proyectos finales"),"categoria"] <- "En los examenes y proyectos finales"
df_perc_periodo_ansiedad <- df_perc_periodo_ansiedad %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_periodo_ansiedad,by=c("periodoAnsiedad"="Var1"))
survey <- survey[,!names(survey)%in% c("periodoAnsiedad")]
names(survey)[33] <- "periodoAnsiedad"
names(survey)
df_perc_periodo_ansiedad <- as.data.frame(prop.table(table(survey$periodoAnsiedad)))
df_perc_periodo_ansiedad <- df_perc_periodo_ansiedad %>% arrange(-Freq)
df_perc_periodo_ansiedad
survey$periodoAnsiedad <- as.factor(survey$periodoAnsiedad)
boxplot(df_perc_periodo_ansiedad$Freq)
hist(df_perc_periodo_ansiedad$Freq)
qqnorm(df_perc_periodo_ansiedad$Freq)


df_perc_influenciaEnRendimiento <- as.data.frame(prop.table(table(survey$influenciaEnRendimiento)))
df_perc_influenciaEnRendimiento <- df_perc_influenciaEnRendimiento %>% arrange(-Freq)
df_perc_influenciaEnRendimiento
df_perc_influenciaEnRendimiento[df_perc_influenciaEnRendimiento$Var1 %in% c("Disminuye"),"categoria"] <- "1"
df_perc_influenciaEnRendimiento[df_perc_influenciaEnRendimiento$Var1 %in% c("Aumenta"),"categoria"] <- "3"
df_perc_influenciaEnRendimiento[df_perc_influenciaEnRendimiento$Var1 %in% c("Se mantiene"),"categoria"] <- "2"
df_perc_influenciaEnRendimiento <- df_perc_influenciaEnRendimiento %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_influenciaEnRendimiento,by=c("influenciaEnRendimiento"="Var1"))
survey <- survey[,!names(survey)%in% c("influenciaEnRendimiento")]
names(survey)[33] <- "influenciaEnRendimiento"
names(survey)
df_perc_influenciaEnRendimiento <- as.data.frame(prop.table(table(survey$influenciaEnRendimiento)))
df_perc_influenciaEnRendimiento <- df_perc_influenciaEnRendimiento %>% arrange(-Freq)
df_perc_influenciaEnRendimiento
survey$influenciaEnRendimiento <- as.factor(survey$influenciaEnRendimiento)
summary(survey$influenciaEnRendimiento)
boxplot(df_perc_influenciaEnRendimiento$Freq)
hist(df_perc_influenciaEnRendimiento$Freq)
qqnorm(df_perc_influenciaEnRendimiento$Freq)


################## eliminamos "genero" por que si ###################
survey <- survey[,!(names(survey) %in% c("genero"))] ####20

################## Transformaci髇 de la EDAD ###################### viva joh

prop.table(table(survey$expAnsiedad))
df_perc_expAnsiedad <- as.data.frame(prop.table(table(survey$expAnsiedad)))
df_perc_expAnsiedad <- df_perc_expAnsiedad %>% arrange(-Freq)
df_perc_expAnsiedad

df_perc_edad <- as.data.frame(prop.table(table(survey$edad)))
df_perc_edad <- df_perc_edad %>% arrange(-Freq)
df_perc_edad
df_perc_edad[df_perc_edad$Var1 %in% c("menor de 18 a駉s","19 - 22 a駉s"),"categoria"] <- "menor de 22 a駉s"
df_perc_edad[df_perc_edad$Var1 %in% c("27 - 30 a駉s","mayor de 30 a駉s"),"categoria"] <- "mayor de 27 a駉s"
df_perc_edad[df_perc_edad$Var1 %in% c("23 - 26 a駉s"),"categoria"] <- "23 - 26 a駉s"
df_perc_edad <- df_perc_edad %>% select(Var1,categoria)
survey <- left_join(survey,df_perc_edad,by=c("edad"="Var1"))
survey <- survey[,!names(survey)%in% c("edad")]
names(survey)[32] <- "edad"
names(survey)
df_perc_edad <- as.data.frame(prop.table(table(survey$edad)))
df_perc_edad <- df_perc_edad %>% arrange(-Freq)
df_perc_edad
survey$edad <- as.factor(survey$edad)
boxplot(df_perc_edad$Freq)
hist(df_perc_edad$Freq)
qqnorm(df_perc_edad$Freq)

write.csv(survey,"survey_limpio_transfor_EC.csv",row.names = FALSE)####27 solo lo hice para ver como se guardaba


