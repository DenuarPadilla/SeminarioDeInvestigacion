#### LIMPIEZA - SI, NO

library(dplyr)
###################################

setwd("/") ####1 RAIZ
setwd("Users/jklev/Documents/UNAH/Clases/Seminario/cap 4/survey analysis/csv/") ####2 DIRECTORIO

survey <- read.csv("survey_limpio_transfor_EC.csv", sep = ",", header = T)

level_one <- c()
si_no <- c()

for (myname in names(survey)) {
  
  unique_values <- unique(survey[,myname])
  
  if (length(unique_values) == 1 ) {
    level_one <- c(level_one, myname)
  }else{
    
    validations <- sum(unique(survey[,myname]) %in% c("No", "Sí", ""))
    if (validations == 3 ) {
      si_no <- c(si_no, myname)
    }
  }
  
}

for (col in si_no) {
  survey [ survey[,co] == "", col] <- "No"
}

level_one
si_no

write.csv(survey,"survey_limpio_transfor_EC.v2.csv",row.names = FALSE)####27 solo lo hice para ver como se guardaba
