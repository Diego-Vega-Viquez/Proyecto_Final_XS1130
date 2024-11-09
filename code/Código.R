#############
# LIBRERIAS #
#############

# Carga los paquetes
library(haven)
library(tidyverse)
#####
# CARGA DATOS
#####
ACTUALIDADES2022 <- as.data.frame(read_dta("https://raw.githubusercontent.com/Diego-Vega-Viquez/Proyecto_Final_XS1130/main/data/ACTUALIDADES2022.dta"))

##########################
# Selección de variables #
##########################

datos <- ACTUALIDADES2022 %>% select(SbjNum, Date, Duration, 
                                     matches("^RC[0-79]\\D?"), matches("^A\\d"), matches("^CS[0-79]"),
                                     edad, edad_imputada, edu, edu_imputada, factor, etnia)

#####################
# Limpieza de datos #
#####################

datos_limpios <- datos

# Elimina las variables CS13
datos_limpios <- datos_limpios %>% select(-c(CS13,CS13_NOMBRE))

# Elimina a las personas ecuestadas que no saben quien es Rodrigo Chavez
datos_limpios <- datos_limpios %>% filter(datos_limpios$RC1 != 6) 

# Combierte todos los 9 en NA de la parte de Chavez (RC)
for (j in c(1:7, 9:11)) {
  variable <- paste0("RC",as.character(j))
  datos_limpios <- datos_limpios %>% mutate(across(all_of(variable), 
                                                   ~ ifelse(. == 9, NA, .)))
}

# Combierte todos los 9 y 999 en NA de la parte de Sociodemográficas (CS)
for (j in c(1:7, 9:10)) {
  variable <- paste0("CS",as.character(j))
  datos_limpios <- datos_limpios %>% mutate(across(all_of(variable), 
                                                   ~ ifelse(. == 9 | . == 999, NA, .)))
}

# for (j in c(1:7, 9:10)) {
#   variable <- paste0("CS",as.character(j))
#   print(summary(datos_limpios[[variable]]))
# }

######################
# Formatea variables #
######################

datos_formateados <- datos_limpios
datos_formateados$Date <- as.Date(datos_formateados$Date)
datos_formateados$Duration <- as.difftime(datos_formateados$Duration)
# Datos de Chavez

for (j in c(1:7, 9:11)) {
  variable <- paste0("RC", as.character(j))
  datos_formateados[[variable]] <- as.factor(datos_formateados[[variable]])
}

# datos_formateados$RC1 <- as.factor(datos_formateados$RC1)
# datos_formateados$RC2 <- as.factor(datos_formateados$RC2)
# datos_formateados$RC3 <- as.factor(datos_formateados$RC3)
# datos_formateados$RC4 <- as.factor(datos_formateados$RC4)
# datos_formateados$RC5 <- as.factor(datos_formateados$RC5)
# datos_formateados$RC6 <- as.factor(datos_formateados$RC6)
# datos_formateados$RC7 <- as.factor(datos_formateados$RC7)
# datos_formateados$RC9 <- as.factor(datos_formateados$RC9)
# datos_formateados$RC10 <- as.factor(datos_formateados$RC10)
# datos_formateados$RC11 <- as.factor(datos_formateados$RC11)


# Datos Sociodemográficos

datos_formateados$CS1 <- as.factor(datos_formateados$CS1)
datos_formateados$CS2 <- as.numeric(datos_formateados$CS2)
datos_formateados$CS3 <- as.factor(datos_formateados$CS3)
datos_formateados$CS4 <- as.factor(datos_formateados$CS4)
datos_formateados$CS5 <- as.factor(datos_formateados$CS5)
datos_formateados$CS6 <- as.factor(datos_formateados$CS6)
datos_formateados$CS7 <- as.factor(datos_formateados$CS7)
datos_formateados$CS9 <- as.factor(datos_formateados$CS9)
datos_formateados$CS10 <- as.numeric(datos_formateados$CS10)
datos_formateados$CS11A <- as.numeric(datos_formateados$CS11A)
datos_formateados <- datos_formateados %>% mutate(CS11A = ifelse(CS11A == 99, NA, CS11A))
datos_formateados$CS11B <- as.numeric(datos_formateados$CS11B)
datos_formateados <- datos_formateados %>% mutate(CS11B = ifelse(CS11B == 99, NA, CS11B))
datos_formateados$CS11C <- as.numeric(datos_formateados$CS11C)
datos_formateados <- datos_formateados %>% mutate(CS11C = ifelse(CS11C == 99, NA, CS11C))
datos_formateados$CS12P <- as.factor(datos_formateados$CS12P)
datos_formateados <- datos_formateados %>% mutate(CS12P = na_if(CS12P, "9. NS/NR")) %>% droplevels()
datos_formateados$CS12C <- as.factor(datos_formateados$CS12C)
datos_formateados <- datos_formateados %>% mutate(CS12C = na_if(CS12C, "9. NS/NR")) %>%
                                           mutate(CS12C = na_if(CS12C, "9.NS/NR")) %>% droplevels()
datos_formateados$CS12D <- as.factor(datos_formateados$CS12D)
datos_formateados <- datos_formateados %>% mutate(CS12D = na_if(CS12D, "9. NS/NR")) %>% 
                                           mutate(CS12D = na_if(CS12D, "9.NS/NR")) %>% 
                                           mutate(CS12D = na_if(CS12D, "-1"))  %>% droplevels()

# La CS13 no tiene nada que ver con el presente trabajo

datos_formateados$A1 <- as.numeric(datos_formateados$A1)
datos_formateados$A3 <- as.numeric(datos_formateados$A3)
datos_formateados$A6 <- as.numeric(datos_formateados$A6)
datos_formateados$A7 <- as.factor(datos_formateados$A7)
datos_formateados$edad <- as.factor(datos_formateados$edad)
datos_formateados$edad_imputada <- as.factor(datos_formateados$edad_imputada)
datos_formateados$edu <- as.factor(datos_formateados$edu)
datos_formateados$edu_imputada <- as.factor(datos_formateados$edu_imputada)
datos_formateados$factor <- as.numeric(datos_formateados$factor)
datos_formateados$etnia <- as.factor(datos_formateados$etnia)

###########################
# Selección de la muestra #
###########################

set.seed(123)
muestra <- datos_formateados[sample(500, replace = F),]

################
# Versión de R #
################

R.version.string

########################
# Preguntas Utilizadas #
########################

labels <- as.data.frame(tibble(Código = names(datos),
                               Variable = map_chr(datos, 
                                                  ~ attr(.x, 
                                                         "label", 
                                                         exact = TRUE
                                                         ) %||% NA
                                                  ) # Extrae el label o NA si no existe 
                               )
                        )
############################
# Descripción de los datos #
############################

# Muestra cómo se ven los datos

glimpse(head(muestra))

# Muestra cuantas variables hay de cada tipo de dato (cuali & cuanti)
tipos <- c()
for (word in colnames(muestra)) {
  tipos <- c(tipos,class(muestra[[word]]))
}
summary(as.factor(tipos))








