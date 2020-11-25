# Cargamos las librerias necesarias
library(tidyverse)
library(dplyr)

# Cargamos los datasets preprocesados
licenciamiento = read.csv("Datasets/2-Preprocesados/licenciamiento.csv")
carnes = read.csv("Datasets/2-Preprocesados/carnes.csv")
programas = read.csv("Datasets/2-Preprocesados/programas.csv")

# Convirtiendo string a entero
carnes$Cant_Carnes <- strtoi(carnes$Cant_Carnes)


# Universidad y si esta licenciada o no
Licenciado <- licenciamiento %>% mutate(LICENCIAMIENTO = ifelse(ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA", "SI", "NO")) %>%
				select(CODIGO_ENTIDAD,NOMBRE,LICENCIAMIENTO,PERIODO_LICENCIAMIENTO)

# Cuantos programas tiene cada universidad
ProgramasTotal <- programas %>% group_by(NOMBRE) %>% summarise(PROGRAMAS_TOTAL= n())

# Cuantos carnes tiene cada universidad
CarnesTotal <- carnes %>% group_by(NOMBRE_UNIVERSIDAD) %>% summarise(CANTIDAD_CARNES = sum(Cant_Carnes))

# Arreglando las clases de los 
Licenciado$CODIGO_ENTIDAD <- strtoi(Licenciado$CODIGO_ENTIDAD)
Licenciado$PERIODO_LICENCIAMIENTO <- strtoi(Licenciado$PERIODO_LICENCIAMIENTO)

# Hacemos un nuevo dataframe
resumen_sunedu <- Licenciado %>% inner_join(ProgramasTotal, by=c("NOMBRE"="NOMBRE")) %>%
			inner_join(CarnesTotal, by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
			arrange(CODIGO_ENTIDAD)


# Guardamos este nuevo data.frame
write.csv(resumen_sunedu, file="Datasets/3-Generado/resumen_sunedu.csv", row.names=F)
