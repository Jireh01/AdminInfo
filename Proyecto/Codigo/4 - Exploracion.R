# Cargamos las librerias necesarias
library(tidyverse)
library(dplyr)

## TRANSFORMACION

# Cargamos los datasets preprocesados
licenciamiento <- read.csv("Datasets/2-Preprocesados/licenciamiento.csv")
carnes <- read.csv("Datasets/2-Preprocesados/carnes.csv")
programas <- read.csv("Datasets/2-Preprocesados/programas.csv")

# Universidad y si esta licenciada o no
Licenciado <- licenciamiento %>% mutate(LICENCIAMIENTO = ifelse(ESTADO_LICENCIAMIENTO == "LICENCIA DENEGADA", "NO", "SI")) %>%
				select(CODIGO_ENTIDAD,NOMBRE,LICENCIAMIENTO,PERIODO_LICENCIAMIENTO)

# Cuantos programas tiene cada universidad
ProgramasTotal <- programas %>% group_by(NOMBRE) %>% summarize(PROGRAMAS_TOTAL= n())

# Cuantos carnes tiene cada universidad
CarnesTotal <- carnes %>% group_by(NOMBRE_UNIVERSIDAD) %>% summarize(CANTIDAD_CARNES = sum(Cant_Carnes))

# Hacemos un nuevo dataframe con estos queries
resumen_sunedu <- Licenciado %>% inner_join(ProgramasTotal, by=c("NOMBRE"="NOMBRE")) %>%
			inner_join(CarnesTotal, by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
			arrange(CODIGO_ENTIDAD)

# Revisamos si la estructura esta bien puesta
head(resumen_sunedu)

# Guardamos este nuevo data.frame
write.csv(resumen_sunedu, file="Datasets/3-Generado/resumen_sunedu.csv", row.names=F)
