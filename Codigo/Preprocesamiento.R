## Cargamos el csv
library(tidyverse)



## Leyendo tabla, activando los headers y definiendo el separados como el simbolo "|"
licenciamiento <- read.table("Otros/LicenciamientoInstitucional.csv", sep="|", header = TRUE)
head(licenciamiento)

#strings.as.factors = FALSE

class(licenciamiento$CODIGO_ENTIDAD)
class(licenciamiento$ESTADO_LICENCIAMIENTO)

class(licenciamiento$LATITUD_uBICACION)


'''
## Reemplazando el simbolo "|" con una coma para su transformacion mas sencilla a dataframe
lic <- ("Otros/LicenciamientoInstitucional.csv")
x <- readLines(lic)
y <- gsub("[|]",",", x)
cat(y, file="nombre.csv", sep="\n")
limpio <- read.csv("nombre.csv")
head(limpio)

'''