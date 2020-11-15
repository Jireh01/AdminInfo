## Cargamos el csv
library(tidyverse)

## Leyendo tabla, activando las cabeceras y definiendo el separados como el simbolo "|"
licenciamiento <- read.table("Otros/LicenciamientoInstitucional.csv", header = TRUE, sep="|")
carnes <- read.table("Otros/CarnesUniversitarios2018.csv", header = TRUE, sep="|")
programas <- read.table("Otros/ProgramasDeUniversidades.csv", header = TRUE, sep="|")

# Revisando si los tipos de valores corresponden a la cabecera
revisarValores <- function(dataFrame){
	for (i in colnames(dataFrame)){
		cat(i, ": " ,class(dataFrame[[i]]), "\nEjemplo:",dataFrame[[i]][1], sep = "", end = "\n\n")
	}
}

revisarValores(licenciamiento)
revisarValores(carnes)
revisarValores(programas)

# Quitamos las tildes para que no haya problemas con la base de datos en linea con esta funcion
quitarTildes <- function(dataFrame){
	conTilde <- c("Á", "É", "Í", "Ó", "Ú", "á", "é", "í", "ó", "ú")
	sinTilde <- c("A", "E", "I", "O", "U", "a", "e", "i", "o", "u")

	for (i in seq_along(conTilde)){
		for (j in colnames(dataFrame)){
			dataFrame[[j]] <- str_replace_all(dataFrame[[j]], conTilde[i], sinTilde[i])
		}
	}
	return(dataFrame)
}

licenciamiento <- quitarTildes(licenciamiento)
carnes <- quitarTildes(carnes)
programas <- quitarTildes(programas)

# Reemplazamos los valores en blanco con NA

plot(mtcars)

# eliminar los espacios en blanco de DEPARTAMENTO_FILIAL
carnes[carnes$DEPARTAMENTO_FILIAL == '',4] <- NA





'''
#TODO:
#ASK FOR MODELADO (FK? E-R?)


## Reemplazando el simbolo "|" con una coma para su transformacion mas sencilla a dataframe
lic <- ("Otros/LicenciamientoInstitucional.csv")
x <- readLines(lic)
y <- gsub("[|]",",", x)
cat(y, file="nombre.csv", sep="\n")
limpio <- read.csv("nombre.csv")
head(limpio)

'''