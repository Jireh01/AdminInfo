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
	conTilde <- c("Á", "É", "Í", "Ó", "Ú", "Ñ", "Ü", "á", "é", "í", "ó", "ú", "ü")
	sinTilde <- c("A", "E", "I", "O", "U", "N", "U", "a", "e", "i", "o", "u", "ü")

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

# reemplazar los espacios en blanco de los dataFrames por NA
removerBlancos <- function(dataFrame){
	for (i in colnames(dataFrame)){
		dataFrame[[i]][dataFrame[[i]] == ''] <- NA
	}
	return(dataFrame)
}

licenciamiento <- removerBlancos(licenciamiento)
carnes <- removerBlancos(carnes)
programas <- removerBlancos(programas)

# remover las lineas con mas de 2 NA
licenciamiento <- licenciamiento[rowSums(is.na(licenciamiento)) < 2, ]
carnes <- carnes[rowSums(is.na(carnes)) < 2, ]
programas <- programas[rowSums(is.na(programas)) < 2, ]

# quitar " S.A.C."" y " S.A."" en los nombres de licenciamiento y programa


# Guardamos los dataframes limpios
write.csv(licenciamiento, file="Otros/licenciamiento.csv")
write.csv(carnes, file="Otros/carnes.csv")
write.csv(programas, file="Otros/programas.csv")






####
#TODO:
# ASK FOR MODELADO (FK? E-R?)
# ASK los outliers para nuestras bases de datos
# ASK Informe automático

## Reemplazando el simbolo "|" con una coma para su transformacion mas sencilla a dataframe
lic <- ("Otros/LicenciamientoInstitucional.csv")
x <- readLines(lic)
y <- gsub("[|]",",", x)
cat(y, file="nombre.csv", sep="\n")
limpio <- read.csv("nombre.csv")
head(limpio)