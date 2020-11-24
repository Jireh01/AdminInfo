# install.packages("xlsx")                                         # Install xlsx R package
library("xlsx") 

# install.packages("readxl")                                       # Install readxl R package
library("readxl")

## Cargamos el csv
library(tidyverse)
library(stringr)

## Leyendo tabla, activ ando los headers y definiendo el separados como el simbolo "|"
licenciamiento <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/LicenciamientoInstitucional.csv", header = TRUE, sep="|")
carnes <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/CarnesUniversitarios2018.csv", header = TRUE, sep="|")
programas <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/ProgramasDeUniversidades.csv", header = TRUE, sep="|")

licenciamientoEx <- read_excel("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/Licenciamiento Institucional_5.xls")
carnesEx <- read_excel("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/Carnes Universitarios 2018.xls")

# Revisando si los tipos de valores corresponden a la cabecera
revisarValores <- function(dataFrame){
	for (i in colnames(dataFrame)){
		cat(i, ": " ,class(dataFrame[[i]]), "\nEjemplo:",dataFrame[[i]][1], sep = "", end = "\n\n")
	}
}
revisarValores(licenciamiento)
revisarValores(carnes)
revisarValores(programas)
revisarValores(licenciamientoEx)
revisarValores(carnesEx)

# convertir el periodo de licenciamiento a numero
licenciamiento$PERIODO_LICENCIAMIENTO <- as.integer(licenciamiento$PERIODO_LICENCIAMIENTO)

# Quitamos las tildes y Ñs para que no haya problemas con la base de datos en linea con esta funcion
quitarTildes <- function(dataFrame){
	conTilde <- c("Á", "É", "Í", "Ó", "Ú", "Ñ", "á", "é", "í", "ó", "ú", "ñ")
	sinTilde <- c("A", "E", "I", "O", "U", "N", "a", "e", "i", "o", "u", "n")

	for (i in seq_along(conTilde)){
		for (j in colnames(dataFrame)){
			dataFrame[[j]] <- str_replace_all(dataFrame[[j]], conTilde[i], sinTilde[i])
		}
	}
	return(dataFrame)
}

licenciamiento <- quitarTildes(licenciamiento)
#carnes <- quitarTildes(carnes)
programas <- quitarTildes(programas)
carnesEx <- quitarTildes(carnesEx)

quitarSAC <- function(dataFrame) {
	for (i in colnames(dataFrame)) {
		# dataFrame[[i]] <- rev(dataFrame[[i]])
		# paste(dataFrame[[i]], collapse='')
		if (sub(dataFrame[[i]], pattern = " S.A.C.", replacement = "") == sub(dataFrame[[i]], pattern = " S.A.C.", replacement = "")){
			dataFrame[[i]] <- sub(dataFrame[[i]], pattern = " S.A.C.", replacement = "")
		} 
		if (sub(dataFrame[[i]], pattern = " S.A.", replacement = "") == sub(dataFrame[[i]], pattern = " S.A.", replacement = "")) {
			dataFrame[[i]] <- sub(dataFrame[[i]], pattern = " S.A.", replacement = "")
		}
	}
	return(dataFrame)
}
licenciamiento <- quitarSAC(licenciamiento)
carnesEx <- quitarSAC(carnesEx)
programas <- quitarSAC(programas)

head(licenciamiento)
head(programas)
head(carnesEx)
#prueba <- gsub("\\s*\\w*$", "", prueba)

# Reemplazamos los valores en blanco con NA
removerBlancos <- function(dataFrame){
	for (i in colnames(dataFrame)){
		dataFrame[[i]][dataFrame[[i]] == ''] <- NA
	}
	return(dataFrame)
}

licenciamiento <- removerBlancos(licenciamiento)
#carnes <- removerBlancos(carnes)
programas <- removerBlancos(programas)
carnesEx <- removerBlancos(carnesEx)

# remover las lineas con mas de 2 NA
licenciamiento <- licenciamiento[rowSums(is.na(licenciamiento)) < 2, ]
#carnes <- carnes[rowSums(is.na(carnes)) < 2, ]
programas <- programas[rowSums(is.na(programas)) < 2, ]
carnesEx <- carnesEx[rowSums(is.na(carnesEx)) < 2, ]


head(licenciamiento)
head(programas)
head(carnesEx)

write.csv(licenciamiento,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/licenciamiento.csv')
write.csv(programas,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/programas.csv')
# write.csv(carnes,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/carnes.csv')
write.xlsx(carnesEx, file = "C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/carnesEx.xlsx", row.names = TRUE, append = TRUE, sheetName = "carnes")

###################################################################################################
