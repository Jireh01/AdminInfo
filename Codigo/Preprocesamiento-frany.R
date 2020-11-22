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

# Quitamos las tildes y Ñs para que no haya problemas con la base de datos en linea con esta funcion
quitarTildes <- function(dataFrame){
	conTilde <- c("Á", "É", "Í", "Ó", "Ú", "Ñ", "á", "é", "í", "ó", "ú")
	sinTilde <- c("A", "E", "I", "O", "U", "N", "a", "e", "i", "o", "u")

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
#licenciamientoEx <- quitarTildes(licenciamientoEx)
carnesEx <- quitarTildes(carnesEx)

quitarSAC <- function(dataFrame) {
	for (i in colnames(dataFrame)) {
		# dataFrame[[i]] <- rev(dataFrame[[i]])
		# paste(dataFrame[[i]], collapse='')
		if (str_extract(dataFrame[[i]], ' S.A.C.') != NA){
			
		} else if (str_extract(dataFrame[[i]], ' S.A.') != NA) {

		}
	}
	return(dataFrame)
}

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
#licenciamientoEx <- removerBlancos(licenciamientoEx)
carnesEx <- removerBlancos(carnesEx)

# remover las lineas con mas de 2 NA
licenciamiento <- licenciamiento[rowSums(is.na(licenciamiento)) < 2, ]
#carnes <- carnes[rowSums(is.na(carnes)) < 2, ]
programas <- programas[rowSums(is.na(programas)) < 2, ]
#licenciamientoEx <- licenciamientoEx[rowSums(is.na(licenciamientoEx)) < 2, ]
carnesEx <- carnesEx[rowSums(is.na(carnesEx)) < 2, ]

# quitar " S.A.C."" y " S.A."" en los nombres de licenciamiento y programa
	# una manera MUY FEA de hacerlo (por mientras)
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA SAN CARLOS S.A.C.","UNIVERSIDAD PRIVADA SAN CARLOS")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD CONTINENTAL S.A.C.","UNIVERSIDAD CONTINENTAL")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA JUAN PABLO II S.A.C.","UNIVERSIDAD PRIVADA JUAN PABLO II")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD LE CORDON BLEU S.A.C.","UNIVERSIDAD LE CORDON BLEU")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA SAN JUAN BAUTISTA S.A.C.","UNIVERSIDAD PRIVADA SAN JUAN BAUTISTA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DE ARTE ORVAL S.A.C.","UNIVERSIDAD PERUANA DE ARTE ORVAL")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD GLOBAL DEL CUSCO S.A.C.","UNIVERSIDAD GLOBAL DEL CUSCO")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA TELESUP S.A.C.","UNIVERSIDAD PRIVADA TELESUP")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD MARITIMA DEL PERU S.A.C.","UNIVERSIDAD MARITIMA DEL PERU")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD CESAR VALLEJO S.A.C.","UNIVERSIDAD CESAR VALLEJO")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"GRUPO EDUCATIVO UNIVERSIDAD PRIVADA DE ICA S.A.C.","GRUPO EDUCATIVO UNIVERSIDAD PRIVADA DE ICA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA AUSTRAL DEL CUSCO S.A.C.","UNIVERSIDAD PERUANA AUSTRAL DEL CUSCO")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD SAN IGNACIO DE LOYOLA S.A.","UNIVERSIDAD SAN IGNACIO DE LOYOLA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DE INVESTIGACION Y NEGOCIOS S.A.C.","UNIVERSIDAD PERUANA DE INVESTIGACION Y NEGOCIOS")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD CIENTIFICA DEL SUR S.A.C.","UNIVERSIDAD CIENTIFICA DEL SUR")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA DE HUANCAYO FRANKLIN ROOSEVELT S.A.C.","UNIVERSIDAD PRIVADA DE HUANCAYO FRANKLIN ROOSEVELT")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD AUTONOMA SAN FRANCISCO S.A.C.","UNIVERSIDAD AUTONOMA SAN FRANCISCO")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA ANTONIO GUILLERMO URRELO S.A.C.","UNIVERSIDAD PRIVADA ANTONIO GUILLERMO URRELO")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD POLITECNICA AMAZONICA S.A.C.","UNIVERSIDAD POLITECNICA AMAZONICA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA ARZOBISPO LOAYZA S.A.C.","UNIVERSIDAD PRIVADA ARZOBISPO LOAYZA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD ALAS PERUANAS S.A.","UNIVERSIDAD ALAS PERUANAS")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DEL ORIENTE S.A.C.","UNIVERSIDAD PERUANA DEL ORIENTE")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD INTERAMERICANA PARA EL DESARROLLO S.A.C.","UNIVERSIDAD INTERAMERICANA PARA EL DESARROLLO")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD LATINOAMERICANA CIMA S.A.C.","UNIVERSIDAD LATINOAMERICANA CIMA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD AUTONOMA DE ICA S.A.C.","UNIVERSIDAD AUTONOMA DE ICA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD CIENCIAS DE LA SALUD S.A.C.","UNIVERSIDAD CIENCIAS DE LA SALUD")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA SERGIO BERNALES S.A.","UNIVERSIDAD PRIVADA SERGIO BERNALES")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD DE AYACUCHO FEDERICO FROEBEL S.A.C.","UNIVERSIDAD DE AYACUCHO FEDERICO FROEBEL")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA DE LA SELVA PERUANA S.A.C.","UNIVERSIDAD PRIVADA DE LA SELVA PERUANA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD SANTO DOMINGO DE GUZMAN S.A.C.","UNIVERSIDAD SANTO DOMINGO DE GUZMAN")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DE CIENCIAS APLICADAS S.A.C.","UNIVERSIDAD PERUANA DE CIENCIAS APLICADAS")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA SANTO TOMAS DE AQUINO DE CIENCIA E INTEGRACION S.A.C.","UNIVERSIDAD PERUANA SANTO TOMAS DE AQUINO DE CIENCIA E INTEGRACION")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA SIMON BOLIVAR S.A.C.","UNIVERSIDAD PERUANA SIMON BOLIVAR")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD AUTONOMA DEL PERU S.A.C.","UNIVERSIDAD AUTONOMA DEL PERU")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA JUAN MEJIA BACA S.A.C.","UNIVERSIDAD PRIVADA JUAN MEJIA BACA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA NORBERT WIENER S.A.","UNIVERSIDAD PRIVADA NORBERT WIENER")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DE INTEGRACION GLOBAL S.A.C.","UNIVERSIDAD PERUANA DE INTEGRACION GLOBAL")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA DE PUCALLPA S.A.C.","UNIVERSIDAD PRIVADA DE PUCALLPA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA LEONARDO DA VINCI S.A.C.","UNIVERSIDAD PRIVADA LEONARDO DA VINCI")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA DEL NORTE S.A.C.","UNIVERSIDAD PRIVADA DEL NORTE")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD SAN ANDRES S.A.C.","UNIVERSIDAD SAN ANDRES")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD MARIA AUXILIADORA S.A.C.","UNIVERSIDAD MARIA AUXILIADORA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD SEÑOR DE SIPAN S.A.C.","UNIVERSIDAD SEÑOR DE SIPAN")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA SISE S.A.C.","UNIVERSIDAD PRIVADA SISE")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD DE CIENCIAS Y ARTES DE AMERICA LATINA S.A.C.","UNIVERSIDAD DE CIENCIAS Y ARTES DE AMERICA LATINA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA LIDER PERUANA S.A.C.","UNIVERSIDAD PRIVADA LIDER PERUANA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA AUTONOMA DEL SUR S.A.C.","UNIVERSIDAD PRIVADA AUTONOMA DEL SUR")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DE CIENCIAS E INFORMATICA S.A.C.","UNIVERSIDAD PERUANA DE CIENCIAS E INFORMATICA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PRIVADA PERUANO ALEMANA S.A.C.","UNIVERSIDAD PRIVADA PERUANO ALEMANA")
licenciamiento$NOMBRE<- str_replace_all(licenciamiento$NOMBRE,"UNIVERSIDAD PERUANA DE LAS AMERICAS S.A.C.","UNIVERSIDAD PERUANA DE LAS AMERICAS")

### OTRA MANERA SERIA INVIERITNEDO LOS ARREGLOS DE NOMBRES Y TODOS LOS QUE COMIENCEN POR '.' HASTA 'S' eliminar
head(licenciamiento)

# eliminar los espacios en blanco de DEPARTAMENTO_FILIAL
# carnes[carnes$DEPARTAMENTO_FILIAL == '',4] <- NA

# eliminar los espacios en blanco de PERIODO_LICENCIAMIENTO
# licenciamiento[licenciamiento$PERIODO_LICENCIAMIENTO == 0, 5] <- NA


write.csv(licenciamiento,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/licenciamiento.csv')
write.csv(programas,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/programas.csv')
# write.csv(carnes,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/carnes.csv')
write.xlsx(carnesEx, file = "Otros/carnesEx.xlsx", row.names = TRUE, append = TRUE, sheetName = "carnesEx")

# Consultas
#- universidades que antes tenian licencia y ahora ya no
#- universidades que cuentan con 6 años de licenciamiento y pertenecen al departamento de lima
#- Cuantos estudiantes estan en universidades no licenciadas de ICA
#- Cuantos estudiantes de universidades privadas estan estudiando CC
#- Cuantas universidades que cuentan con postgrado se encuentran fuera de LIMA
