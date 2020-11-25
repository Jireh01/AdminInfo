# Cargamos las librerias que utilizaremos
library(tidyverse)

# Cargamos los csv que generamos en el paso anterior
licenciamiento <- read.csv("Datasets/1-Convertidos a CSV/licenciamiento.csv")
carnes <- read.csv("Datasets/1-Convertidos a CSV/carnes.csv")
programas <- read.csv("Datasets/1-Convertidos a CSV/programas.csv")

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

# Reemplazar los espacios en blanco de los dataFrames por NA
removerEnBlancos <- function(dataFrame){
	for (i in colnames(dataFrame)){
		dataFrame[[i]][dataFrame[[i]] == ''] <- NA
	}
	return(dataFrame)
}
licenciamiento <- removerEnBlancos(licenciamiento)
carnes <- removerEnBlancos(carnes)
programas <- removerEnBlancos(programas)

# Remover las lineas con mas de 2 columnas con NA
licenciamiento <- licenciamiento[rowSums(is.na(licenciamiento)) < 2, ]
carnes <- carnes[rowSums(is.na(carnes)) < 2, ]
programas <- programas[rowSums(is.na(programas)) < 2, ]

# Remover S.A.C. y S.A. de los nombres
quitarSAC <- function(dataFrame,i) {
	toRemove <- c(" S.A.C."," S.A.C"," S.A.", " S.A")
	for (j in seq_along(dataFrame[[i]])){
		for (k in seq_along(toRemove)){
			dataFrame[[i]][j] <- str_remove(dataFrame[[i]][j],toRemove[k])
		}
	}
	return(dataFrame)
}

carnes <- quitarSAC(carnes,3)
licenciamiento <- quitarSAC(licenciamiento,3)
programas <- quitarSAC(programas,3)

# Remover las columnas innecesarias
licenciamiento <- licenciamiento[,-(0:1), drop = FALSE]
carnes <- carnes[,-(0:1), drop = FALSE]
programas <- programas[,-(0:1), drop = FALSE]

# Convirtiendo string a entero
carnes$CODIGO <- strtoi(carnes$CODIGO)
carnes$CODIGO_CLASE_PROGRAMA <- strtoi(carnes$CODIGO_CLASE_PROGRAMA)
carnes$Cant_Carnes <- strtoi(carnes$Cant_Carnes)
carnes$ANIO_PERIODO <- strtoi(carnes$ANIO_PERIODO)
licenciamiento$CODIGO_ENTIDAD <- strtoi(licenciamiento$CODIGO_ENTIDAD)
licenciamiento$PERIODO_LICENCIAMIENTO <- strtoi(licenciamiento$PERIODO_LICENCIAMIENTO)
programas$CODIGO_ENTIDAD <- strtoi(programas$CODIGO_ENTIDAD)
programas$PERIODO_LICENCIAMIENTO <- strtoi(programas$PERIODO_LICENCIAMIENTO)
programas$CODIGO_CLASE_PROGRAMA_N2 <- stroi(programas$CODIGO_CLASE_PROGRAMA_N2)

# Guardamos los dataframes limpios
write.csv(licenciamiento, file="Datasets/2-Preprocesados/licenciamiento.csv", row.names=F)
write.csv(carnes, file="Datasets/2-Preprocesados/carnes.csv", row.names=F)
write.csv(programas, file="Datasets/2-Preprocesados/programas.csv", row.names=F)



