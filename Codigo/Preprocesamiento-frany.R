## Cargamos el csv
library(tidyverse)

## Leyendo tabla, activ ando los headers y definiendo el separados como el simbolo "|"
licenciamiento <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/LicenciamientoInstitucional.csv", header = TRUE, sep="|")
carnes <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/CarnesUniversitarios2018.csv", header = TRUE, sep="|")
programas <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/ProgramasDeUniversidades.csv", header = TRUE, sep="|")

# Revisando si los tipos de valores corresponden a la cabecera
for (i in colnames(licenciamiento)){
	cat(i, ": " ,class(licenciamiento[[i]]), "\nEjemplo:",licenciamiento[[i]][1], sep = "", end = "\n\n")
}

for (i in colnames(carnes)){
	cat(i, ": " ,class(carnes[[i]]), "\nEjemplo:",carnes[[i]][1], sep = "", end = "\n\n")
}

for (i in colnames(programas)){
	cat(i, ": " ,class(programas[[i]]), "\nEjemplo:",programas[[i]][1], sep = "", end = "\n\n")
}

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

head(licenciamiento)
head(carnes)
head(programas)

# eliminar los espacios en blanco de DEPARTAMENTO_FILIAL
carnes[carnes$DEPARTAMENTO_FILIAL == '',4] <- NA

# eliminar los espacios en blanco de PERIODO_LICENCIAMIENTO
licenciamiento[licenciamiento$PERIODO_LICENCIAMIENTO == 0, 5] <- NA

write.csv(licenciamiento,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/licenciamiento.csv')
write.csv(programas,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/programas.csv')
write.csv(carnes,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/carnes.csv')

# Consultas
#- universidades que antes tenian licencia y ahora ya no
#- universidades que cuentan con 6 años de licenciamiento y pertenecen al departamento de lima
#- Cuantos estudiantes estan en universidades no licenciadas de ICA
#- Cuantos estudiantes de universidades privadas estan estudiando CC
#- Cuantas universidades que cuentan con postgrado se encuentran fuera de LIMA
