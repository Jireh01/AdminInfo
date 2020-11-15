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

tildes <- c('A', 'E', 'I', 'O', 'U')
reemplazo <- c('Á', 'É', 'Í', 'Ó', 'Ú')
 
head(carnes)
reemplazar <- function(dataFrame, porReemplazar, reemplazo){
	for (i in seq_along(porReemplazar)){
		for (j in colnames(dataFrame)){
			dataFrame[[i]] <- str_replace_all(dataFrame[[i]], porReemplazar[i], reemplazo[i])
		}
	}
}

reemplazar(carnes, tildes, reemplazo)
reemplazar(licenciamiento, tildes, reemplazo)
reemplazar(programas, tildes, reemplazo)

head(carnes)

carnes[carnes$DEPARTAMENTO_FILIAL == '',4] <- NA

write.csv(carnes,file='C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Semana 11/Trabajo Final Adminfo/Otros/carnes.csv')

