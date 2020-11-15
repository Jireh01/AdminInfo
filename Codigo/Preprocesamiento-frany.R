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

a <- c('A', 'E', 'I', 'O', 'U')
b <- c('Á', 'É', 'Í', 'Ó', 'Ú')

x <- ('AAA')
str_replace_all(x, a, b)

'''
## Reemplazando el simbolo "|" con una coma para su transformacion mas sencilla a dataframe
lic <- ("Otros/LicenciamientoInstitucional.csv")
x <- readLines(lic)
y <- gsub("[|]",",", x)
cat(y, file="nombre.csv", sep="\n")
limpio <- read.csv("nombre.csv")
head(limpio)

'''