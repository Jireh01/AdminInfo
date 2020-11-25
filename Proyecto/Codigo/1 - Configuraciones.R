# Activando las librerias
library(RMySQL) # nos permite usar MySQL en R de manera facil
library(DBI) # con DBI podemos obtener informacion sobre nuestra base de datos, asi como escribir tablas, etc
library(dplyr)
library(tidyverse)

### Conectando a la base de datos
## Credenciales
driver = MySQL()
host = "52.23.187.2"
port = 3306
user = "administrador"
password = "adminfo"
dbname = "Trabajo Final Adminfo"

## Conexion
if(dbCanConnect(drv=driver, port=port, user=user, host=host, password=password, dbname=dbname)) {
  conexion<-dbConnect(drv=driver, port=port, user=user, host=host, password=password, dbname=dbname) 
}
conexion
dbGetInfo(driver)
dbIsValid(conexion)

##################################################################################

### PROBANDO
## Creando tabla TEST
tablaUno <- c(1:20)
tablaDos <- c(21:40)
test <- data.frame(tablaUno,tablaDos)
head(test)
## Escribiendo tabla TEST a la base de datos
if(!dbExistsTable(conexion,"TEST")){
	dbWriteTable(conexion,name="TEST", value=test, append=TRUE)
}
## Probar la tabla TEST subida a la base de datos
test <- dbReadTable(conexion,"TEST")
head(test)
## Borrar la tabla TEST
dbRemoveTable(conexion,name="TEST")

##################################################################################

## Cargamos las tablas hechas en preprocesamiento
licenciamiento <- read.csv("Datasets/2-Preprocesados/licenciamiento.csv")
carnes <- read.csv("Datasets/2-Preprocesados/carnes.csv")
programas <- read.csv("Datasets/2-Preprocesados/programas.csv")
resumen_sunedu <- read.csv("Datasets/3-Generado/resumen_sunedu.csv")
vinos <- read.csv("Datasets/0-Descargados/winequality-red.csv")

## Subiendo las tablas creadas en preprocesamiento al servidor
if(!dbExistsTable(conexion,"licenciamiento")){
	dbWriteTable(conexion,name="licenciamiento", value=licenciamiento, append=TRUE)
}
if(!dbExistsTable(conexion,"carnes")){
	dbWriteTable(conexion,name="carnes", value=carnes, append=TRUE)
}
if(!dbExistsTable(conexion,"programas")){
	dbWriteTable(conexion,name="programas", value=programas, append=TRUE)
}
if(!dbExistsTable(conexion,"resumen_sunedu")){
	dbWriteTable(conexion,name="resumen_sunedu", value=resumen_sunedu, append=TRUE)
}
if(!dbExistsTable(conexion,"vinos")){
	dbWriteTable(conexion,name="vinos", value=vinos, append=TRUE)
}

## Probar si estan subidos en el servidor
licenciamientoTest <- dbReadTable(conexion,"licenciamiento")
carnesTest <- dbReadTable(conexion,"carnes")
programasTest <- dbReadTable(conexion,"programas")
resumen_suneduTest <- dbReadTable(conexion,"resumen_sunedu")
vinosTest <- dbReadTable(conexion,"vinos")

##Revisar si esta cargado correctamente
view(licenciamientoTest)
