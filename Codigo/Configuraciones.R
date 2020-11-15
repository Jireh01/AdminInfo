# Instalacion de paquetes
install.packages("RMySQL")
install.packages("DBI")

# Activando las librerias

library(RMySQL) # nos permite usar MySQL en R de manera facil
library(DBI) # con DBI podemos obtener informacion sobre nuestra base de datos, asi como escribir tablas, etc
library(dplyr)

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

### PROBANDO
## Creando tabla TEST
tablaUno <- c(1:20)
tablaDos <- c(21:40)
test <- data.frame(tablaUno,tablaDos)
head(test)

## Escribiendo tabla TEST a la base de datos
if(!dbExistsTable(conexion,"TEST"))
    dbWriteTable(conexion,name="TEST", value=test, append=TRUE)

## Probar la tabla TEST subida a la base de datos
test <- dbReadTable(conexion,"TEST")
head(test)

## Borrar la tabla TEST
dbRemoveTable(conexion,name="TEST")