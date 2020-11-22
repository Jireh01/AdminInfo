library("dplyr")

carnesExplo <- read_excel("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/carnesEx.xlsx")
licenciamientoExplo <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/licenciamiento.csv")
programasExplo <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/programas.csv", header=TRUE, sep=",")

head(licenciamiento)
head(programas)
head(carnesEx)

# Consultas
#- universidades que antes tenian licencia y ahora ya no
#- universidades que cuentan con 6 años de licenciamiento y pertenecen al departamento de lima
#- Cuantos estudiantes estan en universidades no licenciadas de ICA
#- Cuantos estudiantes de universidades privadas estan estudiando CC
#- Cuantas universidades que cuentan con postgrado se encuentran fuera de LIMA

q1 <- licenciamiento %>%       
