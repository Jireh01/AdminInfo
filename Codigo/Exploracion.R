library("dplyr")
library("ggplot2")

# carnesExplo <- read_excel("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/carnesEx.xlsx")
# licenciamientoExplo <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/licenciamiento.csv")
# programasExplo <- read.table("C:/Users/Francesco/Documents/UPC/Ciclo 5/Admin de la informacion/Trabajo Final/Trabajo-Final-Adminfo/Otros/programas.csv", header=TRUE, sep=",")

head(licenciamiento)
head(programas)
head(carnesEx)

# Consultas
#- universidades que antes tenian licencia y ahora ya no
#- universidades que cuentan con 6 años de licenciamiento y pertenecen al departamento de lima
#- Cuantos estudiantes estan en universidades no licenciadas de ICA
#- Cuantos estudiantes de universidades privadas estan estudiando CC
#- Cuantas universidades que cuentan con postgrado se encuentran fuera de LIMA

# Universidades que tienen la licencia No otorgada segun la SUNEDU 
q1 <- licenciamiento %>% select(NOMBRE, TIPO_GESTION, DEPARTAMENTO_LOCAL) %>% filter(licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA DENEGADA')      
head(q1)

# esto deberia estar en preproce pero no funcionaba cuando cargaba denuevo
licenciamiento$PERIODO_LICENCIAMIENTO <- as.integer(licenciamiento$PERIODO_LICENCIAMIENTO)
# Universidades que tienen la licencia segun la SUNEDU y le quedan mas de 7 años de licencia
q2 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL) %>% filter(licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', licenciamiento$PERIODO_LICENCIAMIENTO >= 7)
head(q2)

# Universidades publicas que cuentan con el curso Ingenieria Mecanica 
q3 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, NOMBRE_FILIAL, NIVEL_ACADEMICO) %>% filter(programas$TIPO_GESTION == 'PUBLICO', programas$DENOMINACION_PROGRAMA == 'INGENIERIA MECANICA')
head(q3)

# Universidades que cuentan con doctorado en el sector publico 
q4 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, NOMBRE_FILIAL, DENOMINACION_PROGRAMA) %>% filter(programas$TIPO_GESTION == 'PUBLICO', programas$NIVEL_ACADEMICO == 'DOCTORADO')
head(q4)

# Universidades de ICA que no cuentan con licencia por SUNEDU
q5 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL, TIPO_GESTION) %>% filter(licenciamiento$DEPARTAMENTO == 'ICA', licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA DENEGADA')
head(q5)

# Universidades privadas que tienen mas de 10 carnets 
carnesEx$Cant_Carnes <- as.integer(carnesEx$Cant_Carnes)
q6 <- carnesEx %>% select(CODIGO, NOMBRE_UNIVERSIDAD, DEPARTAMENTO_FILIAL) %>% filter(carnesEx$TIPO_GESTION == 'PRIVADO', carnesEx$Cant_Carnes > 10)
head(q6)


# Graficos

# grafico de pie de licenciamiento de universidades
g1 <- pie(table(licenciamiento$ESTADO_LICENCIAMIENTO), main = "Licenciamiento de Universidades")

# Cuantas universidades por departamento hay
g2 <- ggplot(licenciamiento, aes(licenciamiento$NOMBRE, licenciamiento$DEPARTAMENTO_LOCAL)) + geom_jitter() + labs(y = "Departamentos", x = "Universidades")
g2

# 
g3 <- ggplot()