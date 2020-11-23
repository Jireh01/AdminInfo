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

# Todos lo programas que tiene la UPC 
q7 <- programas %>% select(CODIGO_ENTIDAD, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO, DENOMINACION_PROGRAMA) %>% filter(programas$NOMBRE == 'UNIVERSIDAD PERUANA DE CIENCIAS APLICADAS')
head(q7)


# Universidades que no tienen un departamento filial en su registro y cuentan con informatica como curso
q8 <- carnesEx %>% select(CODIGO, NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>% filter(carnesEx$NOMBRE_CLASE_PROGRAMA == 'INFORMATICA', carnesEx$NOMBRE_FILIAL == '[NO ESPECIFICADO]')
head(q8)

# Cursos de universidades de Junin que cuentan con Maestria 
q9 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, TIPO_GESTION, DENOMINACION_PROGRAMA) %>% filter(programas$DEPARTAMENTO_FILIAL == 'JUNIN', programas$NIVEL_ACADEMICO == 'MAESTRIA', programas$TIPO_GESTION == 'PRIVADO')
head(q9)

#- Cuantas universidades que cuentan con postgrado se encuentran fuera de LIMA
q10 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE) %>% filter(programas$DEPARTAMENTO_LOCAL != 'LIMA', programas$TIPO_NIVEL_ACADEMICO == 'POSGRADO')
head(q10)
head(count(q10))

# Cursos que tienen menos del promedio de carnes (estudiantes) y que son de la carrera de derecho
q11 <- carnesEx %>% select(CODIGO, NOMBRE_UNIVERSIDAD, Cant_Carnes) %>% filter(carnesEx$Cant_Carnes < mean(carnesEx$Cant_Carnes),carnesEx$NOMBRE_PROGRAMA == 'DERECHO')
head(q11)

#- universidades que cuentan con 6 años de licenciamiento y pertenecen al departamento de lima
q12 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL) %>% filter(licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', licenciamiento$PERIODO_LICENCIAMIENTO == 6, licenciamiento$DEPARTAMENTO_LOCAL == 'LIMA') %>% group_by(NOMBRE)
head(q12)



######### Graficos ##########

# grafico de pie de licenciamiento de universidades
g1 <- pie(table(licenciamiento$ESTADO_LICENCIAMIENTO), main = "Licenciamiento de Universidades")

# Cuantas universidades por departamento hay
g2 <- ggplot(licenciamiento, aes(licenciamiento$NOMBRE, licenciamiento$DEPARTAMENTO_LOCAL)) + geom_jitter() + labs(y = "Departamentos", x = "Universidades")
g2

# Porcentaje de periodos de licenciamiento
g3 <- pie(table(licenciamiento$PERIODO_LICENCIAMIENTO), main = "Periodo de licenciamiento de Universidades")

# Cantidad de carnes por univeridad (no se ve tan bien)
aux <- carnesEx %>% group_by(NOMBRE_UNIVERSIDAD) %>% summarise(suma_canres = sum(Cant_Carnes))
g4 <- ggplot(aux, aes(y=NOMBRE_UNIVERSIDAD, x=suma_canres, fill=NOMBRE_UNIVERSIDAD)) + theme_minimal()+geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + labs(y='Universidades',x='Carnes', title='Numero de carnes por Universidad')
g4

# Cantidad de carnes por Carreras (se ve muy mal)
aux2 <- carnesEx %>% group_by(NOMBRE_PROGRAMA) %>% summarise(suma_canres = sum(Cant_Carnes))
g5 <- ggplot(aux2, aes(y=NOMBRE_PROGRAMA, x=suma_canres, fill=NOMBRE_PROGRAMA)) + theme_minimal()+geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + labs(y='Universidades',x='Carnes', title='Numero de carnes por Universidad')
g5

# periodo de licenciamiento con algo mas ????
aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
g6 <- ggplot(aux3, aes(y=TIPO_GESTION,x=suma_lic, fill=TIPO_GESTION)) + geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', title='Comparativa entre numero de licenciamientos entre Público y Privado')
g6

