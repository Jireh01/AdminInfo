library(ggplot2)
#install.packages("plotly")
library(plotly)
library(dplyr)

# Cargamos los data.frames
licenciamiento <- read.csv("Datasets/2-Preprocesados/licenciamiento.csv")
carnes <- read.csv("Datasets/2-Preprocesados/carnes.csv")
programas <- read.csv("Datasets/2-Preprocesados/programas.csv")
resumen_sunedu <- read.csv("Datasets/3-Generado/resumen_sunedu.csv")


# 1- Tipo de departamento filial segun universidades publicas con carrera en Ingenieria Mecánica
q3 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, NOMBRE_FILIAL, NIVEL_ACADEMICO) %>% 
            filter(programas$TIPO_GESTION == 'PUBLICO', 
            programas$DENOMINACION_PROGRAMA == 'INGENIERIA MECANICA')
grafico <- ggplot(q3, aes(y=NOMBRE, x=NOMBRE_FILIAL)) + 
            theme_minimal() + geom_point(color="red",size=7) + 
            labs(y="Universidades", x="Departamento Filial", 
            title="A que departamento Filial pertenecen las Universidades publicas con Mecánica")
grafico
# Interpretacion: Muestra que filia esta asociada a las universidades publicas con la carrea Mecatronica

# 2- grafico de pie de licenciamiento de universidades
grafico2 <- ggplot(licenciamiento, aes(x='', y=ESTADO_LICENCIAMIENTO, fill=ESTADO_LICENCIAMIENTO)) + 
                        geom_bar(stat='identity', color='white') + coord_polar(theta='y') + labs(title='Pie de Licenciamiento')
grafico2
# Interpretacion: Muestra un pie con los percentajes de las licencias otorgadas (o denegadas) a las Universidades segun la SUNEDU
# Se puede ver como la mayoria de universidades fueron licenciadas, con mas del 55% son solo licenciaturas otorgadas.


# 3- Cuantas universidades por departamento hay
grafico3 <- ggplot(licenciamiento, aes(x=licenciamiento$NOMBRE, y=licenciamiento$DEPARTAMENTO_LOCAL, fill=DEPARTAMENTO_LOCAL)) + 
                geom_bar(stat='identity', color='white') + labs(y = "Departamentos", x = "Universidades",
                title='Universidades por departamento')
grafico3
# Interpretacion: Cuantas universidades hay por departamento con barras. Se puede ver como LIMA es la que cuenta con mas universidades.
# Sin embargo es complicado ver las demas, por lo que se creo el grafico de pie.

# 4- Cuantas universidades por departamento hay en porcentaje (pie)
grafico4 <- ggplot(licenciamiento, aes(x=DEPARTAMENTO_LOCAL, y='', fill=DEPARTAMENTO_LOCAL)) + 
                geom_bar(stat='identity', color='white') + coord_polar(theta='x') + 
                 labs(y='',x='Departamentos', 
                        title='Universidades según departamento')
grafico4
# Interpretacion: Se puede ver como en segundo lugar esta La Libertad y Junin empatados, y en tercer lugar esta Arequipa

# 5- Porcentaje de periodos de licenciamiento
y <- licenciamiento %>% group_by(PERIODO_LICENCIAMIENTO, ESTADO_LICENCIAMIENTO) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico5 <- ggplot(y, aes(y='',x=suma_lic, fill=PERIODO_LICENCIAMIENTO)) + 
                geom_bar(stat="identity", color='white') + coord_polar(theta='x') + 
                        labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                        title='Periodo de licenciamiento de Universidades')
grafico5
# Interpretacion: Porcentaje de periodos de licenciamiento contando el 0, el que mas porcentaje tiene es 6 años, luego sigue 8 
# y por ultimo 10

# 6- Cantidad de carnes por univeridad 
au <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_canres = sum(CANTIDAD_CARNES))
grafico6 <- ggplot(au, aes(y=NOMBRE, x=suma_canres, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.5,show.legend = FALSE) + 
                        labs(y='Universidades',x='Carnes', 
                        title='Numero de carnes por Universidad')
grafico6
# Interpretacion: Muestra la cantidad de carnes para cada universidad (todas licenciada)


# 7- Cantidad de programas por Universidad 
aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
grafico7 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                    labs(y='Universidades',x='Programas', title='Numero de Programas por Universidad')
grafico7
# Interpretacion: Muestra la cantidad de programas/calses para cada universidad (todas licenciada). Se puede ver como La Cayetano es la Universidad
# que mas programas tiene, y la que menos tiene es la Unviersidad Jaime Bausate y Meza

 
# 8- Periodo de licenciamiento segun tipo de gestion
aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico8 <- ggplot(aux3, aes(y='',x=suma_lic, fill=TIPO_GESTION)) + 
                geom_bar(stat="identity", color='white') + coord_polar(theta='x')
                        labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                        title='Comparativa entre numero de licenciamientos entre Público y Privado')
grafico8
# Interpretacion: Comparativa entre las licenciaciones publicas vs las privadas. Mas licenciadas hay publicas que privadas, auque la diferencia
# es minima


# 9- Periodo de licenciamiento segun cada departamento del Perú
aux4 <- licenciamiento %>% group_by(DEPARTAMENTO_LOCAL) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico9 <- ggplot(aux4, aes(y=DEPARTAMENTO_LOCAL, x=suma_lic, fill=DEPARTAMENTO_LOCAL)) + 
                    theme_minimal() + geom_bar(stat="identity",width = 0.4) + 
                    labs(y='Departamentos', x='Periodo de licenciamiento', 
                    title='Periodo de Licenciamiento según Departamento')
grafico9
# Interpretacion: La maxima cantidad de periodos por deparamento. Se suman todos los periodos (estan en años). Se puede ver como LIMA sobre pasa
# al resto de departamentos por mucho, teniendo mas de 250 años acumulados en licenciamientos. 


# 10- Periodo de licenciamiento segun cada universidad
aux5 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO)) 
grafico10 <- ggplot(aux5, aes(y=NOMBRE, x=suma_lic, fill=NOMBRE)) + theme_minimal() + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Periodo de licenciamiento', title='Periodo de Licenciamiento según Departamento')
grafico10
# Interpretacion: Muestra cual es la licenciatura de las universidades (solo se concideran las que la tienen otorgada). Se puede ver como 
# La PUCP, UNI, San Marcos y UPC, son las 4 universidades que cuentan con licencias de 10 años


# 11- Universidades que tienen mas de 7 años de licencia activa 
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO) %>% 
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', PERIODO_LICENCIAMIENTO >= 7)
grafico11 <- ggplot(query, aes(y=NOMBRE, x=PERIODO_LICENCIAMIENTO, fill=NOMBRE)) + theme_minimal() + 
                geom_bar(stat='identity',width = 0.2,,show.legend = FALSE) + 
                labs(y='Universidad', x='Periodo Licenciamiento', title='Universidades que cuentan con mas de 7 años de Licenciamiento')
grafico11
# Interpretacion: Muestra si las universidades tienen 8 o 10 meses. Se puede ver que mas hay con 8 años de licenciamiento que 10 (aunque no por mucho).
# Además podemos comparar con el grafico 9, se puede ver la drastica reducción de univerisdades que tienen mas de 7 años.


# 12- Universidades privadas que tienen mas de 10000 estudiantes y cuantos estudiantes tienen por universidad
query2 <- carnes %>%
				select(NOMBRE_UNIVERSIDAD, TIPO_GESTION, Cant_Carnes) %>%
				filter(TIPO_GESTION == 'PRIVADO') %>%
				group_by(NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>%
				summarize(CANTIDAD_CARNES = sum(Cant_Carnes)) %>%
				filter(CANTIDAD_CARNES > 10000)
grafico12 <- ggplot(query2, aes(y=NOMBRE_UNIVERSIDAD, x=CANTIDAD_CARNES, fill=NOMBRE_UNIVERSIDAD)) + theme_minimal() + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Numero de estudiantes', 
                            title='Numero de estudiantes por universidades con mas de 1000 estudiantes')
grafico12
# Interpretacion: Muestra la cantidad de alumnos por universidad. Se puede ver como La vallejo gana por goleada a comparacion de cualquier otra
# Seguido de la Universidad Alas Peruanas. Las dos universidades con menor numero de estudiantes son: Universidad Catolica Sedes Sapientiae y 
# la Telesup


# 13- Estudiantes en total de cada departamento del Peru que supera el promedio de estudiantes por departamento
query <- carnes %>%
                group_by(DEPARTAMENTO_FILIAL) %>%
                summarize(CANTIDAD = sum(Cant_Carnes)) %>%
                filter(CANTIDAD > mean(CANTIDAD))
grafico13 <- ggplot(query, aes(x=DEPARTAMENTO_FILIAL, y=CANTIDAD, fill=DEPARTAMENTO_FILIAL)) + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Estudiantes', x='Departamento', 
                        title='Departamentos que superan el numero de estudiantes promedio')
grafico13
# Interpretacion: Muestra los estudiantes por cada departamento que suepera el promedio por departamento. Sin sorpresa, Lima gana por lejos 
# a comparacion de los demas. Luego le sigue Arequipa, La Libertad y por ultimo Junin, aunque los 3 tienen cantidad cercanas.


# 14- Universidades que tienen mas del promedio de programas totales
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>%
						filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL))
grafico14 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Programas', 
                        title='Universidades con mas del promedio de programas')
grafico14
# Interpretacion: Universidades que tienen mas del promedio de programas, se puede ver que la cayetano gana por casi 700.


# 15- Universidades que tienen menos del promedio de carnes
query <- resumen_sunedu %>%
						select(NOMBRE, CANTIDAD_CARNES) %>% 
                                                filter(CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
grafico15 <- ggplot(query, aes(y=NOMBRE, x=CANTIDAD_CARNES, fill=NOMBRE))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Carnes', 
                        title='Universidades que no superan el promedio de carnes')
grafico15
# Interpretacion: Univesidades que tienen menos del promedio de carnes, puede ver como la cayetano supera a los demas por también
# Por 700 carnes, El promedio es 10000 aproximadamente

# 16- Histograma de periodo de frecuencia de licenciamiento
grafico16 <- ggplot(licenciamiento, aes(PERIODO_LICENCIAMIENTO)) + 
                        geom_histogram(binwidth=1, color="black", fill='pink') + labs(x='Carnes totales',
                        title='Histograma de periodo de frecuencia de licenciamiento')
grafico16
# Interpretacion: La frecuencia mas alta es de 6 años para los licenciamientos, luego sigue 0, 8 y pore ultimo 10.
# Se puede ver como es mas complicado obtener licencias de mas de 6 años, siendo 10 la maxima posible y con el menor numero de universidades

# 17- Universidades que tienen menos del promedio de programas totales y que cuentan con licencia 
query <- resumen_sunedu %>% select(NOMBRE,PROGRAMAS_TOTAL) %>% filter(PROGRAMAS_TOTAL < mean(PROGRAMAS_TOTAL))
grafico17 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE)) + 
                geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                labs(y='Universidades', x='Programas', 
                    title='Universidades que tienen menos del promedio de programas')
grafico17
# Interpretacion: Hay 4 universidades con el numero minimo de programas totales. Ademas La universidad con mayor programas en esta categoria
# es la Universidad Privada de Tacna con 90 programas aproximadamente

# 18- Hiatograma de frecuencia de carnes universitarios
grafico18 <- ggplot(resumen_sunedu, aes(CANTIDAD_CARNES)) + 
                        geom_histogram(binwidth=10000, color="black", fill='lightgreen') + labs(x='Carnes totales',
                        title='Hiatograma de frecuencia de carnes universitarios')
grafico18
# Interpretacion: Se puede ver que la mayor frecuencia esta entre 0 y los 1000 carnes de universidades licenciadas

# 19- Diagrama de cajas para los programas totales y la cantidad de carnes para las universidades licenciadas
# Sin outliers
boxplot(resumen_sunedu$CANTIDAD_CARNES, 
        outline = FALSE, col='green', horizontal = TRUE, ylab="Cantidad de Carnes")

# 20- Diagrama de cajas para los programas totales y la cantidad de carnes para las universidades licenciadas
# Sin outliers
boxplot(resumen_sunedu$PROGRAMAS_TOTAL, 
        outline = FALSE, col='yellow', horizontal = TRUE, ylab="Cantidad de Programas")
# Interpretacion: Programas y carnes Con outliers con diagrama de bloque

# 21- Diagrama de cajas para la cantidad de carnes para las universidades licenciadas
# Con outliers
boxplot(resumen_sunedu$CANTIDAD_CARNES,
         col='blue', horizontal = TRUE, outliers = TRUE, ylab='Cantidad de Carnes')

# 22- Diagrama de cajas para los programas totales para las universidades licenciadas 
# Con outliers
boxplot(resumen_sunedu$PROGRAMAS_TOTAL, 
        col='red', horizontal = TRUE, outliers = TRUE, ylab='Cantidad de Programas')
# Interpretacion: Programas y carnes sin outliers  con diagrama de bloque
# Se puede ver como la cantidad de carnes es la que cuenta con mas outliers y con cantidad de 6 outliers

# 23- Histograma de frecuencua de programas de universidades
grafico23 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL)) + 
                        geom_histogram(binwidth=14, color='black', fill='red') + labs(x='Programas Totales', 
                        title='Histograma de frecuencua de programas de universidades')
grafico23
# Interpretacion: La mayor frecuencia de programas se encuentra entre el rango 0 y 100. 
# Además entre 440 y 500 hay un hueco donde hay 0 programas

# 24- Diagrama de dispercion entre los programas y la cantidad de carnes
grafico24 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES, colour=CANTIDAD_CARNES)) + geom_point(color="blue") + 
                    labs(y='Carnes', x='Programas', 
                    title="Diagrama de Disperción Entre Programas y Carnes")
grafico24
# Interpretacion: Grafico de dispercion entre los programas totales y los carnes. Se puede ver como el grafico va en forma de cono para afuera
# Este modelo hasta podria servir para ciertos modelados que se han realizado en clase


# 25- Grafico programas vs carnes (programas <= 200 y carnes <= 20000)
s <- resumen_sunedu %>% select(PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>% 
                        filter(PROGRAMAS_TOTAL <= 200, CANTIDAD_CARNES <= 20000)
grafico25 <- ggplot(s, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES, colour=CANTIDAD_CARNES)) + geom_point(color="black", show.legend=TRUE) + 
                    stat_smooth(method='lm', aes(colour='linear')) + stat_smooth(method='lm', formula = y ~ log(x)) + 
                    labs(y='Carnes', x='Programas', 
                    title="Diagrama de Disperción Entre Programas (<=200) y Carnes(<=20000)")
grafico25
# Interpretacion: Cuando los programas son 200 o menos y los carnes 20000 o menos, se puede llegar a sacar una regresion lineal 
# algo acertada, como se puede ver en la gráfica. Mientras que la logarítmica es menos acertada, salvo en los primeros puntos

# 26- Universidades con mayor promedio de carnes 
query <- resumen_sunedu %>%
						select(NOMBRE, CANTIDAD_CARNES) %>% 
                                                filter(CANTIDAD_CARNES > mean(CANTIDAD_CARNES))
grafico26 <- ggplot(query, aes(y=NOMBRE, x=CANTIDAD_CARNES, fill=NOMBRE))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Carnes', 
                        title='Universidades que sí superan el promedio de carnes')
grafico26
# Interpretacion: Estas son las univeridades que cuentan con mas del promedio de carnes ademas de estar licenciadas. Se puede ver
# como la UPC es la que mas tiene con mas de 70000 carnes, luego le sigue la UTP y por ultimo la PUCP.

# 27- Universidades con menos de 100 programas en total
aux2 <- resumen_sunedu %>% select(NOMBRE, PROGRAMAS_TOTAL) %>% filter(PROGRAMAS_TOTAL <= 100)
grafico7 <- ggplot(aux2, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                    labs(y='Universidades',x='Programas', title='Universidades con menos de 100 programas en total')
grafico7
# Interpretacion: Se realizó este grafico para poder ver mas a detalle a las universidades con menos de 100 programas ya que en el original
# no se alcanza a ver bien. Se puede ver como incluso hay universidades licenciadas que tienen menos de 10 programas, como Universidad Jaime
# Bausate y Meza.



#############################################################
######################## PARA VIDEO #########################
#############################################################

# 23- Histograma de frecuencua de programas de universidades
grafico23 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL)) + 
                        geom_histogram(binwidth=14, color='black', fill='red') + labs(x='Programas Totales', 
                        title='Histograma de frecuencua de programas de universidades')
grafico23
# Interpretacion: La mayor frecuencia de programas se encuentra entre el rango 0 y 100. 
#Además entre 440 y 500 hay un hueco donde hay 0 programas

# 2- grafico de pie de licenciamiento de universidades
grafico2 <- ggplot(licenciamiento, aes(x='', y=ESTADO_LICENCIAMIENTO, fill=ESTADO_LICENCIAMIENTO)) + 
                        geom_bar(stat='identity', color='white') + coord_polar(theta='y')
grafico2
# Interpretacion: Muestra un pie con los percentajes de las licencias otorgadas (o denegadas) a las Universidades segun la SUNEDU

grafico4 <- ggplot(licenciamiento, aes(x=DEPARTAMENTO_LOCAL, y='', fill=DEPARTAMENTO_LOCAL)) + 
                geom_bar(stat='identity', color='white') + coord_polar(theta='x') + 
                 labs(y='',x='Departamentos', 
                        title='Universidades según departamento')
grafico4

# 7- Cantidad de programas por Universidad 
aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
grafico7 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                    labs(y='Universidades',x='Programas', title='Numero de Programas por Universidad')
grafico7
# Interpretacion: Muestra la cantidad de programas/calses para cada universidad (todas licenciada). Se puede ver como La Cayetano es la Universidad
# que mas programas tiene, y la que menos tiene es la Unviersidad Jaime Bausate y Meza

# 8- Periodo de licenciamiento segun tipo de gestion
aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico8 <- ggplot(aux3, aes(y='',x=suma_lic, fill=TIPO_GESTION)) + 
                geom_bar(stat="identity", color='white') + coord_polar(theta='x')
                        labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                        title='Comparativa entre numero de licenciamientos entre Público y Privado')
grafico8
# Interpretacion: Comparativa entre las licenciaciones publicas vs las privadas. Mas licenciadas hay publicas que privadas, auque la diferencia
# es minima

# 12- Universidades privadas que tienen mas de 10000 estudiantes
query2 <- carnes %>%
				select(NOMBRE_UNIVERSIDAD, TIPO_GESTION, Cant_Carnes) %>%
				filter(TIPO_GESTION == 'PRIVADO') %>%
				group_by(NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>%
				summarize(CANTIDAD_CARNES = sum(Cant_Carnes)) %>%
				filter(CANTIDAD_CARNES > 10000)
grafico12 <- ggplot(query2, aes(y=NOMBRE_UNIVERSIDAD, x=CANTIDAD_CARNES, fill=NOMBRE_UNIVERSIDAD)) + theme_minimal() + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Numero de estudiantes', 
                            title='Numero de estudiantes por universidades con mas de 1000 estudiantes')
grafico12
# Interpretacion: Muestra la cantidad de alumnos por universidad. Se puede ver como La vallejo gana por goleada a comparacion de cualquier otra
# Seguido de la Universidad Alas Peruanas. Las dos universidades con menor numero de estudiantes son: Universidad Catolica Sedes Sapientiae y 
# la Telesup