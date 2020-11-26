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
            theme_minimal() + geom_point(color="red",size=3) + 
            labs(y="Departamentos", x="Universidades", title="")
grafico
# Interpretacion: Muestra que filia esta asociada a las universidades publicas con la carrea Mecatronica

# 2- grafico de pie de licenciamiento de universidades
grafico2 <- pie(table(licenciamiento$ESTADO_LICENCIAMIENTO), 
                    main = "Licenciamiento de Universidades")
# Interpretacion: Muestra un pie con los percentajes de las licencias otorgadas (o denegadas) a las Universidades segun la SUNEDU


# 3- Cuantas universidades por departamento hay
grafico3 <- ggplot(licenciamiento, aes(x=licenciamiento$NOMBRE, y=licenciamiento$DEPARTAMENTO_LOCAL, fill=DEPARTAMENTO_LOCAL)) + 
                geom_bar(stat='identity', color='white') + labs(y = "Departamentos", x = "Universidades")
grafico3
# Interpretacion: Cuantas universidades hay por departamento con puntos 

# 3.5- Cuantas universidades por departamento hay en porcentaje (pie)
grafico3.5 <- ggplot(licenciamiento, aes(x='', y=DEPARTAMENTO_LOCAL, fill=DEPARTAMENTO_LOCAL)) + 
                geom_bar(stat='identity', color='white') + coord_polar(theta='y') + 
                 labs(y='Departamentos',x='', 
                        title='Universidades Segun departamento')
grafico3.5

# 4- Porcentaje de periodos de licenciamiento
y <- licenciamiento %>% group_by(PERIODO_LICENCIAMIENTO) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico4 <- ggplot(y, aes(y='',x=suma_lic, fill=PERIODO_LICENCIAMIENTO)) + 
                geom_bar(stat="identity", color='white') + coord_polar(theta='x')
                        labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                        title='Periodo de licenciamiento de Universidades')
grafico4
# Interpretacion: Porcentaje de periodos de licenciamiento contando el 0, el que mas porcentaje tiene es 6 años

# 5- Cantidad de carnes por univeridad 
aux <- resumen_sunedu %>% select(NOMBRE) %>% filter(CANTIDAD_CARNES > 10000) %>% 
                            summarise(suma_canres = sum(CANTIDAD_CARNES))
grafico5 <- ggplot(aux, aes(y=NOMBRE, x=suma_canres, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.5,show.legend = FALSE) + 
                        labs(y='Universidades',x='Carnes', 
                        title='Numero de carnes por Universidad')
grafico5
# Interpretacion: Muestra la cantidad de carnes para cada universidad (todas licenciada)


# 6- Cantidad de programas por Universidad 
aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
grafico6 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                    labs(y='Universidades',x='Programas', title='Numero de Programas por Universidad')
grafico6
# Interpretacion: Muestra la cantidad de programas/calses para cada universidad (todas licenciada)

 
# 7- Periodo de licenciamiento segun tipo de gestion
aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico7 <- ggplot(aux3, aes(y='',x=suma_lic, fill=TIPO_GESTION)) + 
                geom_bar(stat="identity", color='white') + coord_polar(theta='x')
                        labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                        title='Comparativa entre numero de licenciamientos entre Público y Privado')
grafico7
# Interpretacion: Comparativa entre las licenciaciones publicas vs las privadas. Mas licenciadas hay publicas que prvadas


# 8- Periodo de licenciamiento segun cada departamento del Perú
aux4 <- licenciamiento %>% group_by(DEPARTAMENTO_LOCAL) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico8 <- ggplot(aux4, aes(y=DEPARTAMENTO_LOCAL, x=suma_lic, fill=DEPARTAMENTO_LOCAL)) + 
                    theme_minimal() + geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Departamentos', x='Periodo de licenciamiento', 
                    title='Periodo de Licenciamiento según Departamento')
grafico8
# Interpretacion: La maxima cantidad de periodos por deparamento. Se suman todos los periodos (estan en años)


# 9- Periodo de licenciamiento segun cada universidad
aux5 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO)) 
grafico9 <- ggplot(aux5, aes(y=NOMBRE, x=suma_lic, fill=NOMBRE)) + theme_minimal() + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Periodo de licenciamiento', title='Periodo de Licenciamiento según Departamento')
grafico9
# Interpretacion: Muestra cual es la licenciatura de las universidades (solo se concideran las que la tienen otorgada)


# 10- Universidades que tienen mas de 7 años de licencia activa 
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO) %>% 
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', PERIODO_LICENCIAMIENTO >= 7)
grafico10 <- ggplot(query, aes(y=NOMBRE, x=PERIODO_LICENCIAMIENTO)) + theme_minimal() + 
                    geom_boxplot()
grafico10
# Interpretacion: Muestra si las universidades tienen 8 o 10 meses. Se puede ver que mas hay con 8 años de licenciamiento que 10


# 11- Universidades privadas que tienen mas de 10000 estudiantes
query2 <- carnes %>%
				select(NOMBRE_UNIVERSIDAD, TIPO_GESTION, Cant_Carnes) %>%
				filter(TIPO_GESTION == 'PRIVADO') %>%
				group_by(NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>%
				summarize(CANTIDAD_CARNES = sum(Cant_Carnes)) %>%
				filter(CANTIDAD_CARNES > 10000)
grafico11 <- ggplot(query2, aes(y=NOMBRE_UNIVERSIDAD, x=CANTIDAD_CARNES, fill=CANTIDAD_CARNES)) + theme_minimal() + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Numero de estudiantes', 
                            title='Numero de estudiantes por universidades con mas de 1000')
grafico11
# Interpretacion: Muestra la cantidad de alumnos por universidad. Se puede ver como La vallejo gana por goleada a comparacion de cualquier otra


# 12- Estudiantes en total de cada departamento del Peru que supera el promedio de estudiantes por departamento
query <- carnes %>%
                group_by(DEPARTAMENTO_FILIAL) %>%
                summarize(CANTIDAD = sum(Cant_Carnes)) %>%
                filter(CANTIDAD > mean(CANTIDAD))
grafico12 <- ggplot(query, aes(x=DEPARTAMENTO_FILIAL, y=CANTIDAD, fill=CANTIDAD)) + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Estudiantes', x='Departamento', 
                        title='Numero de estudiantes que superan el promedio en su departamento')
grafico12
# Interpretacion: Muestra los estudiantes por cada departamento que suepera el promedio por departamento. Sin sorpresa, Lima gana por lejos a comparacion de los demas


# 13- Universidades que tienen mas del promedio de programas totales
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>%
						filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL), CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
grafico13 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=PROGRAMAS_TOTAL))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Programas', 
                        title='Universidades con mas del promedio de carnes y programas')
grafico13
# Interpretacion: Universidades que tienen mas del promedio de programas y carnes, se puede ver que la cayetano gana por casi 500.


# 14- Universidades que tienen menos del promedio de carnes
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>% filter(CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
grafico14 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Carnes', 
                        title='Numero de estudiantes que superan el promedio en su departamento')
grafico14
# Interpretacion: Univesidades que tienen menos del promedio de carnes, puede ver como la cayetano supera a los demas tambien.


# 15- Histograma de periodo de frecuencia de licenciamiento
hist(resumen_sunedu$PERIODO_LICENCIAMIENTO, 
                main='Histograma de periodo de frecuencia de licenciamiento')
# Interpretacion: La frecuencia mas alta es de 6 años para los licenciamientos

# 16- Universidades que tienen mas del promedio de programas totales y que cuentan con licencia 
query <- resumen_sunedu %>% select(NOMBRE,PROGRAMAS_TOTAL) %>% filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL))
grafico16 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE)) + 
                geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                labs(y='Universidades', x='Programas', 
                    title='Universidades que tienen mas del promedio de programas')
grafico16
# Interpretacion: Otra vez, gana Cayetano con mas del promedio de programas y que cuentan con licencia

# 17- Hiatograma de frecuencia de carnes universitarios
hist(resumen_sunedu$CANTIDAD_CARNES, main='Hiatograma de frecuencia de carnes universitarios')
# Interpretacion: Se puede ver que la mayor frecuencia esta entre 0 y los 1000 carnes de universidades licenciadas


# 18- Diagrama de cajas para los programas totales y la cantidad de carnes para las universidades licenciadas
# Sin outliers
boxplot(resumen_sunedu$PROGRAMAS_TOTAL, 
        resumen_sunedu$CANTIDAD_CARNES, outline = FALSE)
# Interpretacion: Programas y carnes sin outliers  con diagrama de bloque

# 19- Diagrama de cajas para los programas totales y la cantidad de carnes para las universidades licenciadas
# Con outliers
boxplot(resumen_sunedu$PROGRAMAS_TOTAL, 
        resumen_sunedu$CANTIDAD_CARNES)
# Interpretacion: Programas y carnes Con outliers con diagrama de bloque
# Se puede ver como la cantidad de carnes es la que cuenta con mas outliers y con cantidad

# 20- Diagrama de dispercion entre los programas y la cantidad de carnes
grafico20 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES, colour=CANTIDAD_CARNES)) + geom_point(color="blue") + 
                    labs(y='Carnes', x='Programas', 
                    title="Diagrama de Disperción Entre Programas y Carnes")
grafico20
# Interpretacion: Grafico de dispercion entre los programas totales y los carnes. Se puede ver como el grafico va en forma de cono para afuera
# Este modelo hasta podria servir para ciertos modelados que se han realizado en clase


# 21- Histograma de frecuencua de programas de universidades
grafico20 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL)) + 
                        geom_histogram(binwidth=10) + labs(x='Programas Totales', 
                        title='Histograma de frecuencua de programas de universidades')
grafico20
# Interpretacion: La mayor frecuencia de programas se encuentra entre el rango 0 y 100. 
#Además entre 440 y 500 hay un hueco donde hay 0 programas

# 22- Grafico programas vs carnes (programas )
s <- resumen_sunedu %>% select(PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>% 
                        filter(PROGRAMAS_TOTAL <= 200, CANTIDAD_CARNES <= 20000)
grafico21 <- ggplot(s, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES, colour=CANTIDAD_CARNES)) + geom_point(color="blue") + 
                    stat_smooth(method='lm', aes(colour='linear')) + stat_smooth(method='lm', formula = y ~ log(x)) + 
                    labs(y='Carnes', x='Programas', 
                    title="Diagrama de Disperción Entre Programas (<=200) y Carnes(<=20000)")
grafico21
# Interpretacion: Cuando los programas son 200 o menos y los carnes 20000 o menos, se puede llegar a sacar una regresion lineal 
# algo acertada, como se puede ver en la gráfica. Mientras que la logarítmica es menos acertada, salvo en los primeros puntos