
# 1- Tipo de departamento filial segun universidades publicas con carrera en Ingenieria Mecánica
q3 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, NOMBRE_FILIAL, NIVEL_ACADEMICO) %>% 
            filter(programas$TIPO_GESTION == 'PUBLICO', 
            programas$DENOMINACION_PROGRAMA == 'INGENIERIA MECANICA')
grafico <- ggplot(q3, aes(y=NOMBRE, x=NOMBRE_FILIAL)) + 
            theme_minimal() + geom_point(color="red",size=3) + 
            labs(y="Departamentos", x="Universidades", title="")
grafico

# 2- grafico de pie de licenciamiento de universidades
grafico2 <- pie(table(licenciamiento$ESTADO_LICENCIAMIENTO), 
                    main = "Licenciamiento de Universidades")

# 3- Cuantas universidades por departamento hay
grafico3 <- ggplot(licenciamiento, aes(licenciamiento$NOMBRE, licenciamiento$DEPARTAMENTO_LOCAL)) + 
                geom_point() + labs(y = "Departamentos", x = "Universidades")
grafico3

# 4- Porcentaje de periodos de licenciamiento
grafico4 <- pie(table(licenciamiento$PERIODO_LICENCIAMIENTO), 
                main = "Periodo de licenciamiento de Universidades")

# 5- Cantidad de carnes por univeridad 
aux <- resumen_sunedu %>% select(NOMBRE) %>% filter(CANTIDAD_CARNES > 10000) %>% 
                            summarise(suma_canres = sum(CANTIDAD_CARNES))
grafico5 <- ggplot(aux, aes(y=NOMBRE, x=suma_canres, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.5,show.legend = FALSE) + 
                        labs(y='Universidades',x='Carnes', 
                        title='Numero de carnes por Universidad')
grafico5

# 6- Cantidad de programas por Universidad 
aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
grafico6 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                    labs(y='Universidades',x='Programas', title='Numero de Programas por Universidad')
grafico6

# 7- Periodo de licenciamiento segun tipo de gestion
aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico7 <- ggplot(aux3, aes(y=TIPO_GESTION,x=suma_lic, fill=TIPO_GESTION)) + 
                geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                        labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                        title='Comparativa entre numero de licenciamientos entre Público y Privado')
grafico7

# 8- Periodo de licenciamiento segun cada departamento del Perú
aux4 <- licenciamiento %>% group_by(DEPARTAMENTO_LOCAL) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
grafico8 <- ggplot(aux4, aes(y=DEPARTAMENTO_LOCAL, x=suma_lic, fill=DEPARTAMENTO_LOCAL)) + 
                    theme_minimal() + geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Departamentos', x='Periodo de licenciamiento', 
                    title='Periodo de Licenciamiento según Departamento')
grafico8

# 9- Periodo de licenciamiento segun cada universidad
aux5 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO)) 
grafico9 <- ggplot(aux5, aes(y=NOMBRE, x=suma_lic, fill=NOMBRE)) + theme_minimal() + 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Periodo de licenciamiento', title='Periodo de Licenciamiento según Departamento')
grafico9

# 10- Universidades que tienen mas de 7 años de licencia activa 
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO) %>% 
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', PERIODO_LICENCIAMIENTO >= 7)
grafico10 <- ggplot(query, aes(y=NOMBRE, x=PERIODO_LICENCIAMIENTO)) + theme_minimal() + 
                    geom_boxplot()
grafico10

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

# 13- Universidades que tienen mas del promedio de programas totales
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>%
						filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL), CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
grafico13 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=PROGRAMAS_TOTAL))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Departamento', 
                        title='Numero de estudiantes que superan el promedio en su departamento')
grafico13

# 14- Universidades que tienen menos del promedio de carnes
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>% filter(CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
grafico14 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE))+ 
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                    labs(y='Universidades', x='Carnes', 
                        title='Numero de estudiantes que superan el promedio en su departamento')
grafico14

# 15- Histograma de periodo de frecuencia de licenciamiento
hist(resumen_sunedu$PERIODO_LICENCIAMIENTO, 
                main='Histograma de periodo de frecuencia de licenciamiento')

# 16- Universidades que tienen mas del promedio de programas totales y que cuentan con licencia 
query <- resumen_sunedu %>% select(NOMBRE,PROGRAMAS_TOTAL) %>% filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL))
grafico16 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL)) + 
                geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                labs(y='Universidades', x='Programas', 
                    title='Universidades que tienen mas del promedio de programas')

# 17- Hiatograma de frecuencia de carnes universitarios
hist(resumen_sunedu$CANTIDAD_CARNES, main='Hiatograma de frecuencia de carnes universitarios')

# 18- Diagrama de cajas para los programas totales y la cantidad de carnes para las universidades licenciadas
# Sin outliers
boxplot(resumen_sunedu$PROGRAMAS_TOTAL, 
        resumen_sunedu$CANTIDAD_CARNES, outline = FALSE)

# 19- Diagrama de cajas para los programas totales y la cantidad de carnes para las universidades licenciadas
# Con outliers
boxplot(resumen_sunedu$PROGRAMAS_TOTAL, 
        resumen_sunedu$CANTIDAD_CARNES)

# 20- Diagrama de dispercion entre los programas y la cantidad de carnes
grafico20 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES)) + geom_point(color="blue") + 
                    labs(y='Carnes', x='Programas', 
                    title="Diagrama de Disperción Entre Programas y Carnes")
grafico20

# 21- Histograma de frecuencua de programas de universidades
hist(resumen_sunedu$PROGRAMAS_TOTAL, 
        main='Histograma de frecuencua de programas de universidades')
