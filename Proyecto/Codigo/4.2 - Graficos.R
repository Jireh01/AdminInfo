
# 1- Universidades publicas que cuentan con el curso Ingenieria Mecanica 
grafico <- ggplot(q3, aes(y=NIVEL_ACADEMICO, x=NOMBRE_FILIAL)) + 
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

# 5- Cantidad de carnes por univeridad (no se ve tan bien)
aux <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_canres = sum(CANTIDAD_CARNES))
grafico5 <- ggplot(aux, aes(y=NOMBRE, x=suma_canres, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                        labs(y='Universidades',x='Carnes', 
                        title='Numero de carnes por Universidad')
grafico5

# 6- Cantidad de programas por Carreras (se ve muy mal)
aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
grafico6 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                    geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                    labs(y='Universidades',x='Carnes', title='Numero de Programas por Universidad')
grafico6

# 7- periodo de licenciamiento segun tipo de gestion
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

# 10- Universidades que tienen la licencia otorgada segun la SUNEDU y le quedan mas de 7 años de licencia
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO) %>% 
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', PERIODO_LICENCIAMIENTO >= 7)
grafico10 <- ggplot(query, aes(y=NOMBRE, x=PERIODO_LICENCIAMIENTO)) + theme_minimal() + 
                    geom_boxplot()
grafico10

