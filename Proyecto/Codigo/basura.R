library(ggplot2)

# esto deberia estar en preproce pero no funcionaba cuando cargaba denuevo
licenciamiento$PERIODO_LICENCIAMIENTO <- as.integer(licenciamiento$PERIODO_LICENCIAMIENTO)
# Universidades que tienen la licencia segun la SUNEDU y le quedan mas de 7 años de licencia
q2 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL) %>% 
            filter(licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', 
                licenciamiento$PERIODO_LICENCIAMIENTO >= 7)
grafico <- ggplot(q2, aes(y=DEPARTAMENTO_LOCAL, x=NOMBRE)) + theme_minimal() + geom_point(color="red",size=3) + labs(y="Departamentos", x="Universidades", title="")
grafico

# Universidades publicas que cuentan con el curso Ingenieria Mecanica 
q3 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, NOMBRE_FILIAL, NIVEL_ACADEMICO) %>% 
            filter(programas$TIPO_GESTION == 'PUBLICO', 
            programas$DENOMINACION_PROGRAMA == 'INGENIERIA MECANICA')
head(q3)
grafico <- ggplot(q3, aes(y=NIVEL_ACADEMICO, x=NOMBRE_FILIAL)) + theme_minimal() + geom_point(color="red",size=3) + labs(y="Departamentos", x="Universidades", title="")
grafico


# Universidades de ICA que no cuentan con licencia por SUNEDU
q5 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL, TIPO_GESTION) %>% 
            filter(licenciamiento$DEPARTAMENTO == 'ICA', licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA DENEGADA')
head(q5)

# Cursos de universidades de Junin que cuentan con Maestria 
q9 <- programas %>% select(CODIGO_ENTIDAD, NOMBRE, TIPO_GESTION, DENOMINACION_PROGRAMA) %>% 
                filter(programas$DEPARTAMENTO_FILIAL == 'JUNIN', programas$NIVEL_ACADEMICO == 'MAESTRIA', 
                        programas$TIPO_GESTION == 'PRIVADO')
head(q9)

q12 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL) %>% 
                filter(licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA',                      licenciamiento$PERIODO_LICENCIAMIENTO == 6, 
                        licenciamiento$DEPARTAMENTO_LOCAL == 'LIMA') %>% group_by(NOMBRE)
head(q12)