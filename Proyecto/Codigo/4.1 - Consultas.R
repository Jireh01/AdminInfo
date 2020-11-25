# Librerias
library(dplyr)

# Cargamos los data.frames
licenciamiento <- read.csv("Datasets/2-Preprocesados/licenciamiento.csv")
carnes <- read.csv("Datasets/2-Preprocesados/carnes.csv")
programas <- read.csv("Datasets/2-Preprocesados/programas.csv")
resumen_sunedu <- read.csv("Datasets/3-Generado/resumen_sunedu.csv")

# Estadistica Descriptiva
# Media, Maximo, Minimo, Cuartil y Percentil de los Programas Universitarios en total de cada universidad
estadistica <- resumen_sunedu %>% summarise(Cantidad = n(),
			Media = mean(PROGRAMAS_TOTAL, na.rm = TRUE),
			Maximo = max(PROGRAMAS_TOTAL, na.rm = TRUE),
			Minimo = min(PROGRAMAS_TOTAL, na.rm = TRUE),
			Q25 = quantile(PROGRAMAS_TOTAL, .25, na.rm = TRUE),
			Q50 = quantile(PROGRAMAS_TOTAL, .50, na.rm = TRUE),
			Q75 = quantile(PROGRAMAS_TOTAL, .75, na.rm = TRUE),)
estadistica

# Consultas
# 1. Universidades que tienen la licencia denegada segun la lista mas actualizada de la SUNEDU 
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO) %>%
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA DENEGADA')
head(query)

# 2. Universidades que tienen la licencia otorgada segun la SUNEDU y le quedan mas de 7 años de licencia
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO) %>% 
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', PERIODO_LICENCIAMIENTO >= 7)
head(query)

# 3. Universidades publicas que cuentan con un programa de Ingenieria Mecanica 
query <- programas %>%
					select(NOMBRE, TIPO_GESTION,DENOMINACION_PROGRAMA) %>%
					filter(TIPO_GESTION == 'PUBLICO', DENOMINACION_PROGRAMA == 'INGENIERIA MECANICA')
head(query)

# 4. Universidades que cuentan con programas de doctorado en el sector publico
query <- programas %>%
					select(NOMBRE, TIPO_GESTION, NIVEL_ACADEMICO) %>% 
					filter(TIPO_GESTION == 'PUBLICO', NIVEL_ACADEMICO == 'DOCTORADO')
head(query)

# 5. Universidades de ICA que no cuentan con licencia por SUNEDU actualmente
query <- licenciamiento %>%
						select(NOMBRE, DEPARTAMENTO_LOCAL, ESTADO_LICENCIAMIENTO) %>%
						filter(DEPARTAMENTO_LOCAL == 'ICA', ESTADO_LICENCIAMIENTO == 'LICENCIA DENEGADA')
head(query)

# 6. Universidades privadas que tienen mas de 10000 estudiantes 
query <- carnes %>%
				select(NOMBRE_UNIVERSIDAD, TIPO_GESTION, Cant_Carnes) %>%
				filter(TIPO_GESTION == 'PRIVADO') %>%
				group_by(NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>%
				summarize(CANTIDAD_CARNES = sum(Cant_Carnes)) %>%
				filter(CANTIDAD_CARNES > 10000)
head(query)

# 7. Cantidad de programas que tiene la UPC sede San Miguel
query <- programas %>%
					select(NOMBRE, DISTRITO_LOCAL) %>% 
                    filter(NOMBRE == 'UNIVERSIDAD PERUANA DE CIENCIAS APLICADAS', DISTRITO_LOCAL == 'SAN MIGUEL') %>%
					group_by(NOMBRE,DISTRITO_LOCAL) %>%
					summarize(PROGRAMAS_TOTAL = n())
head(query)

# 8. Universidades que no tienen un departamento filial en su registro y cuentan con informatica como curso
query <- carnes %>%
				select(NOMBRE_UNIVERSIDAD, NOMBRE_CLASE_PROGRAMA, NOMBRE_FILIAL, Cant_Carnes) %>% 
				filter(NOMBRE_CLASE_PROGRAMA == 'INFORMATICA', NOMBRE_FILIAL == '[NO ESPECIFICADO]') %>%
				group_by(NOMBRE_UNIVERSIDAD, NOMBRE_CLASE_PROGRAMA, NOMBRE_FILIAL) %>%
				summarize(ESTUDIANTES EN TOTAL = sum(Cant_Carnes))
head(query)

# 9. Cantidad de programas de maestria de universidades privadas de Junin
query <- programas %>%
					select(NOMBRE, TIPO_GESTION, DEPARTAMENTO_FILIAL, DENOMINACION_PROGRAMA, NIVEL_ACADEMICO) %>% 
					filter(TIPO_GESTION == 'PRIVADO', DEPARTAMENTO_FILIAL == 'JUNIN', NIVEL_ACADEMICO == 'MAESTRIA') %>%
					group_by(NOMBRE, TIPO_GESTION, DEPARTAMENTO_FILIAL) %>%
					summarize(PROGRAMAS_TOTAL = n())
head(query)

#10. Cuantas universidades que se encuentran fuera de LIMA cuentan con programas postgrado, ordernar de manera descendiente
query <- programas %>%
					select(NOMBRE, DEPARTAMENTO_LOCAL, TIPO_NIVEL_ACADEMICO) %>% 
					filter(DEPARTAMENTO_LOCAL != 'LIMA', TIPO_NIVEL_ACADEMICO == 'POSGRADO') %>%
					group_by(NOMBRE) %>% summarize(PROGRAMAS_TOTAL = n()) %>%
					arrange(desc(PROGRAMAS_TOTAL))
head(query)

# 11. Cantidad de cursos de la carrera de derecho que tienen menos del promedio de carnes que tiene cada curso
query <- carnes %>%
				select(NOMBRE_CLASE_PROGRAMA, Cant_Carnes) %>%
				filter(Cant_Carnes < first(carnes %>% summarize(media = mean(Cant_Carnes))),
						NOMBRE_CLASE_PROGRAMA == 'DERECHO') %>%
				group_by(NOMBRE_CLASE_PROGRAMA) %>% summarize(Vec = n())
head(query)

# 12. Universidades que cuentan con 6 años de licenciamiento y pertenecen al departamento de Lima
query <- licenciamiento %>%
						select(NOMBRE,DEPARTAMENTO_LOCAL, ESTADO_LICENCIAMIENTO, PERIODO_LICENCIAMIENTO) %>% 
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA',
								PERIODO_LICENCIAMIENTO == 6,
								DEPARTAMENTO_LOCAL == 'LIMA')
head(query)

# 13. Cantidad de estudiantes de universidades privadas en ICA
query <- licenciamiento %>%
						filter(DEPARTAMENTO_LOCAL=="ICA",TIPO_GESTION=="PRIVADO") %>% 
						select(NOMBRE) %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(CANTIDAD_CARNES = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						select(CANTIDAD_ALUMNOS = CANTIDAD_CARNES)
head(query)

# 14. Universidades privadas que no son de Lima que no tienen licencia
query <- licenciamiento %>%
						select(NOMBRE,TIPO_GESTION,DEPARTAMENTO_LOCAL,ESTADO_LICENCIAMIENTO) %>%
						filter(TIPO_GESTION == "PRIVADO",
							DEPARTAMENTO_LOCAL != "LIMA",
							ESTADO_LICENCIAMIENTO != "LICENCIA OTORGADA")
head(query)

# 15. Cantidad de alumnos de universidades cuyas licencias fueron revocadas
query <- licenciamiento %>%
						select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
						filter(ESTADO_LICENCIAMIENTO == "LICENCIA DENEGADA") %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(CANTIDAD_ALUMNOS = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						summarize(TOTAL = sum(CANTIDAD_ALUMNOS))
head(query)

# 16. Universidades que tienen mas del promedio de programas totales y menos del promedio de cantidad de carnes
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>%
						filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL), CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
head(query)

# 17. Cantidad de alumnos de universidades publicas en la carrera de Salud
query <- carnes %>%
				filter(TIPO_GESTION=="PUBLICO", NOMBRE_CLASE_PROGRAMA=="SALUD") %>%
				summarize(Cantidad = sum(Cant_Carnes))

head(query)






q15 <- licenciamiento %>% group_by(DEPARTAMENTO_LOCAL) %>% 
                summarise(percent70 = quantile(licenciamiento$PERIODO_LICENCIAMIENTO, probs = .5)) 
view(q15)
head(q15)

# Universidades que cuentan con 0 años de licencia
q16 <- licenciamiento %>% select(NOMBRE) %>% 
            filter(licenciamiento$PERIODO_LICENCIAMIENTO == min(licenciamiento$PERIODO_LICENCIAMIENTO))
view(q16)


q17 <- carnesEx %>% select(CODIGO, NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>% 
                    filter(carnesEx$Cant_Carnes < max(carnesEx$Cant_Carnes))
head(q17)






# Universidades que tienen la licencia segun la SUNEDU y le quedan mas de 7 años de licencia
q2 <- licenciamiento %>% select(CODIGO_ENTIDAD, NOMBRE, DEPARTAMENTO_LOCAL) %>% 
            filter(licenciamiento$ESTADO_LICENCIAMIENTO == 'LICENCIA OTORGADA', 
                licenciamiento$PERIODO_LICENCIAMIENTO >= 7)
grafico <- ggplot(q2, aes(y=DEPARTAMENTO_LOCAL, x=NOMBRE)) + theme_minimal() + geom_point(color="red",size=3) + labs(y="Departamentos", x="Universidades", title="")
grafico

####
group_nombre<-inner_join(programas%>%group_by(NOMBRE),licenciamiento%>%group_by(NOMBRE),by="NOMBRE")%>%select(NOMBRE,PERIODO_LICENCIAMIENTO.x)
group_programa<-programas%>%group_by(NOMBRE)%>%summarise(total_programas=n())

View(inner_join(group_nombre,group_programa, by="NOMBRE"))