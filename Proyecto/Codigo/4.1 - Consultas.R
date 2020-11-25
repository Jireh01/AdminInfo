# Librerias
library(dplyr)

# Cargamos los data.frames
licenciamiento <- read.csv("Datasets/2-Preprocesados/licenciamiento.csv")
carnes <- read.csv("Datasets/2-Preprocesados/carnes.csv")
programas <- read.csv("Datasets/2-Preprocesados/programas.csv")
resumen_sunedu <- read.csv("Datasets/3-Generado/resumen_sunedu.csv")

# Estadistica Descriptiva
# Media, Maximo, Minimo, Cuartil y Percentil de los Programas Universitarios en total de cada universidad
estadistica_programas <- resumen_sunedu %>% summarise(Cantidad = n(),
			Media = mean(PROGRAMAS_TOTAL, na.rm = TRUE),
			Maximo = max(PROGRAMAS_TOTAL, na.rm = TRUE),
			Minimo = min(PROGRAMAS_TOTAL, na.rm = TRUE),
			Q25 = quantile(PROGRAMAS_TOTAL, .25, na.rm = TRUE),
			Q50 = quantile(PROGRAMAS_TOTAL, .50, na.rm = TRUE),
			Q75 = quantile(PROGRAMAS_TOTAL, .75, na.rm = TRUE),)
estadistica_programas

# Media, Maximo, Minimo, Cuartil y Percentil de los Programas Universitarios en total de cada universidad
estadistica_carnes <- resumen_sunedu %>% summarise(Cantidad = n(),
			Media = mean(CANTIDAD_CARNES, na.rm = TRUE),
			Maximo = max(CANTIDAD_CARNES, na.rm = TRUE),
			Minimo = min(CANTIDAD_CARNES, na.rm = TRUE),
			Q25 = quantile(CANTIDAD_CARNES, .25, na.rm = TRUE),
			Q50 = quantile(CANTIDAD_CARNES, .50, na.rm = TRUE),
			Q75 = quantile(CANTIDAD_CARNES, .75, na.rm = TRUE),)
estadistica_carnes

# Consultas
# 1. Lista de universidades que tienen programas de Doctorado que se encuentren
#		licenciadas segun la lista de licenciamiento de Abril del 2020. Mostrar tambien
#		la cantidad de programas y alumnos que tiene cada una respectivamente.
query <- programas %>%
					filter(NIVEL_ACADEMICO == "DOCTORADO") %>%
					group_by(NOMBRE,NIVEL_ACADEMICO) %>%
					summarize(PROGRAMAS = n()) %>%
					inner_join(licenciamiento %>%
											filter(ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
											select(NOMBRE, ESTADO_LICENCIAMIENTO),
								by=c("NOMBRE"="NOMBRE")) %>%
					inner_join(carnes %>%
										group_by(NOMBRE_UNIVERSIDAD) %>%
										summarize(CANTIDAD_CARNES = sum(Cant_Carnes)),
								by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
					select(NOMBRE, NIVEL_ACADEMICO, ESTADO_LICENCIAMIENTO, PROGRAMAS, ALUMNOS = CANTIDAD_CARNES)	
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

# 4. Cantidad de universidades publicas que tienen licenciatura fuera de lima
#		y su cantidad respectiva de estudiantes que no sean de ingenieria ordenado de
#		mayor a menor cantidad de estudiantes
query <- licenciamiento %>% 
						filter(TIPO_GESTION == "PUBLICO", 
								ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA",
								DEPARTAMENTO_LOCAL != "LIMA") %>%
						select(NOMBRE,TIPO_GESTION,ESTADO_LICENCIAMIENTO,DEPARTAMENTO_LOCAL) %>%
						inner_join(carnes %>%
										filter(NOMBRE_CLASE_PROGRAMA != "SALUD")%>%
										group_by(NOMBRE_UNIVERSIDAD, ) %>%
										summarize(ESTUDIANTES = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						arrange(desc(ESTUDIANTES))
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

# 15. Universidades que tienen mas del promedio de programas totales y menos del promedio de cantidad de carnes
query <- resumen_sunedu %>%
						select(NOMBRE, PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>%
						filter(PROGRAMAS_TOTAL > mean(PROGRAMAS_TOTAL), CANTIDAD_CARNES < mean(CANTIDAD_CARNES))
head(query)

# 16. Cantidad de alumnos de universidades publicas en la carrera de Salud
query <- carnes %>%
				filter(TIPO_GESTION=="PUBLICO", NOMBRE_CLASE_PROGRAMA=="SALUD") %>%
				summarize(Cantidad = sum(Cant_Carnes))
head(query)

#17. Funcion para determinar el estado de licenciamiento de una universidad segun la lista de Abril 2020
alumnosTotal <-function(nombre){
	query <- licenciamiento %>%
							filter(NOMBRE == nombre) %>%
							select(NOMBRE, ESTADO_LICENCIAMIENTO)
	return(query)
}
query <- alumnosTotal("UNIVERSIDAD CESAR VALLEJO")
query

# 18. Cantidad de alumnos de universidades cuyas licencias fueron revocadas
query <- licenciamiento %>%
						select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
						filter(ESTADO_LICENCIAMIENTO == "LICENCIA DENEGADA") %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(CANTIDAD_ALUMNOS = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						summarize(TOTAL = sum(CANTIDAD_ALUMNOS))
head(query)

#19. Departamento que mas universidades con licencia denegada tiene que no sea Lima
query <- licenciamiento %>% 
						filter(DEPARTAMENTO_LOCAL != "LIMA", ESTADO_LICENCIAMIENTO!="LICENCIA OTORGADA") %>%
						group_by(DEPARTAMENTO_LOCAL) %>%
						summarize(UNIVERSIDADES = n()) %>%
						filter(UNIVERSIDADES == first(licenciamiento %>% 
												filter(DEPARTAMENTO_LOCAL != "LIMA", ESTADO_LICENCIAMIENTO!="LICENCIA OTORGADA") %>%
												group_by(DEPARTAMENTO_LOCAL) %>%
												summarize(UNIVERSIDADES = n()) %>%
												summarize (MAX = max(UNIVERSIDADES))))
head(query)

#20. Cantidad de programas pregrado y posgrado en Lima
query <- programas %>%
					filter(DEPARTAMENTO_LOCAL=="LIMA") %>%
					group_by(DEPARTAMENTO_LOCAL,TIPO_NIVEL_ACADEMICO) %>%
					summarize(CANTIDAD = n()) 
head(query)

#21. Estudiantes en total de cada departamento del Peru que supera el promedio de estudiantes por departamento
query <- carnes %>%
				group_by(DEPARTAMENTO_FILIAL) %>%
				summarize(CANTIDAD = sum(Cant_Carnes)) %>%
				filter(CANTIDAD > mean(CANTIDAD))
head(query)

#22. Lista de universidades que tienen la menor cantidad de periodo de funcionamiento (que no sea 0)
query <- licenciamiento %>% 
						filter(PERIODO_LICENCIAMIENTO != 0) %>%
						select(NOMBRE, PERIODO_LICENCIAMIENTO) %>%
						filter(PERIODO_LICENCIAMIENTO == min(PERIODO_LICENCIAMIENTO))
head(query)

#23. Lista de universidades licenciadas y la cantidad de sus estudiantes de cada departamento
query <- licenciamiento %>%
						filter(ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
						select(NOMBRE,DEPARTAMENTO_LOCAL,ESTADO_LICENCIAMIENTO) %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(ESTUDIANTES = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						arrange(DEPARTAMENTO_LOCAL)
head(query)

#24. Cantidad de estudiantes de Ingenieria que no son de Lima
query <- carnes %>%
				filter(grepl("INGENIERIA",NOMBRE_PROGRAMA), DEPARTAMENTO_FILIAL!="LIMA" ) %>%
				group_by(NOMBRE_PROGRAMA) %>%
				summarize(CANT = sum(Cant_Carnes)) %>%
				summarize(CANTIDAD = sum(CANT))
head(query)

#25. Funcion para saber cuantos estudiantes en total tiene una universidad
alumnosTotal <-function(nombre){
	query <- carnes %>%
					filter(NOMBRE_UNIVERSIDAD == nombre) %>%
					select(NOMBRE_UNIVERSIDAD, Cant_Carnes) %>%
					group_by(NOMBRE_UNIVERSIDAD) %>%
					summarize(n = sum(Cant_Carnes))
	return(query)
}
query <- alumnosTotal("UNIVERSIDAD ALAS PERUANAS")
query

# 26. Mostrar lista de universidades y la cantidad de programas posgrado que tienen y ordenar ascendentemente
query <- programas %>%
					filter(TIPO_NIVEL_ACADEMICO == "POSGRADO") %>%
					group_by(NOMBRE) %>%
					summarize(PROGRAMAS= n()) %>%
					arrange(PROGRAMAS)
head(query)

# 27. Universidades que cuentan con programas de doctorado en el sector publico
query <- programas %>%
					select(NOMBRE, TIPO_GESTION, NIVEL_ACADEMICO) %>% 
					filter(TIPO_GESTION == 'PUBLICO', NIVEL_ACADEMICO == 'DOCTORADO')
head(query)

# 28. Universidades que tienen la licencia denegada segun la lista mas actualizada de la SUNEDU 
query <- licenciamiento %>%
						select(NOMBRE, ESTADO_LICENCIAMIENTO) %>%
						filter(ESTADO_LICENCIAMIENTO == 'LICENCIA DENEGADA')
head(query)

# 29. Universidades privadas que tienen licenciamiento segun la lista de Abril 2020 y que tienen mas del
#		promedio de programas academicos. Mostrar el estado de licenciamiento y cantidad de programas
#		academicos. Tambien, ordenar por mayor cantidad de programas y como segunda prioridad alfabeticamente.
query <- licenciamiento %>%
						filter(TIPO_GESTION == "PRIVADO",
							ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
						select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
						inner_join(programas %>%
											group_by(NOMBRE) %>%
											summarize(PROGRAMAS = n()) %>%
											filter(PROGRAMAS > mean(PROGRAMAS)),
									by=c("NOMBRE"="NOMBRE")) %>%
						arrange(desc(PROGRAMAS), NOMBRE)
head(query)

# 30. 


