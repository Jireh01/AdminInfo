vinos<-read.csv("winequality-red.csv")
carnes<-read.csv("carnes.csv")
licenciamiento<-read.csv("licenciamiento.csv")
resumen_sunedu<-read.csv("resumen_sunedu.csv")
programa<-read.csv("programas.csv")
programas<-read.csv("programas.csv")


## REGRESION POLINOMIAL ##
ids <- sample(1:nrow(vinos),size=nrow(vinos)*0.7,replace = FALSE)
entrenamientoRP <- vinos[ids, c(1,3,8,9)]
regresionPolinomial <- vinos[-ids, c(1,3,8,9)]
ft = lm(fixed.acidity~ citric.acid + density + pH, data=entrenamientoRP)
predict(ft, regresionPolinomial)
regresionPolinomial$prediccion <- predict(ft, regresionPolinomial)

###########################
####### QUERIES ###########
###########################

# QUERY 1
query1 <- programas %>%
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


# QUERY 4
query4 <- licenciamiento %>% 
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


# QUERY 10
query10 <- licenciamiento %>%
	filter(TIPO_GESTION == "PRIVADO",
		   ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
	select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
	inner_join(programas %>%
			   	group_by(NOMBRE) %>%
			   	summarize(PROGRAMAS = n()) %>%
			   	filter(PROGRAMAS > mean(PROGRAMAS)),
			   by=c("NOMBRE"="NOMBRE")) %>%
	arrange(desc(PROGRAMAS), NOMBRE)


# QUERY 13
query13 <- licenciamiento %>%
	filter(DEPARTAMENTO_LOCAL=="ICA",TIPO_GESTION=="PRIVADO") %>% 
	select(NOMBRE) %>%
	inner_join(carnes %>%
			   	group_by(NOMBRE_UNIVERSIDAD) %>%
			   	summarize(CANTIDAD_CARNES = sum(Cant_Carnes)),
			   by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
	select(CANTIDAD_ALUMNOS = CANTIDAD_CARNES)


# QUERY 18
query18 <- licenciamiento %>%
	select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
	filter(ESTADO_LICENCIAMIENTO == "LICENCIA DENEGADA") %>%
	inner_join(carnes %>%
			   	group_by(NOMBRE_UNIVERSIDAD) %>%
			   	summarize(CANTIDAD_ALUMNOS = sum(Cant_Carnes)),
			   by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
	summarize(TOTAL = sum(CANTIDAD_ALUMNOS))

# QUERY 19
query19 <- licenciamiento %>% 
	filter(DEPARTAMENTO_LOCAL != "LIMA", ESTADO_LICENCIAMIENTO!="LICENCIA OTORGADA") %>%
	group_by(DEPARTAMENTO_LOCAL) %>%
	summarize(UNIVERSIDADES = n()) %>%
	filter(UNIVERSIDADES == first(licenciamiento %>% 
								  	filter(DEPARTAMENTO_LOCAL != "LIMA",
								  		   ESTADO_LICENCIAMIENTO!="LICENCIA OTORGADA") %>%
								  	group_by(DEPARTAMENTO_LOCAL) %>%
								  	summarize(UNIVERSIDADES = n()) %>%
								  	summarize (MAX = max(UNIVERSIDADES))))


# QUERY 23
query23 <- licenciamiento %>%
	filter(ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
	select(NOMBRE,DEPARTAMENTO_LOCAL,ESTADO_LICENCIAMIENTO) %>%
	inner_join(carnes %>%
			   	group_by(NOMBRE_UNIVERSIDAD) %>%
			   	summarize(ESTUDIANTES = sum(Cant_Carnes)),
			   by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
	arrange(DEPARTAMENTO_LOCAL)

# QUERY 25
alumnosTotal <-function(nombre){
	query <- carnes %>%
		filter(NOMBRE_UNIVERSIDAD == nombre) %>%
		select(NOMBRE_UNIVERSIDAD, Cant_Carnes) %>%
		group_by(NOMBRE_UNIVERSIDAD) %>%
		summarize(n = sum(Cant_Carnes))
	return(query)
}
query25 <- alumnosTotal("UNIVERSIDAD ALAS PERUANAS")

# QUERY 29
query29 <- programas %>%
	select(NOMBRE, DEPARTAMENTO_LOCAL, TIPO_NIVEL_ACADEMICO) %>% 
	filter(DEPARTAMENTO_LOCAL != 'LIMA', TIPO_NIVEL_ACADEMICO == 'POSGRADO') %>%
	group_by(NOMBRE) %>% summarize(PROGRAMAS_TOTAL = n()) %>%
	arrange(desc(PROGRAMAS_TOTAL))

# QUERY 30
query30 <- programas %>%
	select(NOMBRE,NIVEL_ACADEMICO) %>%
	group_by(NOMBRE,NIVEL_ACADEMICO) %>%
	summarize(PROGRAMAS = n()) %>%
	arrange(NOMBRE,desc(PROGRAMAS))



