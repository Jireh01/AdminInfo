library("ggplot2")
library("shinyjs")#para la funcion toggle
library("shiny")
library("readxl")#para lectura de excel
library("dplyr")
library("lubridate")#para fechas
library("sqldf")
library("plotly")#para graficos dinamicos
library("shinythemes")
library("ade4")
library("DT")

## Renzo
#setwd("D:/Renxzen/Documentos/UPC/2020-02/Admin Info/TF/Trabajo-Final-Adminfo/Proyecto/Codigo/6 - Shiny")

source("www/inicio.R")
source("www/recoleccion.R")
source("www/querys.R")
source("www/graficos.R")
source("www/informe.R")
source("www/modelo.R")
source("www/consultas.R")
source("tables.R")


ui <- fluidPage(
    theme = shinytheme("yeti"),
    navbarPage("Proyecto de Universidades peruanas 2019-2020", 
               tabPanel("Inicio",inicio),
               tabPanel("Recoleccion", recoleccion), 
               tabPanel("Preprocesamiento", querys),
               tabPanel("Consultas", navConsultas),
               tabPanel("Graficos", graficos),
               tabPanel("Modelo", modelo),
               tabPanel("Informe", informe))
)

server <- function(input, output) {
    output$idTabla1 <- renderTable({
        file <- input$idArchivo
        read.csv(file$datapath)
    })
    
    output$tildes <- renderText({ '
        #Retirar tildes
        
            quitarTildes <- function(dataFrame){
            conTilde <- c("Á", "É", "Í", "Ó", "Ú", "Ñ", "Ü", "á", "é", "í", "ó", "ú", "ü")
            sinTilde <- c("A", "E", "I", "O", "U", "N", "U", "a", "e", "i", "o", "u", "ü")
        
            for (i in seq_along(conTilde)){
                for (j in colnames(dataFrame)){
                    dataFrame[[j]] <- str_replace_all(dataFrame[[j]], conTilde[i], sinTilde[i])
                }
                    }
                    return(dataFrame)
                }
            licenciamiento <- quitarTildes(licenciamiento)
            carnes <- quitarTildes(carnes)
            programas <- quitarTildes(programas)' })
    
    output$remover <- renderText({ '
         #Reemplazar los espacios en blanco de los dataFrames por NA
         
            removerEnBlancos <- function(dataFrame){
                for (i in colnames(dataFrame)){
                    dataFrame[[i]][dataFrame[[i]] == ] <- NA
                }
                return(dataFrame)
            }
            licenciamiento <- removerEnBlancos(licenciamiento)
            carnes <- removerEnBlancos(carnes)
            programas <- removerEnBlancos(programas)
            
            # Remover las lineas con mas de 2 columnas con NA
            licenciamiento <- licenciamiento[rowSums(is.na(licenciamiento)) < 2, ]
            carnes <- carnes[rowSums(is.na(carnes)) < 2, ]
            programas <- programas[rowSums(is.na(programas)) < 2, ]' })
    
    output$nombres <- renderText({ '
          # Remover S.A.C. y S.A. de los nombres
          
            quitarSAC <- function(dataFrame,i) {
                toRemove <- c(" S.A.C."," S.A.C"," S.A.", " S.A")
                for (j in seq_along(dataFrame[[i]])){
                    for (k in seq_along(toRemove)){
                        dataFrame[[i]][j] <- str_remove(dataFrame[[i]][j],toRemove[k])
                    }
                }
                return(dataFrame)
            }
            carnes <- quitarSAC(carnes,3)
            licenciamiento <- quitarSAC(licenciamiento,3)
            programas <- quitarSAC(programas,3)' })
    
    output$columnas <- renderText({ '
         # Remover las columnas innecesarias
            licenciamiento <- licenciamiento[,-(0:1), drop = FALSE]
            carnes <- carnes[,-(0:1), drop = FALSE]
            programas <- programas[,-(0:1), drop = FALSE]' })
    
    output$string <- renderText({ '
         # Convirtiendo string a entero
            carnes$CODIGO <- strtoi(carnes$CODIGO)
            carnes$CODIGO_CLASE_PROGRAMA <- strtoi(carnes$CODIGO_CLASE_PROGRAMA)
            carnes$Cant_Carnes <- strtoi(carnes$Cant_Carnes)
            carnes$ANIO_PERIODO <- strtoi(carnes$ANIO_PERIODO)
            licenciamiento$CODIGO_ENTIDAD <- strtoi(licenciamiento$CODIGO_ENTIDAD)
            licenciamiento$PERIODO_LICENCIAMIENTO <- strtoi(licenciamiento$PERIODO_LICENCIAMIENTO)
            programas$CODIGO_ENTIDAD <- strtoi(programas$CODIGO_ENTIDAD)
            programas$PERIODO_LICENCIAMIENTO <- strtoi(programas$PERIODO_LICENCIAMIENTO)
            programas$CODIGO_CLASE_PROGRAMA_N2 <- strtoi(programas$CODIGO_CLASE_PROGRAMA_N2)' })
    
    
    
    output$plot1 <- renderPlot({
        box<- input$opcion1
        if (is.null(box))
        {return(NULL)}
        
        if(box == "1")    {
            output$grafico1 <- renderText({
                "  # Historgrama de frecuencua de programas de universidades
                   grafico23 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL)) + 
                        geom_histogram(binwidth=14, color=black, fill=red) + labs(x=Programas Totales, 
                        title=Histograma de frecuencua de programas de universidades)"
                
            })
            
            output$texto1 <- renderText({
                "Interpretacion: La mayor frecuencia de programas se encuentra entre el rango 0 y 100. 
                 Además entre 440 y 500 hay un hueco donde hay 0 programas"
            })
            
            grafico23 <- ggplot(resumen_sunedu, aes(PROGRAMAS_TOTAL)) + 
                geom_histogram(binwidth=14, color='black', fill='red') + labs(x='Programas Totales', 
                                                                              title='Histograma de frecuencua de programas de universidades')
            return(grafico23)
        } 
        
        else{  if(box == "2") {
            output$grafico1 <- renderText({
                "  # Grafico programas vs carnes (programas <= 200 y carnes <= 20000)
                   s <- resumen_sunedu %>% select(PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>% 
                        filter(PROGRAMAS_TOTAL <= 200, CANTIDAD_CARNES <= 20000)
                    grafico25 <- ggplot(s, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES, colour=CANTIDAD_CARNES)) + geom_point(color=black, show.legend=TRUE) + 
                    stat_smooth(method=lm, aes(colour=linear)) + stat_smooth(method=lm, formula = y ~ log(x)) + 
                    labs(y=Carnes, x=Programas, 
                    title=Diagrama de Disperción Entre Programas (<=200) y Carnes(<=20000))"
            })
            
            output$texto1 <- renderText({
                "Interpretacion: Cuando los programas son 200 o menos y los carnes 20000 o menos, se puede llegar a sacar una regresion lineal 
                 algo acertada, como se puede ver en la gráfica. Mientras que la logarítmica es menos acertada, salvo en los primeros puntos"
            })
            
            s <- resumen_sunedu %>% select(PROGRAMAS_TOTAL, CANTIDAD_CARNES) %>% 
                filter(PROGRAMAS_TOTAL <= 200, CANTIDAD_CARNES <= 20000)
            grafico25 <- ggplot(s, aes(PROGRAMAS_TOTAL, CANTIDAD_CARNES, colour=CANTIDAD_CARNES)) + geom_point(color="black", show.legend=TRUE) + 
                stat_smooth(method='lm', aes(colour='linear')) + stat_smooth(method='lm', formula = y ~ log(x)) + 
                labs(y='Carnes', x='Programas', 
                     title="Diagrama de Disperción Entre Programas (<=200) y Carnes(<=20000)")
            return(grafico25)
        }
            else { if(box == "3") {
                output$grafico1 <- renderText({
                    "  # Hiatograma de frecuencia de carnes universitarios
                       grafico18 <- ggplot(resumen_sunedu, aes(CANTIDAD_CARNES)) + 
                        geom_histogram(binwidth=10000, color=black, fill=lightgreen) + labs(x=Carnes totales,
                        title=Hiatograma de frecuencia de carnes universitarios)"
                    
                })
                output$texto1 <- renderText({
                    "Interpretacion: Se puede ver que la mayor frecuencia esta entre 0 y los 1000 carnes de universidades licenciadas"
                })
                
                grafico18 <- ggplot(resumen_sunedu, aes(CANTIDAD_CARNES)) + 
                    geom_histogram(binwidth=10000, color="black", fill='lightgreen') + labs(x='Carnes totales',
                                                                                            title='Hiatograma de frecuencia de carnes universitarios')
                return(grafico18)
            }
                else { if(box == "4")  {
                    output$grafico1 <- renderText({
                        "  # Universidades que tienen menos del promedio de programas totales y que cuentan con licencia
                           query <- resumen_sunedu %>% select(NOMBRE,PROGRAMAS_TOTAL) %>% filter(PROGRAMAS_TOTAL < mean(PROGRAMAS_TOTAL))
                           grafico17 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE)) + 
                           geom_bar(stat=identity,width = 0.2,show.legend = FALSE) + 
                           labs(y=Universidades, x=Programas, 
                           title=Universidades que tienen menos del promedio de programas)"
                        
                    })
                    
                    output$texto1 <- renderText({
                        "Interpretacion: Hay 4 universidades con el numero minimo de programas totales. Ademas La universidad con mayor programas en esta categoria
                         es la Universidad Privada de Tacna con 90 programas aproximadamente"
                    })
                    
                    query <- resumen_sunedu %>% select(NOMBRE,PROGRAMAS_TOTAL) %>% filter(PROGRAMAS_TOTAL < mean(PROGRAMAS_TOTAL))
                    grafico17 <- ggplot(query, aes(y=NOMBRE, x=PROGRAMAS_TOTAL, fill=NOMBRE)) + 
                        geom_bar(stat="identity",width = 0.2,show.legend = FALSE) + 
                        labs(y='Universidades', x='Programas', 
                             title='Universidades que tienen menos del promedio de programas')
                    return(grafico17)
                } 
                    else { if(box=="5")  {
                        output$grafico1 <- renderText({
                            "  # Periodo de licenciamiento segun cada departamento del Perú
                               aux4 <- licenciamiento %>% group_by(DEPARTAMENTO_LOCAL) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
                               grafico9 <- ggplot(aux4, aes(y=DEPARTAMENTO_LOCAL, x=suma_lic, fill=DEPARTAMENTO_LOCAL)) + 
                               theme_minimal() + geom_bar(stat=identity,width = 0.4) + 
                               labs(y=Departamentos, x=Periodo de licenciamiento, 
                               title=Periodo de Licenciamiento según Departamento)  "
                        })
                        
                        output$texto1 <- renderText({
                            "Interpretacion: La maxima cantidad de periodos por deparamento. Se suman todos los periodos (estan en años). Se puede ver como LIMA sobre pasa
                             al resto de departamentos por mucho, teniendo mas de 250 años acumulados en licenciamientos."
                        })
                        
                        aux4 <- licenciamiento %>% group_by(DEPARTAMENTO_LOCAL) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
                        grafico9 <- ggplot(aux4, aes(y=DEPARTAMENTO_LOCAL, x=suma_lic, fill=DEPARTAMENTO_LOCAL)) + 
                            theme_minimal() + geom_bar(stat="identity",width = 0.4) + 
                            labs(y='Departamentos', x='Periodo de licenciamiento', 
                                 title='Periodo de Licenciamiento según Departamento')
                        return(grafico9)
                    }
                        
                    } } } } }
    )
    
    

    output$plot2 <- renderPlot({
        box<- input$opcion2
        if (is.null(box))
        {return(NULL)}
        
        if(box == "1")    {
            output$grafico2 <- renderText({
                "ggplot(licenciamiento,aes(x=DEPARTAMENTO_LOCAL, y=, fill=DEPARTAMENTO_LOCAL))+ geom_bar(stat=identity,color=white)+ coord_polar(theta=x)+
                labs(y=,x=Departamentos, title=Universidades segun departamento)"
                
            })
            
            g_ploteo<-ggplot(licenciamiento,aes(x=DEPARTAMENTO_LOCAL, y=" ", fill=DEPARTAMENTO_LOCAL))+ geom_bar(stat="identity",color="white")+ coord_polar(theta="x")+
                labs(y=" ",x="Departamentos", title="Universidades segun departamento")
            return(g_ploteo)
            
        } 
        
        else{  if(box == "2") {
            output$grafico2 <- renderText({
                " # Universidades privadas  que tienen mas de 10000 estudiantes 
                  query2 <- carnes %>% select(NOMBRE_UNIVERSIDAD, TIPO_GESTION, Cant_Carnes) %>%
                                             filter(TIPO_GESTION == PRIVADO) %>% group_by(NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>%
                                             summarize(CANTIDAD_CARNES = sum(Cant_Carnes)) %>% filter(CANTIDAD_CARNES > 10000)
                         grafico <- ggplot(query2, aes(y=NOMBRE_UNIVERSIDAD, x=CANTIDAD_CARNES, fill=NOMBRE_UNIVERSIDAD)) + theme_minimal() + 
                         geom_bar(stat=identity, width = 0.2, show.legend = FALSE) + labs(y=Universidades, x=Numero de estudiantes, 
                         title = Numero de estudiantes por universidades con mas de 10000)
                 # Interpretacion: Muestra la cantidad de alumnos por universidad. Se puede ver como La vallejo gana por goleada a comparacion de cualquier otra
                 # Seguido de la Universidad Alas Peruanas. Las dos universidades con menor numero de estudiantes son: Universidad Catolica Sedes Sapientiae y 
                 # la Telesup"
                
            })
            output$texto2 <- renderText({
                "Interpretacion: Muestra la cantidad de alumnos por universidad. Se puede ver como La vallejo gana por goleada a comparacion de cualquier otra
                 Seguido de la Universidad Alas Peruanas. Las dos universidades con menor numero de estudiantes son: Universidad Catolica Sedes Sapientiae y 
                 la Telesup"
            })
            
            query2 <- carnes %>% select(NOMBRE_UNIVERSIDAD, TIPO_GESTION, Cant_Carnes) %>%
                filter(TIPO_GESTION == 'PRIVADO') %>% group_by(NOMBRE_UNIVERSIDAD, TIPO_GESTION) %>%
                summarize(CANTIDAD_CARNES = sum(Cant_Carnes)) %>% filter(CANTIDAD_CARNES > 10000)
            grafico <- ggplot(query2, aes(y=NOMBRE_UNIVERSIDAD, x=CANTIDAD_CARNES, fill=NOMBRE_UNIVERSIDAD)) + theme_minimal() + 
                geom_bar(stat='identity', width = 0.2, show.legend = FALSE) + 
                labs(y='Universidades', x='Numero de estudiantes', title = 'Numero de estudiantes por universidades con mas de 10000')
            return(grafico)
        }
            else { if(box == "3") {
                output$grafico2 <- renderText({
                    " # Periodo de licenciamiento segun tipo de gestion
                            aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
                            grafico8 <- ggplot(aux3, aes(y=,x=suma_lic, fill=TIPO_GESTION)) + 
                            geom_bar(stat=identity, color=white) + coord_polar(theta=x)
                            labs(y=Publico vs Privado,x=Numero de años totales en licenciamiento, 
                            title=Comparativa entre numero de licenciamientos entre Público y Privado)
                    # Interpretacion: Comparativa entre las licenciaciones publicas vs las privadas. Mas licenciadas hay publicas que privadas, auque la diferencia
                    # es minima"
                    
                })
                output$texto2 <- renderText({
                    "Interpretacion: Comparativa entre las licenciaciones publicas vs las privadas. Mas licenciadas hay publicas que privadas, auque la diferencia
                 es minima"
                })
                
                aux3 <- licenciamiento %>% group_by(TIPO_GESTION) %>% summarise(suma_lic=sum(PERIODO_LICENCIAMIENTO))
                grafico8 <- ggplot(aux3, aes(y='',x=suma_lic, fill=TIPO_GESTION)) + 
                    geom_bar(stat="identity", color='white') + coord_polar(theta='x')
                labs(y='Publico vs Privado',x='Numero de años totales en licenciamiento', 
                     title='Comparativa entre numero de licenciamientos entre Público y Privado')
                return(grafico8)
            }
                else { if(box == "4")  {
                    output$grafico2 <- renderText({
                        "  # Cantidad de programas por Universidad
                             aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
                             grafico7 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                             geom_bar(stat=identity,width = 0.8,show.legend = FALSE) + 
                             labs(y=Universidades,x=Programas, title=Numero de Programas por Universidad)
                        # Interpretacion: Muestra la cantidad de programas/calses para cada universidad (todas licenciada). Se puede ver como La Cayetano es la Universidad
                        # que mas programas tiene, y la que menos tiene es la Unviersidad Jaime Bausate y Meza"
                        
                    })
                    output$texto2 <- renderText({
                        "Interpretacion: Muestra la cantidad de programas/calses para cada universidad (todas licenciada). Se puede ver como La Cayetano es la Universidad
                         que mas programas tiene, y la que menos tiene es la Unviersidad Jaime Bausate y Meza"
                    })
                    
                    aux2 <- resumen_sunedu %>% group_by(NOMBRE) %>% summarise(suma_programas = sum(PROGRAMAS_TOTAL))
                    grafico7 <- ggplot(aux2, aes(y=NOMBRE, x=suma_programas, fill=NOMBRE)) + theme_minimal()+
                        geom_bar(stat="identity",width = 0.8,show.legend = FALSE) + 
                        labs(y='Universidades',x='Programas', title='Numero de Programas por Universidad')
                    return(grafico7)
                } 
                    else { if(box=="5")  {
                        output$grafico2 <- renderText({
                            "  # Grafico de pie de licenciamiento de universidades
                               grafico <- ggplot(licenciamiento, aes(x=, y=ESTADO_LICENCIAMIENTO, fill=ESTADO_LICENCIAMIENTO)) + 
                        geom_bar(stat=identity, color=white) + coord_polar(theta=y)"
                            
                        })
                        output$texto2 <- renderText({
                            "Interpretacion: Se puede ver como hay un numero conciderable de universidades licenciadas. Se puede ver como mas del
                             60% son solo universidades Licenciadas."
                        })
                        
                        grafico2 <- ggplot(licenciamiento, aes(x='', y=ESTADO_LICENCIAMIENTO, fill=ESTADO_LICENCIAMIENTO)) + 
                            geom_bar(stat='identity', color='white') + coord_polar(theta='y')
                        return(grafico2)
                    }
                        
                    } } } } }
    )
    #modelo3
    output$modelo3 <- renderText({
        'plot(vinos$free.sulfur.dioxide, vinos$sulphates)
        df <- data.frame(vinos$free.sulfur.dioxide, vinos$sulphates)
        kmeans <- kmeans(df, 7)
        plot(df, col = kmeans$cluster)
        points(kmeans$centers, col = 1:2, pch = 8, cex = 2)'
        
          })
    
    output$plot3<-renderPlot({
        plot(vinos$free.sulfur.dioxide, vinos$sulphates)
        df <- data.frame(vinos$free.sulfur.dioxide, vinos$sulphates)
        kmeans <- kmeans(df, 7)
        plot(df, col = kmeans$cluster)
        points(kmeans$centers, col = 1:2, pch = 8, cex = 2)
        
        
    })
    
    output$texto3 <- renderText({
        '#Interpretacion: Se puede ver como cuando tenemos 7 cluser, en la funcion KMEANS, automaticamente se visualizan los 7 cluster. Ademas, se puede ver como los cluster
        no estan en la misma posicion, si no que estan de acuerdo al promedio de las distancias de todos los puntos, dentro de ese grupo o categoria.
        Para cambiar los valores y los cluster, solo modifique los x e y del dataframe y el numero en la funcion kmeans'
        
    })
    
    #modelo4
    output$modelo4 <- renderText({
        '# Regresion lineal (en el shiny las variables las pone el usuario)
            R=lm(vinos$fixed.acidity~vinos$density)
            
        #plotear los puntos
            plot(vinos$density, vinos$fixed.acidity)
            # Te muestra la linea de regresion
            abline(R, col="red", lwd=2)'
        
    })
    
    output$plot4<-renderPlot({
        
        # Regresion lineal (en el shiny las variables las pone el usuario)
        R=lm(vinos$fixed.acidity~vinos$density)
        
        #plotear los puntos
        plot(vinos$density, vinos$fixed.acidity)
        # Te muestra la linea de regresion
        abline(R, col="red", lwd=2)
        
    })
    
    output$texto4 <- renderText({
        '# Interpretacion: Usamos la funcion de lm() para saber la regresion lineal de dos variables (he ahi "lineal") luego ploteamos las variables solas y
        por ultimo dibujando la linea de la regresion usando la funcion creada anteriormente llamada R  '
        
    })
    
    #modelo5
    output$modelo5 <- renderText({
        '# para ver la regresion del fixed.acidity en relacion a las otras 3 variables
        # citric.acid, density, pH
        
        ids <- sample(1:nrow(vinos),size=nrow(vinos)*0.7,replace = FALSE)
        entrenamiento <- vinos[ids, c(1,3,8,9)] #Se coge las columnas que se evaluaran (70%)
        probar <- vinos[-ids, c(1,3,8,9)] #Se escogen las columnas que se evaluaran (30%)
        ft = lm(fixed.acidity~ citric.acid + density + pH, data=entrenamiento)
        
        # predecir
        predict(ft, probar)
        probar$prediccion <- predict(ft, probar)
        probar
        
        # Determinar la precision del modelo entrenado (porcentaje)
        error <- mean(abs(100((probar$prediccion - probar$fixed.acidity)/ probar$prediccion)))
        
        accuracy <- 100 - error
        accuracy'
                
    })
    
    output$plot5 <- DT::renderDataTable({
        
        regresionPolinomial
        
    })
    
    output$texto5 <- renderText({
        'Interpretacion: Primero, tenemos que dividir en dos nuestro dataset unos con el que se entranará (70% del dataset) y los demás resptantes
se usarán para probar o testear. Se tiene que seleccionar las columnas que uno quiere usar para predecir. Se usó también la función por 
defecto de R lm(). Además también se uso la función predict() para predecir con respecto a las demas variables. Por ultimo, sacamos el error
para saber cual es el accuracy. En nuestro caso es 92%, siendo un buen resultado de predicción.'
        
    })
    
    #modelo6
    output$modelo6 <- renderText({
        '# normalizacion de los datos: estandarizacion(variables - promedio)/desv
vinosPCA <- scale(vinos)
pca <- prcomp(vinosPCA)
str(pca)
pca[[1]] # desviaciones
pca[[2]] # rotaciones
pca[[5]] # individuos

# Dependiendo de cuantas componentes se escribe abajo 
componentes <- cbind(pca[[2]][,1],pca[[2]][,2],pca[[2]][,3], pca[[2]][,4])
individuos <- pca[[5]][,c(1:4)]

# analisis de cluster del componente c1 y c2
s.corcircle(componentes[,c(1,2)]) #Todos los componentes de la col 1 y 2 
s.corcircle(componentes[,c(1,3)])
s.corcircle(componentes[,c(1,4)])
s.corcircle(componentes[,c(1,1)])'
        
    })
    
    output$plot6<-renderPlot({
        vinosPCA <- scale(vinos)
        pca <- prcomp(vinosPCA)
        str(pca)
        pca[[1]] # desviaciones
        pca[[2]] # rotaciones
        pca[[5]] # individuos
        
        # Dependiendo de cuantas componentes se escribe abajo 
        componentes <- cbind(pca[[2]][,1],pca[[2]][,2],pca[[2]][,3], pca[[2]][,4])
        individuos <- pca[[5]][,c(1:4)]
        
        #install.packages("ade4") 
        # analisis de cluster del componente c1 y c2
        s.corcircle(componentes[,c(1,2)]) #Todos los componentes de la col 1 y 2 

    })
    
    output$texto6 <- renderText({
        
        'Interpretacon: PCA o "Principal Component Analysis", en nuestro caso, en las componentes[,c(1,2)] se puede ver como hay 4 grupos, 3 de ellos
         bien marcados:
         Primer grupo: pH (Oeste)
         Segundo grupo: alcohol, quality (Sur)
         Tercer grupo: sulphates, citric.acid, fixed.acidity (Este)
         Cuarto grupo: Volatile.acidity, total.sulfur.dioxide, residual.sulfur (Norte)
         El grupo menos relacionado es el cuarto, donde no se ve muy bien y no se marca, ya que cubre mucho espacio y hay mucha diferencia entre si
         El grupo que esta mas relacionado entre si es el Segundo y tercero, ya que se ven que son los mas cercanos entre si, ademas de separados del
         resto. '
        
    })
    
    #modelo7
    output$modelo7 <- renderText({'
            x <- vinos$citric.acid 
            y <- vinos$density 
            
            dataframe = data.frame(x, y)
            
            etiquetar <- function(dataframe) {
                 grupos <- c()
                 for (i in 1:NROW(dataframe)) {
                      if(dataframe$x[i]>=min(dataframe$x) & dataframe$x[i]<(max(dataframe$x)*0.4)) {
                           grupos <- c(grupos,A)
                      }
                      else if(dataframe$x[i]>=(max(dataframe$x)*0.4) & dataframe$x[i]<(max(dataframe$x)*0.6)) {
                           grupos <- c(grupos, B)
                      }
                      else grupos <- c(grupos, C)
                 }
                 dataframe <- cbind(dataframe, grupos)
                 return(dataframe)
            }
            dataframe = etiquetar(dataframe)
            head(dataframe)
            
            ggplot(data = dataframe,aes(x=dataframe$x,y=dataframe$y,color=dataframe$grupos))+
                 geom_point()+xlab(X)+ylab(Y)+ggtitle(Clasificador KNN)
            
            # sacar el 70% y el 30% para entrenamiento y testeo respectivamente
            ids=sample(1:nrow(dataframe),size=nrow(dataframe)*0.7,replace = FALSE)
            
            Entrenamiento<-dataframe[ids,]
            Test<-dataframe[-ids,]
            
            ggplot(data = Entrenamiento ,aes(x=x,y=y,color=grupos))+
                 geom_point()+xlab(X)+ylab(Y)+ggtitle(Clasificador KNN)
            
            dataframe.temporal = dataframe
            
            knn <- function(dataframe.temporal, nuevoX, nuevoY, k, metodo) {
                 if (metodo == 1) {
                      d <- (abs(nuevoX-dataframe.temporal$x)-abs(nuevoY-dataframe.temporal$y))
                 } else {
                      d <- sqrt((nuevoX-dataframe.temporal$x)^2 + (nuevoY-dataframe.temporal$y)^2)
                 }
                 dataframe.temporal <- cbind(dataframe.temporal, d)
                 vOrden <- sort(dataframe.temporal$d)
                 vecinos <- dataframe.temporal[dataframe.temporal$d %in% vOrden[1:k],3]
                 return (vecinos[1:k])
            }
            v <- knn(dataframe, 7, 13, 1332, 1)
            porc<-function(vector,value) {
                 return (sum(as.integer(vector==value)))
            }
            a<-porc(v,A)
            b<-porc(v,B)
            c<-porc(v,C)
            total<-(a+b+c)
            a*100/total
            b*100/total
            c*100/total'
        
    })
    
    output$plot7<-renderPlot({
        x <- vinos$citric.acid 
        y <- vinos$density    
        
        dataframe = data.frame(x, y)
        
        etiquetar <- function(dataframe) {
            grupos <- c()
            for (i in 1:NROW(dataframe)) {
                if(dataframe$x[i]>=min(dataframe$x) & dataframe$x[i]<(max(dataframe$x)*0.4)) {
                    grupos <- c(grupos,'A')
                }
                else if(dataframe$x[i]>=(max(dataframe$x)*0.4) & dataframe$x[i]<(max(dataframe$x)*0.6)) {
                    grupos <- c(grupos, 'B')
                }
                else grupos <- c(grupos, 'C')
            }
            dataframe <- cbind(dataframe, grupos)
            return(dataframe)
        }
        dataframe = etiquetar(dataframe)
        head(dataframe)
        
        # sacar el 70% y el 30% para entrenamiento y testeo respectivamente
        ids=sample(1:nrow(dataframe),size=nrow(dataframe)*0.7,replace = FALSE)
        
        Entrenamiento<-dataframe[ids,]
        Test<-dataframe[-ids,]
        
        entrenamiento <- ggplot(data = Entrenamiento ,aes(x=x,y=y,color=grupos))+
            geom_point()+xlab("X")+ylab("Y")+ggtitle("Clasificador KNN")
        return(entrenamiento)
        
        dataframe.temporal = dataframe
        
        knn <- function(dataframe.temporal, nuevoX, nuevoY, k, metodo) {
            if (metodo == 1) {
                d <- (abs(nuevoX-dataframe.temporal$x)-abs(nuevoY-dataframe.temporal$y))
            } else {
                d <- sqrt((nuevoX-dataframe.temporal$x)^2 + (nuevoY-dataframe.temporal$y)^2)
            }
            dataframe.temporal <- cbind(dataframe.temporal, d)
            vOrden <- sort(dataframe.temporal$d)
            vecinos <- dataframe.temporal[dataframe.temporal$d %in% vOrden[1:k],3]
            return (vecinos[1:k])
        }
        v <- knn(dataframe, 7, 13, 1332, 1)
        porc<-function(vector,value) {
            return (sum(as.integer(vector==value)))
        }
        a<-porc(v,"A")
        b<-porc(v,"B")
        c<-porc(v,"C")
        total<-(a+b+c)
        a*100/total
        b*100/total
        c*100/total
        
    })
    
    output$texto7 <- renderText({
        '#Interpretacion: Se puede ver como cuando tenemos 7 cluser, en la funcion KMEANS, automaticamente se visualizan los 7 cluster. Ademas, se puede ver como los cluster
        no estan en la misma posicion, si no que estan de acuerdo al promedio de las distancias de todos los puntos, dentro de ese grupo o categoria.
        Para cambiar los valores y los cluster, solo modifique los x e y del dataframe y el numero en la funcion kmeans'
        
    })
    
    
    
    ################## Consultas ###################################################### 
    ### Estadisticas Programas
    output$textoEstaPrograma1 <- renderText({
        '# Media, Maximo, Minimo, Cuartil y Percentil de los Programas Universitarios en total de cada universidad'
    })
    output$textoEstaPrograma2 <- renderText({
        'estadistica_programas <- resumen_sunedu %>% summarise(Cantidad = n(),
			Media = mean(PROGRAMAS_TOTAL, na.rm = TRUE),
			Maximo = max(PROGRAMAS_TOTAL, na.rm = TRUE),
			Minimo = min(PROGRAMAS_TOTAL, na.rm = TRUE),
			Q25 = quantile(PROGRAMAS_TOTAL, .25, na.rm = TRUE),
			Q50 = quantile(PROGRAMAS_TOTAL, .50, na.rm = TRUE),
			Q75 = quantile(PROGRAMAS_TOTAL, .75, na.rm = TRUE),)'
    })
    output$tablaEstaPrograma <- DT::renderDataTable({ estadisticaProgramasTabla })
    
    
    ### Estadisticas Carnes
    output$textoEstaCarne1 <- renderText({
        '# Media, Maximo, Minimo, Cuartil y Percentil de los Carnes Universitarios en total de cada universidad'
    })
    output$textoEstaCarne2 <- renderText({
        'estadistica_carnes <- resumen_sunedu %>% summarise(Cantidad = n(),
			Media = mean(CANTIDAD_CARNES, na.rm = TRUE),
			Maximo = max(CANTIDAD_CARNES, na.rm = TRUE),
			Minimo = min(CANTIDAD_CARNES, na.rm = TRUE),
			Q25 = quantile(CANTIDAD_CARNES, .25, na.rm = TRUE),
			Q50 = quantile(CANTIDAD_CARNES, .50, na.rm = TRUE),
			Q75 = quantile(CANTIDAD_CARNES, .75, na.rm = TRUE),)
estadistica_carnes'
    })
    output$tablaEstaCarne <- DT::renderDataTable({ estadisticaCarnesTabla })
    
    
    ### Query 1
    output$query1Texto1 <- renderText({
        '1. Lista de universidades que tienen programas de Doctorado que se encuentren
        licenciadas segun la lista de licenciamiento de Abril del 2020. Mostrar tambien
        la cantidad de programas y alumnos que tiene cada una respectivamente.'
    })
    
    output$query1Texto2 <- renderText({
        'query <- programas %>%
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
					select(NOMBRE, NIVEL_ACADEMICO, ESTADO_LICENCIAMIENTO, PROGRAMAS, ALUMNOS = CANTIDAD_CARNES)'
    })
    output$tablaQuery1 <- DT::renderDataTable({ query1 })
    
    
    ### Query 4
    output$query4Texto1 <- renderText({
        '# 4. Cantidad de universidades publicas que tienen licenciatura fuera de lima
         #		y su cantidad respectiva de estudiantes que no sean de ingenieria ordenado de
         #		mayor a menor cantidad de estudiantes'
    })
    output$query4Texto2 <- renderText({
        'query <- licenciamiento %>% 
						filter(TIPO_GESTION == "PUBLICO", 
								ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA",
								DEPARTAMENTO_LOCAL != "LIMA") %>%
						select(NOMBRE,TIPO_GESTION,ESTADO_LICENCIAMIENTO,DEPARTAMENTO_LOCAL) %>%
						inner_join(carnes %>%
										filter(NOMBRE_CLASE_PROGRAMA != "SALUD")%>%
										group_by(NOMBRE_UNIVERSIDAD, ) %>%
										summarize(ESTUDIANTES = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						arrange(desc(ESTUDIANTES))'
    })
    output$tablaQuery4 <- DT::renderDataTable({ query4 })
    
    
    ### Query 10
    output$query10Texto1 <- renderText({
        '# 10. Universidades privadas que tienen licenciamiento segun la lista de Abril 2020 y que tienen mas del
        #		promedio de programas academicos. Mostrar el estado de licenciamiento y cantidad de programas
        #		academicos. Tambien, ordenar por mayor cantidad de programas y como segunda prioridad alfabeticamente.'
    })
    output$query10Texto2 <- renderText({
        'query <- licenciamiento %>%
						filter(TIPO_GESTION == "PRIVADO",
							ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
						select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
						inner_join(programas %>%
											group_by(NOMBRE) %>%
											summarize(PROGRAMAS = n()) %>%
											filter(PROGRAMAS > mean(PROGRAMAS)),
									by=c("NOMBRE"="NOMBRE")) %>%
						arrange(desc(PROGRAMAS), NOMBRE)'
    })
    output$tablaQuery10 <- DT::renderDataTable({ query10 })
    
    ### Query 13
    output$query13Texto1 <- renderText({
        '# 13. Cantidad de estudiantes de universidades privadas en ICA'
    })
    output$query13Texto2 <- renderText({
        'query <- licenciamiento %>%
						filter(DEPARTAMENTO_LOCAL=="ICA",TIPO_GESTION=="PRIVADO") %>% 
						select(NOMBRE) %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(CANTIDAD_CARNES = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						select(CANTIDAD_ALUMNOS = CANTIDAD_CARNES)'
    })
    output$tablaQuery13 <- DT::renderDataTable({ query13 })
    
    ### Query 18
    output$query18Texto1 <- renderText({
        '# 18. Cantidad de alumnos de universidades cuyas licencias fueron revocadas'
    })
    output$query18Texto2 <- renderText({
        'query <- licenciamiento %>%
						select(NOMBRE,ESTADO_LICENCIAMIENTO) %>%
						filter(ESTADO_LICENCIAMIENTO == "LICENCIA DENEGADA") %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(CANTIDAD_ALUMNOS = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						summarize(TOTAL = sum(CANTIDAD_ALUMNOS))'
    })
    output$tablaQuery18 <- DT::renderDataTable({ query18 })
    
    ### Query 19
    output$query19Texto1 <- renderText({
        '#19. Departamento que mas universidades con licencia denegada tiene que no sea Lima'
    })
    output$query19Texto2 <- renderText({
        'query <- licenciamiento %>% 
						filter(DEPARTAMENTO_LOCAL != "LIMA", ESTADO_LICENCIAMIENTO!="LICENCIA OTORGADA") %>%
						group_by(DEPARTAMENTO_LOCAL) %>%
						summarize(UNIVERSIDADES = n()) %>%
						filter(UNIVERSIDADES == first(licenciamiento %>% 
												filter(DEPARTAMENTO_LOCAL != "LIMA",
														ESTADO_LICENCIAMIENTO!="LICENCIA OTORGADA") %>%
												group_by(DEPARTAMENTO_LOCAL) %>%
												summarize(UNIVERSIDADES = n()) %>%
												summarize (MAX = max(UNIVERSIDADES))))'
    })
    output$tablaQuery19 <- DT::renderDataTable({ query19 })
    
    ### Query 23
    output$query23Texto1 <- renderText({
        '#23. Lista de universidades licenciadas y la cantidad de sus estudiantes de cada departamento'
    })
    output$query23Texto2 <- renderText({
        'query <- licenciamiento %>%
						filter(ESTADO_LICENCIAMIENTO == "LICENCIA OTORGADA") %>%
						select(NOMBRE,DEPARTAMENTO_LOCAL,ESTADO_LICENCIAMIENTO) %>%
						inner_join(carnes %>%
											group_by(NOMBRE_UNIVERSIDAD) %>%
											summarize(ESTUDIANTES = sum(Cant_Carnes)),
									by=c("NOMBRE"="NOMBRE_UNIVERSIDAD")) %>%
						arrange(DEPARTAMENTO_LOCAL)'
    })
    output$tablaQuery23 <- DT::renderDataTable({ query23 })
    
    ### Query 25
    output$query25Texto1 <- renderText({
        '#25. Funcion para saber cuantos estudiantes en total tiene una universidad'
    })
    output$query25Texto2 <- renderText({
        'alumnosTotal <-function(nombre){
	query <- carnes %>%
					filter(NOMBRE_UNIVERSIDAD == nombre) %>%
					select(NOMBRE_UNIVERSIDAD, Cant_Carnes) %>%
					group_by(NOMBRE_UNIVERSIDAD) %>%
					summarize(n = sum(Cant_Carnes))
	return(query)
}
query <- alumnosTotal("UNIVERSIDAD ALAS PERUANAS")
query'
    })
    output$tablaQuery25 <- DT::renderDataTable({ query25 })
    
    ### Query 29
    output$query29Texto1 <- renderText({
        '#29. Cuantas universidades que se encuentran fuera de LIMA cuentan con programas postgrado, ordernar de manera descendiente'
    })
    output$query29Texto2 <- renderText({
        'query <- programas %>%
					select(NOMBRE, DEPARTAMENTO_LOCAL, TIPO_NIVEL_ACADEMICO) %>% 
					filter(DEPARTAMENTO_LOCAL != "LIMA", TIPO_NIVEL_ACADEMICO == "POSGRADO") %>%
					group_by(NOMBRE) %>% summarize(PROGRAMAS_TOTAL = n()) %>%
					arrange(desc(PROGRAMAS_TOTAL))'
    })
    output$tablaQuery29 <- DT::renderDataTable({ query29 })
    
    ### Query 30
    output$query30Texto1 <- renderText({
        '# 30. Obtener la cantidad de programas academicos en cada tipo de nivel academico de las universidades,
        #		ademas de tambien ordenarlo a ascendentemente segun el nombre y descendentemente la cantidad
        #		de programas como segunda prioridad'
    })
    output$query30Texto2 <- renderText({
        'query <- programas %>%
					select(NOMBRE,NIVEL_ACADEMICO) %>%
					group_by(NOMBRE,NIVEL_ACADEMICO) %>%
					summarize(PROGRAMAS = n()) %>%
					arrange(NOMBRE,desc(PROGRAMAS))'
    })
    output$tablaQuery30 <- DT::renderDataTable({ query30 })
    
    
    

}


shinyApp(ui = ui, server = server)
