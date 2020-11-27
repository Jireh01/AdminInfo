library(shiny)
library("ggplot2")
library("shinyjs")#para la funcion toggle
library("shiny")
library("readxl")#para lectura de excel
library("dplyr")
library("lubridate")#para fechas
library("sqldf")
library("plotly")#para graficos dinamicos
library("shinythemes")


source("www/inicio.R")
source("www/recoleccion.R")
source("www/querys.R")
source("www/graficos.R")
source("www/informe.R")
source("www/modelo.R")
source("www/consultas.R")

ui <- fluidPage(
    theme = shinytheme("yeti"),
    navbarPage("Proyecto de Universidades peruanas 2019-2020", 
               tabPanel("Inicio",inicio),
               tabPanel("Recoleccion", recoleccion), 
               tabPanel("Preprocesamiento", querys),
               tabPanel("Consultas", consultas),
               tabPanel("Graficos", graficos),
               tabPanel("Modelo", modelo),
               tabPanel("Informe", informe))
)

server <- function(input, output) {
    vino<-read.csv("winequality-red.csv")
    carnes<-read.csv("carnes.csv")
    licenciamiento<-read.csv("licenciamiento.csv")
    programa<-read.csv("programas.csv")
    getwd()
    
    output$idTabla1 <- renderTable({
        file <- input$idArchivo
        read.csv(file$datapath)
    })
    
    output$tildes <- renderText({ '
        #Retirar tildes
        
            quitarTildes <- function(dataFrame){
            conTilde <- c("Ã", "Ã‰", "Ã", "Ã“", "Ãš", "Ã‘", "Ãœ", "Ã¡", "Ã©", "Ã­", "Ã³", "Ãº", "Ã¼")
            sinTilde <- c("A", "E", "I", "O", "U", "N", "U", "a", "e", "i", "o", "u", "Ã¼")
        
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
                "# Cantidad de universidades por departamento
                          ggplot(licenciamiento,aes(x=DEPARTAMENTO_LOCAL, y=, fill=DEPARTAMENTO_LOCAL))+ geom_bar(stat=identity,color=white)+ coord_polar(theta=x)+
                          labs(y=,x=Departamentos, title=Universidades segun departamento)"
                
            })
            output$texto2 <- renderText({
                "Interpretacion: Muestra un pie con los percentajes de las licencias otorgadas (o denegadas) a las Universidades segun 
                 la SUNEDU. Se puede ver como LIMA es la que más tiene, lo cual tiene sentido al ser la capital"
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
    
    
}


shinyApp(ui = ui, server = server)
