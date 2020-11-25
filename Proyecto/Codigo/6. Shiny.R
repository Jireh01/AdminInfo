library("ggplot2")
library("shinyjs")#para la funcion toggle
library("shiny")
library("readxl")#para lectura de excel
library("dplyr")
library("lubridate")#para fechas
library("sqldf")
library("plotly")#para graficos dinamicos
library("shinythemes")

source("Proyecto/Codigo/www/introduccion.R")
source("Proyecto/Codigo/www/informe.R")

ui <- fluidPage(
  theme = shinytheme("slate"),
  navbarPage( "Trabajo Final de Administración de la Información",
    tabPanel("Presentacion",introduccion),
    tabPanel("Recolecccion", 
             sidebarLayout(
               sidebarPanel(
                 h3("Panel lateral"),
                 fileInput(inputId = "idArchivo",label="Seleccione archivo")
               ),
               mainPanel(
                 h3("Panel principal"),
                 tableOutput(outputId = "idTabla1")
                 
               )
             )
    )

    
            ,tabPanel("Preprocesamiento",
                        navlistPanel(
                          tabPanel("Quitar tildes",
                                 h4("Quitar tildes"),verbatimTextOutput("tildes")),
                          
                          tabPanel("Remover los espacios en blanco con N/A",
                                   h4("Reemplazar espacios por N/A"),
                                   verbatimTextOutput("blanco")),
                          tabPanel("Uniformizar nombres",
                                   h4("Uniformizar Nombres"),
                                   verbatimTextOutput("nombres")),
                          tabPanel("Remover columnas innecesarias",
                                   h4("Remover columnas innecesarias"),verbatimTextOutput("columnas")),
                          tabPanel("Convertir string a entero",
                                   h4("Convertir string a entero"),
                                   verbatimTextOutput("consulta2"))
                        )
            )
  
              ,tabPanel('Consultas Exploracion',
                        navlistPanel(
                          tabPanel("Consultas DPLYR",
                      h4("DPLYR"), hr(),selectInput("selectdp", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),checkboxInput("control2", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta2"),dataTableOutput("tablaS3"))
                      , tabPanel("Consultas con SQLDF", h4("SQLDF"),hr(),selectInput("selectsq", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),checkboxInput("control3", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta3"),dataTableOutput("tablaS4"))
                      
                      )
                      )
            
            ,tabPanel('Graficos',
                      navlistPanel( 
                        tabPanel("Graficos GGPLOT2",h4("GGPLOT2"),hr(),selectInput("selectgg", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),checkboxInput("control4", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta4"),plotOutput('plot1'))
                        ,tabPanel("Graficos PLOTY",h4("PLOTY"),hr(),
                      selectInput("selectpl", label = NULL, choices = c('consulta 1'='1','consulta 2'='2','consulta 3'='3','consulta 4'='4','consulta 5'='5')),checkboxInput("control5", "Mostrar Codigo", FALSE),hr(),verbatimTextOutput("consulta5"),plotlyOutput("plot2"))))
            
            ,tabPanel('Modelo',
                      navlistPanel(  
                      tabPanel("Ayuda vs Estimacion de Daño",hr(),checkboxInput("control6", "Mostrar Codigo", FALSE),verbatimTextOutput("consulta6"),hr(), plotOutput("plot8"))  ) )
    ,tabPanel('Informe', informe)    )
          
  
    )
  

  

options(shiny.maxRequestSize=30*1024^2) 
server <- function(input, output) {
  getwd()

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
  
  output$blanco <- renderText({ '
   # Reemplazar los espacios en blanco de los dataFrames por NA
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
  output$entero <- renderText({ '
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
  
  
  output$consulta2 <- renderText({
    
" colocar el codigo de la consulta por ejemplo:
dplyr1<- dtFinal%>%filter(DesFenome=='INCENDIO URBANO'& grepl('DOMESTICO', DesEmerge))%>% select(CodEmerge:DesFenome,MesEmergencia)%>% arrange(MesEmergencia)
 "
    
  })
  
  output$tablaS3 <- renderDataTable({
    box<- input$selectdp
    if (is.null(box))
    {return(NULL)}
  
    if(box == "1")    {
      dplyr1<- dtFinal%>%filter(DesFenome=='INCENDIO URBANO'& grepl('DOMESTICO', DesEmerge))%>% select(CodEmerge:DesFenome,MesEmergencia)%>% arrange(MesEmergencia)
      return (dplyr1)
    } else{  if(box == "2") {
      dplyr2<- dtFinal%>% group_by(DesDpto)%>% summarise(cantidad=n()) 
      return (dplyr2)
    } else { if(box == "3") {
      dplyr3<- dtFinal%>%filter(grepl('DEFENSA CIVIL', DesFuente) & DiaRegistro >= 20)%>% select(matches('?Des'))%>%slice(1000:1500)
      return (dplyr3)
    } else { if(box == "4")
    { dplyr4 <- dtFinal%>%filter(CodUsuari %in% c('CRDCPASCO03', 'CRDCPASCO04','CRDCLALIBERTAD02'))%>% select( contains('emer'),CodUsuari)%>% arrange(desc(DiaEmergencia),MesEmergencia)
    return (dplyr4)
    } else { if(box=="5")
    { dplyr5 <- dtFinal%>%filter(AnioEmergencia <= 2016,grepl('GEODINAMICA', DesGrupo))%>% select( contains('Grupo'))%>% group_by(DesGrupo)%>%summarise(cantidad = n())
    return (dplyr5)
    } 
    
    } }
    } } }
  )
  
  output$consulta3 <- renderText({
    
"colocar el codigo de la consulta por ejemplo:
sqldf1<-sqldf('SELECT DiaEmergencia, avg(AHumanitarFam) as avg_ahf FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ahf DESC;')

"
    
  })
  
  output$tablaS4 <- renderDataTable({
    box<- input$selectsq
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      sqldf1<-sqldf('SELECT DiaEmergencia, avg(AHumanitarFam) as avg_ahf FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ahf DESC;')
      return (sqldf1)
    } else{  if(box == "2") {
      sqldf2<-sqldf('SELECT DiaEmergencia, avg(EdanosVivienda)as avg_ev FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ev ASC;')
      return (sqldf2)
    } else { if(box == "3") {
      sqldf1<-sqldf('SELECT DiaEmergencia, avg(AHumanitarFam) as avg_ahf FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ahf DESC;')
      sqldf2<-sqldf('SELECT DiaEmergencia, avg(EdanosVivienda)as avg_ev FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ev ASC;')
      sqldf3<- sqldf('SELECT  *    FROM sqldf1 d INNER JOIN sqldf2 b ON d.DiaEmergencia = b.DiaEmergencia WHERE d.avg_ahf  between 3 AND 4 ;')
      return (sqldf3)
    } else { if(box == "4")
    { sqldf4<- sqldf('select DesDist,Desfenome,count(*) as cantidad from dtFinal WHERE Desfenome like "%ACT%" GROUP BY Desfenome HAVING cantidad = max((select count(*) from dtFinal GROUP BY Desfenome )) ')
      return (sqldf4)
    } else { if(box=="5")
    { sqldf5<- sqldf('select  DesDist,DiaEmergencia, DesGrupo,sum(EdanosVivienda) as SumaEV from dtFinal GROUP BY DesGrupo HAVING SumaEV = 1 or (SumaEV != 0 and SumaEV between 2 and 10000 )  order by DiaEmergencia  DESC ')
      return (sqldf5)
    }  
    }
  }}}})
  
  

  
  output$consulta4 <- renderText({
  "  colocar el codigo de la consulta por ejemplo:
  gr1<- dtFinal  %>% filter(MesEmergencia<5)%>% group_by(DesGrupo)
    gp1 <- ggplot(gr1, aes(x=CodGrupo)) + geom_bar( width=0.5, colour='blue', fill='gray')+ facet_grid(MesEmergencia ~.)
    
    "
  })
  
  output$plot1 <- renderPlot({
    box<- input$selectgg
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      gr1<- dtFinal  %>% filter(MesEmergencia<5)%>% group_by(DesGrupo)
      gp1 <- ggplot(gr1, aes(x=CodGrupo)) + geom_bar( width=0.5, colour='blue', fill='gray')+ facet_grid(MesEmergencia ~.)
      return (gp1)
    } else{  if(box == "2") {
      gr2<- dtFinal  %>% filter(grepl('GEODINAMICA', DesGrupo))
      gp2 <- ggplot(gr2, aes(x=AnioEmergencia)) + geom_bar( width=0.5, colour='blue', fill='gray')+ facet_grid(~DesGrupo )
      return (gp2)
    } else { if(box == "3") {
      gr3<-dtFinal %>% filter(DesGrupo == 'TECNOLOGICOS')
      gp3<- ggplot(gr3, aes(x=DesFenome)) + geom_bar( width=0.5, colour='blue', fill='gray')+
        coord_flip() + facet_wrap(~ AnioEmergencia)
      return (gp3)
    } else { if(box == "4")
    { gp4<-ggplot(dtFinal, aes(MesEmergencia)) + geom_density()+geom_line(stat='density')
    return (gp4)
    } else { if(box=="5")
    { gr5<- dtFinal%>%filter(MesEmergencia >= 3 & MesEmergencia <= 8)%>% select( contains('Grupo'))%>% group_by(DesGrupo)%>%summarise(Cantidad = n())
    gp5<- ggplot(gr5, aes(x=1, y=Cantidad,fill=DesGrupo)) + geom_bar(stat='identity', colour='black') + coord_polar(theta='y')
    return (gp5)
    } 
    } }
    } } }
  )
  
  
 
  output$consulta5 <- renderText({
    "colocar el codigo de la consulta por ejemplo:
   
    "
  })
  
  output$plot2 <- renderPlotly({
    box<- input$selectpl
    if (is.null(box))
    {return(NULL)}
    
    if(box == "1")    {
      plo1<- plot_ly(dtFinal, x = ~DesGrupo, y = ~DesFenome,type = 'bar')%>% layout(xaxis=list(title=''),yaxis=list(title=''))
      return (plo1)
    } else{  if(box == "2") {
      dt2<- dtFinal%>% group_by(CodUsuari)%>% summarise(AHumanitarFam=sum(AHumanitarFam), EDanosVivienda =sum(EDanosVivienda))
      plo2<- plot_ly(data = dt2, x = ~EDanosVivienda, y = ~AHumanitarFam, type = 'scatter', mode = 'markers',
                     marker = list(size = 10,color = 'rgba(255, 182, 193, .9)',line = list(color = 'rgba(152, 0, 0, .8)',width = 0.5))) %>% layout(yaxis = list(title= ''),xaxis = list(title= ''))
      return (plo2)
    } else { if(box == "3") {
      plot3<- plot_ly(dtFinal, x = ~DesDpto, y = ~AHumanitarFam, type = 'bar') %>% layout( xaxis = list(title = ''),yaxis = list(title = ''))
      return (plot3)
    } else { if(box == "4")
    { 
      sqldf1<-sqldf('SELECT DiaEmergencia, avg(AHumanitarFam) as avg_ahf FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ahf DESC;')
    sqldf2<-sqldf('SELECT DiaEmergencia, avg(EdanosVivienda)as avg_ev FROM dtFinal  GROUP BY  DiaEmergencia order by avg_ev ASC;')
    sqldf3<- sqldf('SELECT  *    FROM sqldf1 d INNER JOIN sqldf2 b ON d.DiaEmergencia = b.DiaEmergencia WHERE d.avg_ahf  between 3 AND 4 ;')
    
      plot4<- plot_ly() %>%add_bars(x = sqldf3$DiaEmergencia,y = sqldf3$avg_ahf,marker = list(color = 'red'),name='P.Ayuda Humanitaria') %>%
      add_bars( x = sqldf3$DiaEmergencia..3, y = sqldf3$avg_ev,marker = list(color = 'blue'),name ='P.Estimacion de Daños')
    return (plot4)
    } else { if(box=="5")
    { 
      plot5<-plot_ly(dtFinal, labels = ~DesFenome, values = ~AHumanitarFam, type = 'pie',
                     textposition = 'inside',
                     textinfo = 'label+percent',
                     insidetextfont = list(color = '#FFFFFF'),
                     hoverinfo = 'text',
                     marker = list(line = list(color = '#FFFFFF', width = 1)),
                     showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    return (plot5)
    } 
    } }
    } }
    })
  
output$consulta6 <- renderText({
"dtModeloR2<-data.frame(dtFinal%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
dtModeloR2.1<-dtModeloR2%>%select(AHumanitarFam,EDanosVivienda)

regresion2 <- lm(AHumanitarFam  ~  EDanosVivienda, data = dtModeloR2)
summary(regresion2)
    
ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle('Gráfica de Regresion') + xlab('Ayuda Humanitaria por Familia') + ylab('Estimacion de Daños por Vivienda') + geom_smooth(method=lm)
    "
  })

  

  
  
  
  output$tablaS6 <- renderTable({
    
    if(is.null(input$file1)){return ()}
    input$file1
    
  })
  

  
 
  output$plot8 <- renderPlot({
    
    dtModeloR2<-data.frame(dtFinal%>%group_by(DesDpto)%>%summarise(AHumanitarFam=sum(AHumanitarFam),EDanosVivienda=sum(EDanosVivienda)))
    
    return( 
      
      ggplot(dtModeloR2, aes(x=AHumanitarFam, y=EDanosVivienda)) + geom_point() + ggtitle("Gráfica de Regresion") + xlab("Ayuda Humanitaria") + ylab("Estimacion de Daños por Vivienda") + geom_smooth(method=lm)
    )
    
  })
  
  observeEvent(input$control1, {
    if(input$control1==TRUE)
    {toggle("consulta1")} else {toggle("consulta1")}
    
    })
  
  
  
  observeEvent(input$control2, {
    
    if(input$control2==TRUE)
    {toggle("consulta2")} else {toggle("consulta2")}
    
  })
  
  observeEvent(input$control3, {
    
    if(input$control3==TRUE)
    {toggle("consulta3")} else {toggle("consulta3")}
    
  })

  observeEvent(input$control4, {
    
    if(input$control4==TRUE)
    {toggle("consulta4")} else {toggle("consulta4")}
    
  })
  observeEvent(input$control5, {
    
    if(input$control5==TRUE)
    {toggle("consulta5")} else {toggle("consulta5")}
    
  })


  observeEvent(input$control6, {
    
    if(input$control6==TRUE)
    {toggle("consulta6")} else {toggle("consulta6")}
    
  })
  
    
    output$tablaS5 <- renderTable({
      Pro<- read.csv(file = 'datasetFinalPre.csv')
      if (is.null(Pro)) 
        return(NULL)
      else head(Pro)
  })
  

  
    
}

# Run the application 
shinyApp(ui = ui, server = server)
