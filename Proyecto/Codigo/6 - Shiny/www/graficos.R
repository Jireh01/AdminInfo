graficos<-{
  seleccion = c('Grafico 1'='1','Grafico 2'='2','Grafico 3'='3','Grafico 4'='4','Grafico 5'='5')
  navlistPanel( 
    tabPanel("Graficos del 1 al 5",h4("GRAFICO"),hr(),
             selectInput("opcion1", label = NULL, seleccion),hr(),
             verbatimTextOutput("grafico1"),plotOutput('plot1'), 
             verbatimTextOutput("texto1")),
    
    tabPanel("Graficos del 5 al 10",h4("GRAFICO"),hr(),
              selectInput("opcion2", label = NULL, seleccion ),hr(),
             verbatimTextOutput("grafico2"),plotOutput("plot2"),
             verbatimTextOutput("texto2")))
  
}