modelo<-{
  navlistPanel( 
    tabPanel("KMEANS",h4("KMEANS"),hr(),
             verbatimTextOutput("modelo1"),plotOutput('plot3'), 
             verbatimTextOutput("texto1")),
    
    tabPanel("REGRESION LINEAL",h4("REGRESION LINEAL"),hr(),
             verbatimTextOutput("modelo2"),plotOutput("plot4"),
             verbatimTextOutput("texto2")),
    
    tabPanel("REGRESION POLINOMIAL",h4("REGRESION POLINOMIAL"),hr(),
             verbatimTextOutput("modelo3"),plotOutput("plot5"),
             verbatimTextOutput("texto3")),
    
    tabPanel("PCA",h4("PCA"),hr(),
             verbatimTextOutput("modelo4"),plotOutput("plot6"),
             verbatimTextOutput("texto4")),
    
    tabPanel("KNN",h4("KNN"),hr(),
             verbatimTextOutput("modelo5"),plotOutput("plot7"),
             verbatimTextOutput("texto5")))
}