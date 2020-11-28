modelo<-{
  navlistPanel( 
    tabPanel("KMEANS",h4("KMEANS"),hr(),
             verbatimTextOutput("modelo3"),plotOutput('plot3'), 
             verbatimTextOutput("texto3")),
    
    tabPanel("REGRESION LINEAL",h4("REGRESION LINEAL"),hr(),
             verbatimTextOutput("modelo4"),plotOutput("plot4"),
             verbatimTextOutput("texto4")),
    
    tabPanel("REGRESION POLINOMIAL",h4("REGRESION POLINOMIAL"),hr(),
             verbatimTextOutput("modelo5"),dataTableOutput("plot5"),
             verbatimTextOutput("texto5")),
    
    tabPanel("PCA",h4("PCA"),hr(),
             verbatimTextOutput("modelo6"),plotOutput("plot6"),
             verbatimTextOutput("texto6")),
    
    tabPanel("KNN",h4("KNN"),hr(),
             verbatimTextOutput("modelo7"),plotOutput("plot7"),
             verbatimTextOutput("texto7")))
}