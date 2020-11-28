querys<-{
  
  navlistPanel(
    tabPanel("Eliminacion de tildes",
             h4("Cambiar tildes"),hr(),
             verbatimTextOutput("tildes")),
    tabPanel("Remover los espacios en blanco con N/A",
             h4("Remover Espacios"),hr(),
             verbatimTextOutput("remover")),
    tabPanel("Uniformizar nombres",
             h4("Uniformizar nombres"),hr(),
             verbatimTextOutput("nombres")),
    tabPanel("Remover columnas innecesarias",
             h4("Remover Columnas"),hr(),
             verbatimTextOutput("columnas")),
    tabPanel("Convertir string a entero",
             h4("Convertir string a entero"),hr(),
             verbatimTextOutput("string"))
    
  )
}