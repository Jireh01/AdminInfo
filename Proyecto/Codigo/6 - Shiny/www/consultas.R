consultas<-{
  seleccion = c('Query 1'='1','Query 2'='2','Query 3'='3','Query 4'='4','Query 5'='5','Query 6'='6','Query 7'='7','Query 8'='8','Query 9'='9'
                ,'Query 10'='10')
    tabPanel("Querys",h4("QUERYS"),hr(),
             selectInput("q1", label = NULL, seleccion),hr(),
             verbatimTextOutput("query1"),tableOutput('table'), 
             verbatimTextOutput("texto3"))
}