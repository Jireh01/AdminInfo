library(DT)
consultas<-{
	navlistPanel( 
		tabPanel("Estadistica Programas",h4("Estadistica Descriptiva de los Programas Universitarios"),hr(),
				 verbatimTextOutput("textoEstaPrograma1"),DT::dataTableOutput('tablaEstaPrograma'), 
				 verbatimTextOutput("textoEstaPrograma2")),
		
		tabPanel("Estadistica Carnes",h4("Estadistica Descriptiva de los Carnes Universitarios"),hr(),
				 verbatimTextOutput("textoEstaCarne1"),DT::dataTableOutput("tablaEstaCarne"),
				 verbatimTextOutput("textoEstaCarne2")),
		
		tabPanel("Query 1",h4("Query 1"),hr(),
				 verbatimTextOutput("query1Texto1"),DT::dataTableOutput("tablaQuery1"),
				 verbatimTextOutput("query1Texto2")),
		
		tabPanel("Query 4",h4("Query 4"),hr(),
				 verbatimTextOutput("query4Texto1"),DT::dataTableOutput("tablaQuery4"),
				 verbatimTextOutput("query4Texto2")),
		
		tabPanel("Query 10",h4("Query 10"),hr(),
				 verbatimTextOutput("query10Texto1"),DT::dataTableOutput("tablaQuery10"),
				 verbatimTextOutput("query10Texto2")),
		
		tabPanel("Query 13",h4("Query 13"),hr(),
				 verbatimTextOutput("query13Texto1"),DT::dataTableOutput("tablaQuery13"),
				 verbatimTextOutput("query13Texto2")),

		tabPanel("Query 18",h4("Query 18"),hr(),
				 verbatimTextOutput("query18Texto1"),DT::dataTableOutput("tablaQuery18"),
				 verbatimTextOutput("query18Texto2")),
		
		tabPanel("Query 19",h4("Query 19"),hr(),
				 verbatimTextOutput("query19Texto1"),DT::dataTableOutput("tablaQuery19"),
				 verbatimTextOutput("query19Texto2")),
		
		tabPanel("Query 23",h4("Query 23"),hr(),
				 verbatimTextOutput("query23Texto1"),DT::dataTableOutput("tablaQuery23"),
				 verbatimTextOutput("query23Texto2")),
		
		tabPanel("Query 25",h4("Query 25"),hr(),
				 verbatimTextOutput("query25Texto1"),DT::dataTableOutput("tablaQuery25"),
				 verbatimTextOutput("query25Texto2")),
		
		tabPanel("Query 29",h4("Query 29"),hr(),
				 verbatimTextOutput("query29Texto1"),DT::dataTableOutput("tablaQuery29"),
				 verbatimTextOutput("query29Texto2")),
		
		tabPanel("Query 30",h4("Query 30"),hr(), 
				 verbatimTextOutput("query30Texto1"),DT::dataTableOutput("tablaQuery30"),
				 verbatimTextOutput("query30Texto1"))
		)
	
}