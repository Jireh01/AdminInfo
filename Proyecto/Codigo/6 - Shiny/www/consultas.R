
navConsultas <- {
	navlistPanel( 
		tabPanel("Estadistica Programas",h4("Estadistica Descriptiva de los Programas Universitarios"),hr(),
				 verbatimTextOutput("textoEstaPrograma1"),verbatimTextOutput("textoEstaPrograma2"),DT::dataTableOutput('tablaEstaPrograma')),
		
		tabPanel("Estadistica Carnes",h4("Estadistica Descriptiva de los Carnes Universitarios"),hr(),
				 verbatimTextOutput("textoEstaCarne1"),verbatimTextOutput("textoEstaCarne2"),DT::dataTableOutput("tablaEstaCarne")),
		
		tabPanel("Query 1",h4("Query 1"),hr(),
				 verbatimTextOutput("query1Texto1"),verbatimTextOutput("query1Texto2"),DT::dataTableOutput("tablaQuery1")),
				 
		tabPanel("Query 4",h4("Query 4"),hr(),
				 verbatimTextOutput("query4Texto1"),verbatimTextOutput("query4Texto2"),DT::dataTableOutput("tablaQuery4")),
		
		tabPanel("Query 10",h4("Query 10"),hr(),
				 verbatimTextOutput("query10Texto1"),verbatimTextOutput("query10Texto2"),DT::dataTableOutput("tablaQuery10")),

		tabPanel("Query 13",h4("Query 13"),hr(),
				 verbatimTextOutput("query13Texto1"),verbatimTextOutput("query13Texto2"),DT::dataTableOutput("tablaQuery13")),

		tabPanel("Query 18",h4("Query 18"),hr(),
				 verbatimTextOutput("query18Texto1"),verbatimTextOutput("query18Texto2"),DT::dataTableOutput("tablaQuery18")),
		
		tabPanel("Query 19",h4("Query 19"),hr(),
				 verbatimTextOutput("query19Texto1"),verbatimTextOutput("query19Texto2"),DT::dataTableOutput("tablaQuery19")),
		
		tabPanel("Query 23",h4("Query 23"),hr(),
				 verbatimTextOutput("query23Texto1"),verbatimTextOutput("query23Texto2"),DT::dataTableOutput("tablaQuery23")),
		
		tabPanel("Query 25",h4("Query 25"),hr(),
				 verbatimTextOutput("query25Texto1"),verbatimTextOutput("query25Texto2"),DT::dataTableOutput("tablaQuery25")),
		
		tabPanel("Query 29",h4("Query 29"),hr(),
				 verbatimTextOutput("query29Texto1"),verbatimTextOutput("query29Texto2"),DT::dataTableOutput("tablaQuery29")),
		
		tabPanel("Query 30",h4("Query 30"),hr(), 
				 verbatimTextOutput("query30Texto1"),verbatimTextOutput("query30Texto2"),DT::dataTableOutput("tablaQuery30"))
		)
	
}