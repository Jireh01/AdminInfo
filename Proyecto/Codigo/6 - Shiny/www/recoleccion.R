recoleccion<-{ 

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
  
  
  }