introduccion<-{
  fluidRow(
    column(12,align="center",h3("UNIVERSIDAD PERUANA DE CIENCIAS APLICADAS - UPC"),
           tags$b(h4("Trabajo Final de Administracion de la Informacion")),
           tags$img(src="upc.png", width="10%")),
    div(
      column(12,align="center",br(),h3("Tema Coyuntural"),
             h4("En el Peru, la educacion es algo muy importante y que lamentablemente, conseguir una de buena calidad es dificil. 
               Dias antes de comenzar el trabajo final, algunas universidades comenzaron a reclamarle a la SUNEDU sobre su licencia,
               ya que muchas habian sido denegadas por la misma. Nos parecio un tema muy interesante y de indole importante para el pais.
               Para este caso de estudio sobre las universidades con licencias y sin las mismas, usamos tres datasets:
               El primero sobre los licenciamientos, su periodo, las universidades, donde quedaban.
               El segundo datasets fue sobre los programas o cursos que las universidades brindaban, asi como postgrado, y el nivel con el
               que termina ese curso/programa. El ultimo dataset es sobre los carnes que tienen las universidades por curso.
               Los tres datasets se juntarán tomando en cuenta el dataset de licenciamiento, ya que no solo es el mas importante,
               sino tambien el mas actualizado de los 3, por lo que sera esencial guiarse de ese.")),
      column(12,align="center",br(),h3("Descripcion del Proyecto"),
             h4("El siguiente trabajo contara con Datasets originales del gobierno del Peru, cuyos daots han sido recolectados en los ultimos meses.
                Se realizara diversas consultas a esta base de datos para poder obtener informacion precisa y concisa. Por otro lado, 
                nos guiaremos de graficos para poder entender y manejar la informacion de una manera mas facil.
                Asimismo, para la parte de los modelos de machine learning como regresion lineal, KNN y Kmeans, entre otros, no se usaran los
                Datasets mencionados previamente debido a que no cuentan con datos continuos que nos permiten realizar estos ejercicios.
                En su defecto, usaremos una base de datos de Calidad de Vinos sacada de la pagina de Kaggle, la cual tiene datos 
                optimos para poder realizar los modelamientos planteados.")),
      column(12,aling="center",br(),h3("INTEGRANTES")),
      column(12,aling="right",h2("Francescco Bassino")),
      column(12,align="left",tags$img(src="francescco.jpg",width="10%")),
      column(12,aling="center",h2("                                  Jireh Ferroa")),
      column(12,align="left",tags$img(src="jireh.jpg",width="10%")),
      column(12,aling="left",h3("Renzo Mondragon")),
      column(12,align="left",tags$img(src="renzo.jpg",width="10%"))                
      
    )
    
  )}
