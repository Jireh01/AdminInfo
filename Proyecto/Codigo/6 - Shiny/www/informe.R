informe<-{
  fluidRow(
    column(12,align="center",h3("UNIVERSIDAD PERUANA DE CIENCIAS APLICADAS - UPC"),
           tags$b(h4("Trabajo Final de Administracion de la Informacion")),
           tags$img(src="upc.png", width="10%")),
    div(
      column(12,align="Left",br(),h3("Tema Coyuntural"),
             h4("En el Peru, la educacion es algo muy importante y que lamentablemente, conseguir una de buena calidad es dificil. 
               Dias antes de comenzar el trabajo final, algunas universidades comenzaron a reclamarle a la SUNEDU sobre su licencia,
               ya que muchas habian sido denegadas por la misma. Nos parecio un tema muy interesante y de indole importante para el pais.
               Para este caso de estudio sobre las universidades con licencias y sin las mismas, usamos tres datasets:
               El primero sobre los licenciamientos, su periodo, las universidades, donde quedaban.
               El segundo datasets fue sobre los programas o cursos que las universidades brindaban, asi como postgrado, y el nivel con el
               que termina ese curso/programa. El ultimo dataset es sobre los carnes que tienen las universidades por curso.
               Los tres datasets se juntarán tomando en cuenta el dataset de licenciamiento, ya que no solo es el mas importante,
               sino tambien el mas actualizado de los 3, por lo que sera esencial guiarse de ese.")),
      column(12,align="Left",br(),h3("Descripcion del Proyecto"),
             h4("El siguiente trabajo contara con Datasets originales del gobierno del Peru, cuyos daots han sido recolectados en los ultimos meses.
                Se realizara diversas consultas a esta base de datos para poder obtener informacion precisa y concisa. Por otro lado, 
                nos guiaremos de graficos para poder entender y manejar la informacion de una manera mas facil.
                Asimismo, para la parte de los modelos de machine learning como regresion lineal, KNN y Kmeans, entre otros, no se usaran los
                Datasets mencionados previamente debido a que no cuentan con datos continuos que nos permiten realizar estos ejercicios.
                En su defecto, usaremos una base de datos de Calidad de Vinos sacada de la pagina de Kaggle, la cual tiene datos 
                optimos para poder realizar los modelamientos planteados.")),
      
      column(12, align="Left", br(),h3("Procedimiento"),style="text-align: justify;",
             h4("Se ha recolectado los datos de https://www.datosabiertos.gob.pe/.Cave recalcar que para la recoleccion se han 
             descargado fuentes de tipo csv y excel. UNa vez realizado esto, se han preprocesado los valores, se han quitado
                tildes, se han removido los espacios en blanco y reemplazados con N/A, se uniformizado los nombres de as variables, se han removido las columnas innecesarias
                y se ha convertido los string a enteros. Por ultimo, se han guardado los 3 Dataset ya preprocesados y se han guardado en un formato csv, 
                esto debido a que deseamos que sea homogeneo.
                Una vez obtenido estos nuevos csv se ha pasado a la exploracion y transforamcion de los datasets. Como se menciono en la introduccion, lo que se
                busca es uniformar los 3 dataset y formar uno con las licencias actualziadas, por lo que se ha agarrado las 3 dataset y se ha realziado un Inner_join
                para que al final nos quedemos con las columnas deseadas como nombre, numero de programas y cantidad de carnets.
                Una vez realziado esto, se ha creado el ultimo dataet con los licenciamientos actualizados.
                Finalmente, pasamos a las consultas. En estas se abarcaron diferentes conceptos realizados en clase. Tenemos consultas en las que se ha 
                sacado precentiles, media, maximo y minimo. En otras hemos usado el select y filter para un mejor manejo de datos. Todas las consultas de datos que se han realizado
                han sido con el proposito de poder manjear la informacion de forma mas facil y sencilla. Estas consultas van desde lo mas basico hasta lo 
                mas complicado. En la parte de graficos, se ha usado diferentes herramientas y diagramas para poder representar algunas consultasy problematicas de los Datasets.
                Entre estos diagramas utilizados estan los de barra, dispercion, caja y pie. Estos graficos, al igual que las consultas, van en aumento de complejidad.
                En la parte de los modelados, como se ha mencionado en la presentacionm, nuestro dataset no era apto para realizar estos modelados. Por ello, se consulto
                al docente si se podia realizar esto con un dataset externo, a lo que el afirmo. El primer modelo realizado fue la regresion lineal, a esta se 
                utilizola funcion lm, que esta integrada dentro de R. Esta nos ha permitido dibujar la linea de regresion en un eje x vs y. Después, se vio la regresion
                polinomial, esta regresion no se puede greaficar debido a que al ser tres variables se tenia que dibujar en 3D y al ser 4, en 4D. En el proyecto, se ha utilizado
                una variable con respecto a otras tres. Una vez hecho esto utilizamos la funcion predict, incorporada en R, para poder, valga la redundancia, predecir
                el resultado con el dataset entrenado anteriormente. Por ultimo, se calcula la accuracy del algoritmo. El siguiente modelo es el KNN. 
                Para este se ha utilizado la funcion incorporada de R, donde se ha utilziado el 70% de un dataset para entrenar y el otro 30% para testear.
                Una vez hecho esto se realiza una fucnion para calcular el accuracy del algoritmo. El sigueinte algoritmo es Kmeans, para este se ha utilziado
                cualquier dos variables, x, y del dataframe vinos. Utilizaremos la funcion incorporada de R, en esta funcion se ha pasado los
                Dataframe y los k. Luego, simplemente, los ploteamos con la funcion kmeans. El algoritmo PCA se ha realziado el mismo procedimiento que realizo
                el docente al explicar este problema. SVM, se ha utlizado un x y y del dataframe, cave recalcar que el x y y tienen que estar
                como factor. Una vez hecho esto se utiliza la funcion SVM de R, en la cual se le tiene que pasar y con la formula que se incorporará para resolver 
                dicho modelo de Machine Learning, luego le pasamos el dataframe, el kernel, que en este caso es linear o radial y por ultimo el 
                costume, que puede variar. Luego, simplemente, ploteamos, utilizando la funcion creada SVM y el dataframe.
                Adicionalmente, en el .R del github se encuentra un modelo de Machine Learning adicional de SVM.
                KN2.0, este KN se incorporo siguiendo el modelo del profesor, la diferencia es que se ha tratado de generalizar el x para que pueda
                aceptar cualqueir largo maximo y minimo, asi no tiene que se especificado manualmente cada vez que se cambie el x. Para mas informacion
                consultar modelados.R")),
      column(12,align="Left",tags$img(src="010.jpg",width="50%")),
      column(12,align="Left",tags$img(src="009.jpg",width="50%")),
      column(12, align="Left", br(),h3("Conclusiones"),
             h4("Hemos podido observar que existen diferentes modelos que nos pueden ayudar a manejar la informacion. Debemos tener en cuenta que no todos
                los datasets se prestan para hacer todo tipo de modelado, si bien es cierto, se pueden realizar varias consultas en los datasets descargados
                del gobierno, no se pueden aplicar modelos predictivos en este, por ello es que solo se ha utilizado para la parte de recoleccion de la informacion.
                Para futuras referencias, debemos tomar en cuenta la variedad de modelos de Dataset que pueden existir y que no todos se adecuan a lo que deseamos.
                Por otro lado, el manejo de la informacion que se ha realziado pretende ayudar a la facilidad de este, es bueno ser preciso y consiso al momento
                de poder realizar querys y consultas, siempre tomando en cuenta que otra persona puede leer la informacion rescatada y la tiene que entender.
                Finalmente, se puede observar que con Datasets que se presten a modelos predictivos se pueden hacer variedades de cosas. Se sabe que mas adelante
                se va a llevar un curso de Machine Learning, pero con el, capaz, poco concimiento que tenemos ahora se pueden hacer infinidad de cosas.")),
      column(12, align="Left", br(),h3("Agradecimientos"),
             h4("Agradecemos al profesor WALTER CUEVA por habernos guiado en todo este trayecto del ciclo que, a pesar de pequeños tropezones que se
                ha podido dar en todo este tiempo, ha sabido tratar a sus alumnos con sabiduria e inteligencia.
                Gracias profesor.
                Por favor, sea bueno calificando, Gracias."))
             
      
    )
    
  )}
