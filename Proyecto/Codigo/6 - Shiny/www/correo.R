library(rJava)
library("mailR")

send.mail(from = "----------------",
          to = c("correo1@gmail.com", "francesco8bassino@gmail.com"), 
          subject = "Trabajo Final de Administracion de la Informacion!!!",
          body = "Hola, este es un mensaje para mi compañero de grupo de Adminfo!!!",
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "josezea@usantotomas.edu.co",
                      passwd = "contraseña", ssl = TRUE),  
          authenticate = TRUE,
          send = TRUE,
          attach.files = c("wordcloud.png" ),
          file.names = c( "de palabras.png"), # El nombre con el que se quiere adjuntar el correo
          file.descriptions = c("Archivo que contiene nube de palabras"),
          # optional parameter
          debug = TRUE)