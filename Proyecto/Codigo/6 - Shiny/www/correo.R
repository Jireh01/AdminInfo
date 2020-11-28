library(rJava)
library(mailR)

send.mail(from = "francesco_bassino@hotmail.com",
          to = c("renxzen@gmail.com", "u201816649@upc.edu.pe", "francesco8bassino@gmail.com"), 
          subject = "Trabajo Final de Administracion de la Informacion!!!",
          body = "Hola, este es un mensaje para mi compañero de grupo de Adminfo!!!",
          smtp = list(host.name = "smtp.gmail.com", port = 465,
                      user.name = "elsolido15@gmail.com",
                      passwd = "amoelsushi88", ssl = TRUE),  
          authenticate = TRUE)
getwd()