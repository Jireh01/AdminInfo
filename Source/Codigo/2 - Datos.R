## Se cargan los archivos de datos que se usaran para su conversion a dataframes

# Se carga el archivo en excel utilizando la libreria "readxl"
library(readxl)
licenciamiento <- data.frame(read_excel("Datasets/0-Descargados/LicenciamientoInstitucional.xls"))

# Se cargan los archivos que estan separados con el simbolo "|"
carnes <- read.table("Datasets/0-Descargados/CarnesUniversitarios2018.csv", header = TRUE, sep="|")
programas <- read.table("Datasets/0-Descargados.ProgramasDeUniversidades.csv", header = TRUE, sep="|")

## Se guardan los archivos en un formato mas accesible para su futura utilizacion
write.csv(licenciamiento, file="Datasets/1-Convertidos a CSV/licenciamiento.csv")
write.csv(carnes, file="Datasets/1-Convertidos a CSV/carnes.csv")
write.csv(programas, file="Datasets/1-Convertidos a CSV/programas.csv")
