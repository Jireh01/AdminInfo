library(tidyverse)
library(corrplot)
library("ggplot2")

vinos <- read.csv("Datasets/0-Descargados/winequality-red.csv")

v <- cor(vinos)
corrplot(v)
view(vinos)
#################### Regresion Lineal ####################
# Regresion lineal (en el shiny las variables las pone el usuario)
R = lm(vinos$fixed.acidity~vinos$pH)
# ploteal los puntos
plot(vinos$pH, vinos$fixed)
# Te muestra la linea de regresion
abline(R, col="red", lwd=2)

############### Regresion polinomial #####################
# para ver la regresion del fixed.acidity en relacion a las otras 3 variables
# citric.acid, density, pH

ids <- sample(1:nrow(vinos),size=nrow(vinos)*0.7,replace = FALSE)

entrenamiento <- vinos[ids, c(1,3,8,9)]
probar <- vinos[-ids, c(1,3,8,9)]

ft = lm(fixed.acidity~ citric.acid + density + pH, data=entrenamiento)

# predecir
predict(ft, probar)
probar$prediccion <- predict(ft, probar)
probar

# Determinar la precision del modelo entrenado (porcentaje)
error <- mean(abs(100*((probar$prediccion - probar$fixed.acidity)/ probar$prediccion)))

accuracy <- 100 - error
accuracy


################### KNN #####################
library(ggplot2)
library(class)

# en esta parte seleccionas que variables quieres 
vinosKnn <- data.frame(vinos$fixed.acidity, vinos$chlorides)
dat <- sample(1:nrow(vinosKnn),size=nrow(vinosKnn)*0.7,replace = FALSE)

train <- vinos[dat,] # 70%
test <- vinos[dat,] # 30%

train.labels <- vinos[dat,1]
test.labels <- vinos[-dat,1]

knn <- knn(train=train, test=test, cl=train.labels, k = 10, prob=TRUE)

accuracy.fin <- 100 * sum(train.labels == knn)/NROW(test.labels)
accuracy.fin


################### KMeans ##################
corrplot(v)
plot(vinos$free.sulfur.dioxide, vinos$sulphates)
df <- data.frame(vinos$free.sulfur.dioxide, vinos$sulphates)

kmeans <- kmeans(df, 7)
plot(df, col = kmeans$cluster)
points(kmeans$centers, col = 1:2, pch = 8, cex = 2)


################### PCA #####################
vinosPCA <- scale(vinos)
pca <- prcomp(vinosPCA)
str(pca)
pca[[1]] # desviaciones
pca[[2]] # rotaciones
pca[[5]] # individuos

# Dependiendo de cuantas componentes se escribe abajo 
componentes <- cbind(pca[[2]][,1],pca[[2]][,2],pca[[2]][,3], pca[[2]][,4])
individuos <- pca[[5]][,c(1:4)]

#install.packages("ade4") 
library(ade4)
# analisis de cluster del componente c1 y c2
s.corcircle(componentes[,c(1,2)]) #Todos los componentes de la col 1 y 2
#cuales son los regristros que indican el grado de participacion

s.corcircle(componentes[,c(1,4)])


################### SVM #####################
library(e1071)

dff <- data.frame(vinos$quality, vinos$free.sulfur.dioxide)

m <- svm(vinos.quality~., data = dff)
plot(m, dff)

## more than two variables: fix 2 dimensions
m2 <- svm(Species~., data = iris)
plot(m2, iris, Petal.Width ~ Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))
