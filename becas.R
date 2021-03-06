becascsv <- read.csv(file="minable.csv")

# Borrar columnas que no necesito

becascsv$cIdentidad <- NULL 
becascsv$fNacimiento <- NULL
becascsv$jReprobadas  <- NULL
becascsv$dHabitacion  <- NULL
becascsv$aEconomica  <- NULL
becascsv$rating  <- NULL
becascsv$sugerencias  <- NULL
becascsv$cDireccion  <- NULL
becascsv$oSolicitudes  <- NULL
becascsv$pReside  <- NULL
becascsv$grOdontologicos  <- NULL
becascsv <- becascsv[-1]

opsu <- sum(becascsv$mIngreso==0) #OPSU
convenioi <- sum(becascsv$mIngreso==1) #Convenios Interinstitucionales
conveniod <- sum(becascsv$mIngreso==2) #Convenios Internos
prueba <- sum(becascsv$mIngreso==3) #Prueba Interna

# Divido la data en Entrenamiento y Prueba en 70% y 30%

samp <- sample(nrow(becascsv),nrow(becascsv)*0.7, replace = FALSE)

t_entrenamiento = becascsv[samp,]
t_prueba = becascsv[-samp,]

# Establezco el factor de clasificacion del entrenamiento	y el de pueba

entrenamientoLabels= t_entrenamiento$mIngreso
pruebaLabels= t_prueba$mIngreso

#Normalizamos la data

funcionn <- function(x) { return ((x - min(x)) / (max(x) - min(x))) } 
t_entrenamiento  <- as.data.frame(lapply(t_entrenamiento, funcionn)) 

# Comienzo k-Vecinos

#Voy a ir probando con varios k hantes de encontrar en mejor 
# segun su matriz de confusión
#empecé con k=5
library(class)

becas_pred1 <- knn(train = t_entrenamiento, test = t_prueba, cl = entrenamientoLabels, k=5)

matriz1 <- table(pruebaLabels,becas_pred1)

#Despues k=10

becas_pred2 <- knn(train = t_entrenamiento, test = t_prueba, cl = entrenamientoLabels, k=10)

matriz2 <- table(pruebaLabels,becas_pred2)

#Ultimo k=15

becas_pred3 <- knn(train = t_entrenamiento, test = t_prueba, cl = entrenamientoLabels, k=15)

matriz3 <- table(pruebaLabels,becas_pred3)

#Verifico cual matriz es la mejor y elijo esa

matrizvecinos <- matriz3

### Fin K Vecinos

# Ahora sigo con Reglas de Clasificación
library ('rpart')

samp <- sample(nrow(becascsv),nrow(becascsv)*0.7, replace = FALSE)

t_entrenamiento = becascsv[samp,]
t_prueba = becascsv[-samp,]

tree <- rpart(mIngreso ~ ., data = t_entrenamiento, method = 'class')

predict<-predict(tree, t_prueba, type = "class")

t_prueba$Predict <- predict
t_prueba$Predijo <- -1 
for (i in 1:nrow(t_prueba)){
  if (t_prueba$Predict[i]==0) {t_prueba$Predijo[i] <- 0}
  if (t_prueba$Predict[i]==1) {t_prueba$Predijo[i] <- 1}
  if (t_prueba$Predict[i]==2) {t_prueba$Predijo[i] <- 2}
  if (t_prueba$Predict[i]==3) {t_prueba$Predijo[i] <- 3}
}
library(pROC)
# Basic example
r <- roc(t_prueba$mIngreso, t_prueba$Predijo, levels=c(0,1,2,3))
plot(r)
matrizclasi<-table(predict, t_prueba$Predijo)

matrizclasi

### Fin Reglas de Clasificación

# Comienzo con Arboles de Desición

library ('rpart')

samp <- sample(nrow(becascsv),nrow(becascsv)*0.7, replace = FALSE)

t_entrenamiento = becascsv[samp,]
t_prueba = becascsv[-samp,]

tree <- rpart(mIngreso ~ ., data = t_entrenamiento, method = 'class')
plot(tree)
text(tree)

predict<-predict(tree, t_prueba, type = "class")

matrizarbol <- table(predict, t_prueba$mIngreso)

matrizarbol

### Fin Arboles de Desición

#Comparar Diferentes Matrices de Confución
#Matriz k-Vecinos
matrizvecinos
#Matriz Arboles de Desición
matrizarbol
#Matriz Reglas de Clasificación
matrizclasi
### Fin Parte I Tarea 2