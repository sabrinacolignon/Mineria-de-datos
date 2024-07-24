#-------------------------- EJERCICIO 1 ----------------------------------------
library("arulesCBA")
library("arules")
library("caret")
# CargO el archivo titanic.raw.rdata
load("./datos/titanic.raw.rdata")

# Explorar la estructura de los datos
set.seed(1234) 
porc <- 0.8

#numeor de obs de los datos
N <- nrow(titanic.raw)

#tamaño de particion de entrenamiento:
tamanio<-floor (porc*N)

train.ind <- sample(seq_len(N), size = tamanio)

#separo los datos:

datos.train <- titanic.raw[train.ind, ]
datos.test <- titanic.raw[-train.ind, ]

#encuentro las cars
#a)
CARs1 <- CBA(Survived ~ ., 
             data = datos.train, 
             parameter = list(supp=0.1, conf=0.3), 
             verbose = FALSE)

inspect(CARs1$rules)
prediction <- predict(CARs1, datos.test)
tabla1 <- confusionMatrix(prediction, datos.test$Survived)
tabla1

#b) Repita para soporte = 0,001 y confianza = 0,8. ¿Cuántas CARs
#encuentra?
CARs2 <- CBA(Survived ~ ., 
             data = datos.train, 
             parameter = list(supp=0.001, conf=0.3), 
             verbose = FALSE)

inspect(CARs2$rules)
prediction2 <- predict(CARs2, datos.test)
tabla2 <- confusionMatrix(prediction2, datos.test$Survived)
tabla2
#c) CMAR con soporte mínimo 0,1 y confianza mínima de 0,3. ¿Cuántas
#CARs encuentra?
CARs3 <- CMAR(Survived ~ ., 
             data = datos.train, 
             support=0.1, 
             confidence = 0.3,
             verbose = FALSE)

inspect(CARs3$rules)
prediction3 <- predict(CARs3, datos.test)
tabla3 <- confusionMatrix(prediction3, datos.test$Survived)
tabla3

#me quedo con el que tiene menos reglas!!!

#d) Repita para soporte = 0,001 y confianza = 0,8. ¿Cuántas CARs
  #encuentra?
CARs4 <- CMAR(Survived ~ ., 
              data = datos.train, 
              support=0.001, 
              confidence = 0.8,
              verbose = FALSE)

inspect(CARs4$rules)
prediction4 <- predict(CARs4, datos.test)
tabla4 <- confusionMatrix(prediction4, datos.test$Survived)
tabla4    


# ----------------------------- EJERCICIO 2 ---------------------
#limpio
rm(list = ls())

#cargo los datos:
datos <- read.csv("./datos/Entrenamiento_ECI_2020.csv")

# Explorar la estructura de los datos
str(datos)
head(datos)
summary(datos)

datos <- datos %>% mutate(Stage = case_when(
  Stage == "Closed Won" ~ "Closed Won",
  TRUE ~ "Other"
))

col <- c(2:9,11:22,24:28,32:35,43,51)

datos <- datos %>% mutate(across(colnames(datos)[col], as.factor))

#validación:
set.seed(1234) 
porc <- 0.8

#numeor de obs de los datos
N <- nrow(datos)

#tamaño de particion de entrenamiento:
tamanio <- floor (porc*N)

train.ind <- sample(seq_len(N), size = tamanio)

#separo los datos:
datos.train <- datos[train.ind, ]
datos.test <- datos[-train.ind, ]

CARs1 <- CBA(Stage ~ ., 
             data = datos.train[,col],
             parameter = list(supp=0.01, conf=0.5), 
             verbose = FALSE)

inspect(CARs1$rules) #46 reglas
prediction <- predict(CMAR1, datos.test[,col])
tabla <- confusionMatrix(prediction, datos.test$Stage)
tabla #accuracy=0.96

#CMAR:
CMAR <- CMAR(Stage ~ ., 
              data = datos.train[,col], 
              support=0.1, 
              confidence = 0.5,
              verbose = FALSE) #da

inspect(CMAR$rules)
prediction2 <- predict(CMAR, datos.test)
tabla2 <- confusionMatrix(prediction2, datos.test$Stage)
tabla2
