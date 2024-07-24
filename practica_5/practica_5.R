#install.packages("arules")
#Cargamos las librerias
library("arules")

# Cargar los datos
column_names <- c("order_id", "product_name")
transacciones <- read.transactions("./datos/orders.csv", sep = ";", format = "single", header = TRUE, cols = column_names)

# Explorar los datos
summary(transacciones)

# Encontrar itemsets frecuentes con un soporte mínimo del 1%
soporte <- 0.001
itemsets_frec <- apriori(transacciones, 
                         parameter = list(support = soporte,
                                          target = "frequent itemsets"), 
                         control = list(verbose=F))
inspect(itemsets_frec)

# Encontramos itemsets frecuentes 
inspect(transacciones[1:5])

size(transacciones)

# Vemos los 5 itemsets frecuentes de mayor soporte
itemsets <- sort(itemsets_frec, by = "support", decreasing = TRUE)
inspect(itemsets)

#c) Obtenga las reglas de asociación para todas las transacciones del
#supermercado para una confianza mínima de 0,7.

confianza <- 0.7

reglas <- apriori(transacciones,
                  parameter = list(support = soporte,
                                   confidence = confianza,
                                   target = "rules"),
                  control = list(verbose = F))
reglas 

# Ordenamos las reglas por confianza y las mostramos
inspect(sort(reglas, decreasing = TRUE, by = "confidence"))
# 
size(reglas)


# ------------------------------------ EJERCICIO 2 --------------------------------------------------------
# CargO el archivo titanic.raw.rdata
load("./datos/titanic.raw.rdata")
# Explorar la estructura de los datos
str(titanic.raw)
head(titanic.raw)
summary(titanic.raw)

#convierto a transacciones:
transacciones_titanic <- as(titanic.raw, "transactions")

aux <- transactions(titanic.raw)

inspect(aux)

#Encuentre las reglas de asociación para un soporte del 0,5% y una
#confianza del 80%, donde el consecuente sea si ese pasajero sobrevivió
#o no.

#rhs=consecuente
#lhs=antecedente
reglas_titanic <- apriori( titanic.raw,
                           parameter = list (minlen = 2, support = 0.005, conf=0.8),
                           appearance = list(rhs=c("Survived=No", "Survived=Yes"),
                                             default="lhs"),
                           control = list(verbose=F)               
  )

# Ordenamos las reglas por confianza y las mostramos
inspect(sort(reglas_titanic, decreasing = TRUE, by = "confidence"))
# 
size(reglas_titanic)

