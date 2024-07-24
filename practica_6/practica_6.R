#Cargamos las librerias
library("arules")

# Cargar los datos
transacciones <- read.transactions("./datos/groceries.csv", sep = ",", format = "basket", header = FALSE, cols = NULL)

#a) Explore los datos. ¿En qué forma están representados los datos?
summary(transacciones)
str(transacciones)

#¿Cuántas transacciones y cuántos ítems contienen los datos?
head(transacciones) # tiene 169 items (columns)
inspect(transacciones) # tiene 9835 transacciones
length(transacciones) # tiene 9835 transacciones

# ---------------
#b) Con el resultado de la función itemFrequency() [2] puede encontrar el
#soporte de cada elemento en el conjunto de datos. Utilice un criterio
#estadístico adecuado para definir el soporte mínimo y encuentre los 5
#itemsets frecuentes de mayor soporte.

items_freq <- itemFrequency(transacciones)

boxplot(items_freq) #como tiene muchos outliers uso la mediana
median(items_freq)
#el soporte mínimo es de: 0.0104728

#hallo los itemsets frecuentes:
soporte <- median(items_freq)

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
#supermercado para una confianza mínima de 0,5 y de 0,25. ¿Cuántas
#reglas encuentra en cada caso? ¿Qué puede decir de las reglas
#obtenidas para el caso de menor confianza?

confianza05 <- 0.5

reglas05 <- apriori(transacciones,
                  parameter = list(support = soporte,
                                   confidence = confianza05,
                                   target = "rules"),
                  control = list(verbose = F))
reglas05 

# Ordenamos las reglas por confianza y las mostramos
inspect(sort(reglas05, decreasing = TRUE, by = "confidence"))

size(reglas05)

#--------
confianza025 <- 0.25

reglas025 <- apriori(transacciones,
                    parameter = list(support = soporte,
                                     confidence = confianza025,
                                     target = "rules"),
                    control = list(verbose = F))
reglas025 

# Ordenamos las reglas por confianza y las mostramos
inspect(sort(reglas025, decreasing = TRUE, by = "confidence"))
# 
size(reglas025)

#d) Para las reglas obtenidas con una confianza de 0,25, obtenga:
  #- las reglas que tienen un lift mayor a 2;
  #- las reglas que contienen el elemento “other vegetables” en el consecuente con un lift mayor a 2;
  #- las reglas que contienen solamente “other vegetables" y "yogurt” en el antecedente y un lift mayor a 2; y
  #- las reglas que contienen “other vegetables" o "yogurt” en el consecuente y un lift mayor a 2.

#veo reglas ordenadas por lift:
inspect(sort(reglas025, decreasing=TRUE, by= "lift"))

#- las reglas que tienen un lift mayor a 2;
reglas025.lift <- subset(reglas025, subset = lift > 2)
reglas025.lift #set of 49 rules 

#veo ordenadas por lift:
inspect(sort(reglas025.lift, decreasing=TRUE, by= "lift"))

#- las reglas que contienen el elemento “other vegetables” en el consecuente con un lift mayor a 2;
reglas025.veglift <- subset(reglas025, subset=rhs%in%c("other vegetables") & lift>2)

reglas025.veglift #set of 18 rules 

#veo ordenadas por lift:
inspect(sort(reglas025.veglift, decreasing=TRUE, by= "lift"))

#- las reglas que contienen solamente “other vegetables" y "yogurt” en el antecedente y un lift mayor a 2;
reglas025.veg_yog_lift <- subset(reglas025.lift, subset=lhs%oin%c("other vegetables", "yogurt") & lift>2)

reglas025.veg_yog_lift # set of 3 rules 

#veo ordenadas por lift:
inspect(sort(reglas025.veg_yog_lift, decreasing=TRUE, by= "lift"))

#- las reglas que contienen “other vegetables" o "yogurt” en el consecuente y un lift mayor a 2.
reglas025.veg_o_yog_lift <- subset(reglas025.lift, subset=rhs%oin%c("other vegetables", "yogurt") & lift>2)

reglas025.veg_o_yog_lift #set of 28 rules
#veo ordenadas por lift:
inspect(sort(reglas025.veg_o_yog_lift, decreasing=TRUE, by= "lift"))


#------------------------------------------ EJERCICIO 2 ------------------------------------------------
#Utilice el archivo “orders.csv” de la actividad 1 de la guía anterior y obtenga:

# Cargar los datos
column_names <- c("order_id", "product_name")
transacciones_orders <- read.transactions("./datos/orders.csv", sep = ";", format = "single", header = TRUE, cols = column_names)

#a) Los itemsets frecuentes con el algoritmo Eclat, para un soporte mínimo de 0.1% 
#y una longitud mínima de 3 ítems.

itemsets <- eclat(transacciones_orders, parameter = list(supp = 0.001, minlen = 3))
itemsets #set of 219 itemsets 


#b) Encuentre las reglas de asociación para una confianza mínima del 70%,
#con los itemsets frecuentes obtenidos anteriormente y ordenelas por lift.

## Create rules from the frequent itemsets
rules <- ruleInduction(itemsets, transacciones_orders, confidence = 0.7)
rules #set of 7 rules

#veo ordenadas por lift:
inspect(sort(rules, decreasing=TRUE, by= "lift"))

#c) Compare las reglas obtenidas con el algoritmo Eclat, con las obtenidas
#para los mismos valores de soporte y confianza con el algoritmo Apriori

reglas_apriori <- apriori(transacciones_orders,
                  parameter = list(support = 0.001,
                                   confidence = 0.7,
                                   target = "rules",
                                   minlen = 3),
                  control = list(verbose = F))

reglas_apriori #set of 7 rules

# Ordenamos las reglas por confianza y las mostramos
inspect(sort(reglas_apriori, decreasing = TRUE, by = "confidence"))

#d) Compare las métricas comentadas en la teoría (coverage, test exacto de Fisher y lift) 
#para los dos conjuntos de reglas encontrados en el punto anterior

interestMeasure(rules, measure = c("lift", "coverage", "fishersExactTest"))
interestMeasure(reglas_apriori, measure = c("lift", "coverage", "fishersExactTest"))

