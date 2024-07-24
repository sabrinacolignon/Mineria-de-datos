# librerias 
library(dplyr)
library(ggplot2)
library(ggthemes)
library(arules)
library(arulesSequences)
library(treemapify)
library(stringr)

# Carga de datos
datos <- read.csv("./datos/e-shop clothing 2008.csv", header = TRUE, sep = ";")

#Exploración de datos
#Variables
str(datos) # Todas las variables son numéricas, se transforman las correspondientes a factor

#Resumen de datos
summary(datos) # No hay valores faltantes

#Resumen detallado de los datos numéricos
#Secuencia de clicks
summary(datos$order)
#Precio
summary(datos$price)

#Transformación de variables
#Pais
datos$country <- as.factor(datos$country)
#Sesión del usario
datos$session.ID <- as.factor(datos$session.ID)
#Categoría principal del producto
datos$page.1..main.category. <- as.factor(datos$page.1..main.category.)
#Subcategoría del producto
datos$page.2..clothing.model. <- as.factor(datos$page.2..clothing.model.)
#Color del producto
datos$page.3..colour. <- as.factor(datos$page.3..colour.)
#Ubicación del producto en la página
datos$location <- as.factor(datos$location)
#Modelo de fotografía
datos$model.photography <- as.factor(datos$model.photography)
#Información sobre el precio del producto 
datos$price.2 <- as.numeric(datos$price.2)
#N° de página en la que se encuentra el producto
datos$page <- as.factor(datos$page)

#Visualización de datos
#---------------------------------------------
#Clicks por sesion
#Histograma
ggplot(datos, aes(x = order)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma de clicks por sesión", x = "Clicks", y = "Frecuencia") +
  theme_stata()+scale_color_stata()
#Tabla de frecuencias
table(datos$order)

#Clicks totales por sesión
clicks_por_sesion <- datos %>%
  group_by(session.ID) %>%
  summarise(clicks = n())

#Histograma
ggplot(clicks_por_sesion, aes(x = clicks)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  labs(title = "Histograma de clicks por sesión", x = "Clicks", y = "Frecuencia") +
  theme_stata()+scale_color_stata()

#---------------------------------------------
#Sesiones por país
sesiones_por_pais <- datos %>%
  group_by(country) %>%
  summarise(sesiones = n_distinct(session.ID))

#Eliminamos los 4 últimos paises
sesiones_por_pais <- sesiones_por_pais[-c(43:47),]

#Creamos un dataset sin Polonia que tiene un número de sesiones muy superior al resto
sesiones_por_pais_sin_PL <- sesiones_por_pais[-c(29),]

#Ordenamos los dataset según el número de sesiones
sesiones_por_pais <- sesiones_por_pais[order(sesiones_por_pais$sesiones, decreasing = TRUE),]
sesiones_por_pais_sin_PL <- sesiones_por_pais_sin_PL[order(sesiones_por_pais_sin_PL$sesiones, decreasing = TRUE),]

#Histograma ordenado
ggplot(sesiones_por_pais, aes(x = reorder(country, -sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "#3DC61C", color = "black") +
  labs(title = "Sesiones por país incluyendo Polonia", x = "ID país", y = "N° de sesiones") +
  theme_stata()+scale_color_stata()

ggplot(sesiones_por_pais_sin_PL, aes(x = reorder(country, -sesiones), y = sesiones)) +
  geom_bar(stat = "identity", fill = "#3DC61C", color = "black") +
  labs(title = "Sesiones por país sin incluir Polonia", x = "ID país", y = "N° de sesiones") +
  theme_stata()+scale_color_stata()

#Tabla de estadísticos
summary(sesiones_por_pais$sesiones)
summary(sesiones_por_pais_sin_PL$sesiones)
#----------------------------------------------
#Productos vistos por sesión
productos_vistos_por_sesion <- datos %>%
  group_by(session.ID) %>%
  summarise(productos_vistos = n_distinct(page.2..clothing.model.))

#Boxplot de productos vistos por sesión vertical
ggplot(productos_vistos_por_sesion, aes(x = "", y = productos_vistos)) +
  geom_boxplot(fill = "#E12CB0", color = "black") +
  labs(title = "Boxplot de productos vistos por sesión", x = "", y = "N° de productos vistos") +
  theme_stata()+scale_color_stata()

#Tabla de estadísticos
summary(productos_vistos_por_sesion$productos_vistos)
#----------------------------------------------
#Categorías de productos mas vistas por sesión
categorias_mas_vistas <- datos %>%
  group_by(session.ID) %>%
  summarise(categoría_producto = n_distinct(page.1..main.category.))
#Creamos un nuevo dataframe con las frecuencias de cada categoría
categorias_mas_vistas <- categorias_mas_vistas %>%
  group_by(categoría_producto) %>%
  summarise(frecuencia = n())

#Treemap
ggplot(categorias_mas_vistas, aes(area = frecuencia, fill = as.factor(categoría_producto))) +
  geom_treemap() + geom_treemap_text(aes(label = frecuencia), colour = "white") +
  labs(title = "Treemap de categorías de productos más vistas por sesión", fill = "Categoría", size = "Frecuencia") +
  theme_stata()+scale_color_stata()

#-----------------------------------------------
#Evolución de la cantidad de clicks por sesión a través de los meses
#Agrupamos la cantidad de clicks por mes
clicks_por_mes <- datos %>%
  group_by(month) %>%
  summarise(clicks = n())

#Gráfico de línea
ggplot(clicks_por_mes, aes(x = month, y = clicks)) +
  geom_line(color = "#E12CB0") +
  geom_point(color = "#E12CB0") +
  geom_text(aes(label = clicks), hjust = -0.2, vjust = 0.5) +
  labs(title = "Evolución de la cantidad de clicks por sesión a través de los meses", x = "Mes", y = "N° de clicks") +
  theme_stata()+scale_color_stata()
#-----------------------------------------------
#Número de transacciones e items
datos_agrupados <- datos %>%
  select(session.ID, page.2..clothing.model.) %>%
  group_by(session.ID) %>%
  summarise(productos = list(page.2..clothing.model.))

#Número de transacciones
length(unique(datos_agrupados$session.ID))
#Número de items
length(unique(unlist(datos_agrupados$productos)))
#-----------------------------------------------

#Generamos un objeto de tipo transactions
transacciones <- as(datos_agrupados$productos, "transactions")

#Itemsets frecuentes
itemsets_frecuentes <- apriori(transacciones,
                               parameter = list(support = 0.02,
                                                target = "frequent itemsets",
                                                minlen = 2),
                               control = list(verbose = FALSE))

itemsets_frecuentes_ordenados <- sort(itemsets_frecuentes, by = "support", decreasing = TRUE)

summary(itemsets_frecuentes_ordenados)
#Mostramos los diez primeros de mayor soporte

inspect(itemsets_frecuentes_ordenados[1:10])

#-----------------------------------------------
#Reglas de asociación para Polonia en categoria blusas
#Filtramos los datos

datos_polonia_blusas <- datos %>%
  filter(country == 29 & page.1..main.category. == 3)

#Agrupamos los datos
datos_agrupados_polonia_blusas <- datos_polonia_blusas %>%
  select(session.ID, page.2..clothing.model.) %>%
  group_by(session.ID) %>%
  summarise(productos = list(page.2..clothing.model.))

#Generamos un objeto de tipo transactions
transacciones_polonia_blusas <- as(datos_agrupados_polonia_blusas$productos, "transactions")

#Reglas de asociación
reglas_asociacion_polonia_blusas <- apriori(transacciones_polonia_blusas,
                                            parameter = list(support = 0.02,
                                                             confidence = 0.2,
                                                             target = "rules"),
                                            control = list(verbose = FALSE))

#Ordenamos las reglas
reglas_asociacion_polonia_blusas_ordenadas <- sort(reglas_asociacion_polonia_blusas, by = "support", decreasing = TRUE)
#Mostramos las 10 primeras
inspect(reglas_asociacion_polonia_blusas_ordenadas[1:10])
#---------------------------------------------------------------
#Reglas de asociación para República Checa en categoria blusas
#Filtramos los datos
datos_rep_checa_blusas <- datos %>%
  filter(country == 9 & page.1..main.category. == 3)

#Agrupamos los datos
datos_agrupados_rep_checa_blusas <- datos_rep_checa_blusas %>%
  select(session.ID, page.2..clothing.model.) %>%
  group_by(session.ID) %>%
  summarise(productos = list(page.2..clothing.model.))

#Generamos un objeto de tipo transactions
transacciones_rep_checa_blusas <- as(datos_agrupados_rep_checa_blusas$productos, "transactions")

#Reglas de asociación
reglas_asociacion_rep_checa_blusas <- apriori(transacciones_rep_checa_blusas,
                                              parameter = list(support = 0.04,
                                                               confidence = 0.25,
                                                               target = "rules"),
                                              control = list(verbose = FALSE))

#Ordenamos las reglas
reglas_asociacion_rep_checa_blusas_ordenadas <- sort(reglas_asociacion_rep_checa_blusas, by = "support", decreasing = TRUE)
#Mostramos las 10 primeras
inspect(reglas_asociacion_rep_checa_blusas_ordenadas[1:7])


#------------------------------------------------------------------
# Secuencias mas frecuentes
#Seleccionamos las columnas a utilizar

datos_secuencias <- datos %>%
  select(session.ID, order, page.2..clothing.model.)

#Cambiamos los nombres de las columnas
colnames(datos_secuencias) <- c("sequenceID", "eventID", "itemID")

#Convertimos a transaccion
transacciones_secuencias <- as(datos_secuencias %>% transmute(items = itemID), "transactions")

#Cambiamos el nombre de las columnas en la transaccion para poder trabajar con el algoritmo cSPADE
transactionInfo(transacciones_secuencias)$sequenceID <- datos_secuencias$sequenceID
transactionInfo(transacciones_secuencias)$eventID <- datos_secuencias$eventID

#Cambiamos los nombres de los items
itemLabels(transacciones_secuencias) <- str_replace_all(itemLabels(transacciones_secuencias), "items=", "")

#Revisamos las 10 primeras secuencias
inspect(transacciones_secuencias[1:10])
#Aplica el algoritmo de secuencias
secuencias_frecuentes <- cspade(transacciones_secuencias,
                     parameter = list(support = 0.03),
                     control = list(verbose = F))
#Secuencias obtenidas
summary(secuencias_frecuentes)

#Nos quedamos solo con las que tienen mas de un item
secuencias_frecuentes.mayores <- subset(secuencias_frecuentes, size(x) > 1)

#Ordenamos las secuencias
secuencias_frecuentes.mayores <- sort(secuencias_frecuentes.mayores, by = "support", decreasing = TRUE)
inspect(secuencias_frecuentes.mayores)
