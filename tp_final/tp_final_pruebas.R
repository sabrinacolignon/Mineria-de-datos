clickstream_data  <- read.csv("./datos/e-shop clothing 2008.csv")
# Visualización de las primeras filas del conjunto de datos
head(clickstream_data)
# Verificación de los nombres de las columnas
names(clickstream_data)

# Instalación y carga de la librería dplyr si no está instalada
if(!require(dplyr)) install.packages("dplyr")
library(dplyr)

# Asociación de la categoría principal a cada producto específico
product_categories <- clickstream_data %>%
  select("page 1 (main category)", "page 2 (clothing model)") %>%
  distinct()

# Visualización de la asociación
head(product_categories)
