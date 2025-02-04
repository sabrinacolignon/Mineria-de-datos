# Ejemplo 1
## limpio la memoria
rm( list=ls() )
gc()
# Cargamos los paquetes que vamos a usar
library("data.table") # para usar fread()
# Descargamos los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/iris/iris.data"
archsalida <- "./datos/iris.data"

download.file(
  url = url, 
  destfile = archsalida
)
# Abrimos el archivo
datos <- fread(archsalida)
#----
# Ejemplo 2
## limpio la memoria
rm( list=ls() )
gc()
# Cargamos los paquetes que vamos a usar
#install.packages("rvest")
library("rvest")
library("dplyr")
library("robotstxt")

# a) Obtenemos una tabla
url <- "https://ourworldindata.org/famines#the-our-world-in-data-dataset-of-famines"
# Verificamos el acceso a la página
paths_allowed(url)
pagina <- read_html(url)
# Identificador CSS 
css <- ".article-block__table--wide > table:nth-child(1)"
#
tabla <- pagina %>%
  html_elements(css = css) %>% 
  html_table(fill = T, dec = ".")

# <table>xxxxxxxxx</table>
View(tabla[[1]])
#Devuelve un formato de lista que en el primer elemento tiene la tabla

datos <- tabla[[1]]
# Guardamos los datos
fwrite(datos, "./datos/famines.csv", sep = ";")

#----
# Ejemplo 3
## limpio la memoria
rm( list=ls() )
gc()
# Cargamos los paquetes que vamos a usar
#install.packages("rvest")
library("rvest")
library("dplyr")
#install.packages("robotstxt")
library("robotstxt")
# a) Obtenemos una tabla
# Verificamos el acceso a la página
paths_allowed("https://statistics.stanford.edu/people/alumni")
# Cargamos la primera página y extraemos la info
pagina <- read_html("https://statistics.stanford.edu/people/alumni?page=0")
# Definimos el elemento CSS del encabezado de la tabla
cssHeader <- ".hb-table-pattern__row"
#
header <- pagina %>% 
  html_nodes(cssHeader) %>%
  html_nodes("div") %>%
  html_text(trim = T)
# Definimos los CSS de los campos de interés 
cssTabla <- ".hb-table-pattern__body" #
cssFila <- ".hb-table-row"#
cssCamposAlumno <- ".hb-table-row__column" 
#
filas <- pagina %>% 
  html_nodes(css = cssTabla) %>% 
  html_nodes(css = cssFila) %>% 
  html_nodes(css = cssCamposAlumno) %>% 
  html_text(trim = T)
# Armamos la tabla
indices <- seq(from = 1,
               to = length(filas), 
               by = length(header))
tabla <- NULL
for (i in indices){
  aux <- filas[i:(i+4)]
  tabla <- c(tabla, aux)
}
tabla <- data.frame(matrix(tabla, ncol = length(header), 
                           byrow = T))
colnames(tabla) <- header
head(tabla)
# Guardamos nuestra tabla
library("data.table")
fwrite(tabla, file = "./salidas/pagina01Alumnos.csv", sep = ";")
# b) Obtenemos las primeras 3 páginas
# Extraemos los datos de las primeras 3 páginas de alumnos
path <- "https://statistics.stanford.edu/people/alumni?page="
ini <- 0
fin <- 2
tabla <- NULL
for(i in 0:(fin-ini)){
  pagina <- read_html(paste0(path,i))
  filas <- pagina %>% 
    html_nodes(css = cssTabla) %>% 
    html_nodes(css = cssFila) %>% 
    html_nodes(css = cssCamposAlumno) %>% 
    html_text(trim = T)
  for (i in indices){
    aux <- filas[i:(i+4)]
    tabla <- c(tabla, aux)
  }
}
tabla <- data.frame(matrix(tabla, ncol = length(header), 
                           byrow = T))
colnames(tabla) <- header
head(tabla)
tail(tabla)
# Guardamos nuestra tabla completa
library("data.table")
fwrite(tabla, file = "./salidas/pagina1-3Alumnos.csv", sep = ";")
#-------------------------------------------------------------------------------

