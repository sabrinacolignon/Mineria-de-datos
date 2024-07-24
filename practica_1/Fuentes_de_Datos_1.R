#install.packages("rvest")
# Carga el paquete rvest
#library(rvest)

# EJEMPLO DE PRÁCTICA

#rm (list=ls))

#gc()

#library("data.table")

#url <- ""

#archsalida <- ".7datos/iris.data"

#download.file(
  #url= url,
  #destfile =  archsalida)

#abro:
#datos <- fread(archsalida)

#------------------------------------------------------------------
library("rvest")
library("dplyr")
library("robotstxt")
library("data.table")

#a) obtenemos la tabla:
url <- "https://ourworldindata.org/famines#the-our-world-in-data-dataset-of-famines"

#verifico acceso a la pagina
paths_allowed(url)
pagina <- read_html(url)

#identificador css:
css <-"#owid-document-root > article > div.article-block__table--wide.col-start-2.span-cols-12.article-block__table--header-row > table"

#my-players-table > div.mod-container.mod-table.mod-no-header-footer > div > table

#devuelve una lista con todo el contenido
tabla <- pagina %>% #tabla es un objeto de tipo lista que tiene adentro una tabla
  html_elements(css=css) %>%
  html_table(fill=T, dec=".")


#devuelve en formato lista a la tabla:
View(tabla[[1]])

datos <- tabla [[1]]

#guardo los datos:
fwrite (datos, "./datos/faminies.csv", sep=";")

#-------------------------------------------------------
#EJERCICIO 2
rm( list=ls() ) #limpio memoria
gc()

url2 <- "https://statistics.stanford.edu/people/alumni"

#verifico acceso a tabla
paths_allowed("https://www.rdocumentation.org/packages/data.table/versions/1.15.0/topics/fwrite")

#cargo pagina y extraigo info:
pagina <- read_html("")

#defino el elemento css del encabezado
cssHeader <- ""

header<- pagina %>%
  html_nodes(cssHeader) %>%
  html_nodes("div") %>% #etiquetas
  html_text(trim=T) #saco texto de etiquetas

#defino css de campos de interes:
cssTabla <- ""
cssFila <- ""
cssCampoAlumno <- ""

filas <- pagina %>%
  html_nodes(css = cssTabla) %>%
  html_nodes(css = cssFila) %>%
  html_nodes(css = cssCampoAlumno) %>%
  html_text(trim=T)

#armo la tabla:
indices <- seq (from=1,
                to= length(filas),
                by= lenght(header))
tabla <- NULL

for (i in indices){
  aux<- filas
  
}
#----------------------------------------------------------------------------------------------------------------
# ---------------------------------------------- EJERCICIO 1 ----------------------------------------------------
#Cargo las librerías:
library("rvest")
library("dplyr")
library("robotstxt")


rm( list=ls() ) #limpio memoria
gc()

# Descargamos los datos
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00616/Tetuan%20City%20power%20consumption.csv"
archsalida <- "./datos/machine_learning"

download.file(
  url = url, 
  destfile = archsalida
)
# Abrimos el archivo
datos <- fread(archsalida)

#-----------------------------------------------------------------------------------------------------------------
# ---------------------------------------------- EJERCICIO 2 -----------------------------------------------------
rm( list=ls() ) #limpio memoria
gc()

#a) obtenemos la tabla:
url <- "https://www.espn.com/nfl/superbowl/history/winners"

#verifico acceso a la pagina
paths_allowed(url)
pagina <- read_html(url)

#identificador css:
css <-"#my-players-table > div.mod-container.mod-table.mod-no-header-footer > div > table"


#devuelve una lista con todo el contenido
tabla <- pagina %>% #tabla es un objeto de tipo lista que tiene adentro una tabla
  html_elements(css=css) %>% #uso "elements" porque en la pagina tiene el formato de TABLE !!
  html_table(fill=T, dec=".")


#devuelve en formato lista a la tabla:
View(tabla[[1]])

datos <- tabla [[1]]

#guardo los datos:
fwrite (datos, "./datos/nfl.csv", sep=";")

#-----------------------------------------------------------------------------------------------------------------
# ---------------------------------------------- EJERCICIO 3 -----------------------------------------------------
rm( list=ls() )
gc()

# a) Obtenemos una tabla
# Verificamos el acceso a la página
paths_allowed("https://statistics.stanford.edu/people/alumni")

# Cargamos la primera página y extraemos la info
pagina <- read_html("https://statistics.stanford.edu/people/alumni?page=0")

# Definimos el elemento CSS del encabezado de la tabla
cssHeader <- "#block-humsci-colorful-content > article > div > div.decanter-grid.hb-three-column.clearfix.hb-three-column--one-sidebar > div.hb-layout-builder-main-content.hb-three-column__main > div > div > div:nth-child(2) > div > div.paragraph-item.ptype-hs-view > div > div > div > div > div > div > div > div.hb-table-pattern__header"

header <- pagina %>% 
  html_nodes(cssHeader) %>%
  html_nodes("div") %>%
  html_text(trim = T)


# Definimos los CSS de los campos de interés 
#cssTabla <- "#block-humsci-colorful-content > article > div > div.decanter-grid.hb-three-column.clearfix.hb-three-column--one-sidebar > div.hb-layout-builder-main-content.hb-three-column__main > div > div > div:nth-child(2) > div > div.paragraph-item.ptype-hs-view > div > div > div > div > div > div > div > div.hb-table-pattern__body" 
#cssFila <- "#block-humsci-colorful-content > article > div > div.decanter-grid.hb-three-column.clearfix.hb-three-column--one-sidebar > div.hb-layout-builder-main-content.hb-three-column__main > div > div > div:nth-child(2) > div > div.paragraph-item.ptype-hs-view > div > div > div > div > div > div > div > div.hb-table-pattern__body > div:nth-child(1)"
#cssCamposAlumno <- "#block-humsci-colorful-content > article > div > div.decanter-grid.hb-three-column.clearfix.hb-three-column--one-sidebar > div.hb-layout-builder-main-content.hb-three-column__main > div > div > div:nth-child(2) > div > div.paragraph-item.ptype-hs-view > div > div > div > div > div > div > div > div.hb-table-pattern__body > div:nth-child(1) > div:nth-child(1)" 

cssTabla <- ".hb-table-pattern__body" 
cssFila <- ".hb-table-row"
cssCamposAlumno <- ".hb-table-row__column"

filas <- pagina %>% 
  html_nodes(css = cssTabla) %>% #uso "nodes" porque en la pagina tiene el formato de DIV !!
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
fwrite(tabla, file = "./datos/pagina1_Alumnos.csv", sep = ";")


# b) obtengo alumnos de 1980 a 2007

# Extraemos los datos de las paginas correspondientes:
path <- "https://statistics.stanford.edu/people/alumni?page="
ini <- 4
fin <- 8
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
fwrite(tabla, file = "./datos/1980-2007Alumnos.csv", sep = ";")
