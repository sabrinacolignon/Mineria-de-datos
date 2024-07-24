#install.packages("pdftools")
library("pdftools")
library("stringr")


#-------------------------- EJERCICIO 1 ------------------------
rm(list = ls())
gc()

#cargo los datos
datos <- pdf_text("https://bancos.salud.gob.ar/sites/default/files/2022-10/BEN_621_SE_39.pdf")

#la tabla esta en la hoja 11:

hoja <- datos[11]

#separo por linea:
lineas <- strsplit(hoja, "\n") #\n: es donde tiene que cortar es una nueva linea

#me quedo cone el primer elemento
lineas <- lineas [[1]]

#nos quedamos con la                 
ini <- grep ("Región", lineas)[2] #el dos es porque es un vector               
#grep devuelve el vector con las posiciones donde aparece
fin <- grep ("Región del Pacífico Occidental", lineas)

tabla <- lineas [ini:fin]

head(tabla)

#elimino espacios en blanco al inicio y al final
tabla <- trimws(tabla)
#verifico:
tabla

#cambio los numeros que aparecen con la "." como divisor de miles
tabla <- str_remove_all(tabla, "\\.")

# Vamos a dividir las cadenas de las filas en 3 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla <- str_split_fixed(tabla, " {2,}", 3)

#armo el dataframe
#desde la fila 2, porque la 1 es el encabezado
tabla1 <- data.frame(matrix(tabla[2:nrow(tabla),],ncol=3))

#doy nombres
colnames(tabla1) <- tabla [1,]

#transformo los datos
tabla1$Casos<- as.numeric(tabla1$Casos)
tabla1$Fallecidos<- as.numeric(tabla1$Fallecidos)
View(tabla1)

#guardo los datos:
library("data.table")
fwrite(tabla1, "./datos/ejercicio1.csv", sep=";")

#-------------------------- EJERCICIO 2 ------------------------
rm(list = ls())
gc()

#cargo los datos
datos <- pdf_text("./datos/usbp_stats_fy2017_sector_profile.pdf")

# La tabla de interés está en la página 1
hoja <- datos[1]

# Separamos por linea
lineas <- strsplit (hoja, "\n") # \n es nueva línea

# Eliminar las dos primeras líneas de encabezado
lineas <- lineas[[1]]

ini <- grep("Miami", lineas) 

fin <- grep("Nationwide Total", lineas)

tabla <- lineas[ini:fin]

head(tabla)

#Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla <- trimws(tabla)

# Verificamos
tabla

# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla <- str_remove_all(tabla,"\\,")

# Vamos a dividir las cadenas de las filas en 10 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla <- str_split_fixed(tabla, " {2,}", 10)

# Armamos un dataframe
# Desde la fila 2, porque la 1 es el encabezado de la tabla
tabla1 <- data.frame(matrix(tabla[1:nrow(tabla),], ncol=10))
tabla1 <- tabla1[-c(5, 15), ]

# Le damos los nombres
colnames(tabla1) <- c("SECTOR", "Agent_Staffing", "Apprehensions", "Other_Than_Mexican_Apprehensions", 
                      "Marijuana", "Cocaine", "Accepted_Prosecutions", "Assaults", "Rescues", "Deaths")

# Transformamos los datos
tabla1$Agent_Staffing <- as.numeric(tabla1$Agent_Staffing)
tabla1$Apprehensions <- as.numeric(tabla1$Apprehensions)
tabla1$Other_Than_Mexican_Apprehensions <- as.numeric(tabla1$Other_Than_Mexican_Apprehensions)
tabla1$Marijuana <- as.numeric(tabla1$Marijuana)
tabla1$Cocaine <- as.numeric(tabla1$Cocaine)
tabla1$Accepted_Prosecutions <- as.numeric(tabla1$Accepted_Prosecutions)
tabla1$Assaults <- as.numeric(tabla1$Assaults)
tabla1$Rescues <- as.numeric(tabla1$Rescues)
tabla1$Deaths <- as.numeric(tabla1$Deaths)

View(tabla1)

# Guardamos los datos
fwrite(tabla1,"./datos/ejercicio2.csv", sep=";")

#-------------------------- EJERCICIO 3 ------------------------
rm(list = ls())
gc()

#cargo los datos
datos <- pdf_text("./datos/rep_covid-19_er_01_06_2022.pdf")


#TABLA N1:
# La tabla de interés está en la página 4
hoja <- datos[4]

# Separamos por linea
lineas <- strsplit (hoja, "\n") # \n es nueva línea

# Eliminar las dos primeras líneas de encabezado
lineas <- lineas[[1]]

ini <- grep("Colón", lineas) 

fin <- grep("Total", lineas)[2]

tabla <- lineas[ini:fin]

head(tabla)

#Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla <- trimws(tabla)

# Verificamos
tabla

# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla <- str_remove_all(tabla,"\\.")

# Vamos a dividir las cadenas de las filas en 5 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla <- str_split_fixed(tabla, " {2,}", 5)

# Armamos un dataframe
# Desde la fila 1:
tabla1 <- data.frame(matrix(tabla[1:nrow(tabla),], ncol=5))

# Eliminar las filas pares para usar las impares ya que se me hace una linea con datos y otra na y así:
tabla1 <- tabla1[seq(1, nrow(tabla1), by = 2), ] 

# Le damos los nombres
colnames(tabla1) <- c("Departamentos", "C_Total_Acumulados", "C_Últimos_14_días", 
                      "F_Total_Acumulados", "F_Últimos_14_días")

# Transformamos los datos
tabla1$C_Total_Acumulados <- as.numeric(tabla1$C_Total_Acumulados)
tabla1$C_Últimos_14_días <- as.numeric(tabla1$C_Últimos_14_días)
tabla1$F_Total_Acumulados <- as.numeric(tabla1$F_Total_Acumulados)
tabla1$F_Últimos_14_días <- as.numeric(tabla1$F_Últimos_14_días)

View(tabla1)

#---------------------------------------------------------------------------------------
#TABLA N2:
# La tabla de interés está en la páginas 5, 6, 7 y 8
hoja5 <- datos[5]
hoja6 <- datos[6]
hoja7 <- datos[7]
hoja8 <- datos[8]

# Separamos por linea
lineas5 <- strsplit (hoja5, "\n") # \n es nueva línea
lineas6 <- strsplit (hoja6, "\n") # \n es nueva línea
lineas7 <- strsplit (hoja7, "\n") # \n es nueva línea
lineas8 <- strsplit (hoja8, "\n") # \n es nueva línea

# Eliminar las dos primeras líneas de encabezado
lineas5 <- lineas5[[1]]
lineas6 <- lineas6[[1]]
lineas7 <- lineas7[[1]]
lineas8 <- lineas8[[1]]

#tabla1:
ini5 <- grep("Arroyo Baru", lineas5) 

fin5 <- grep("General Ramirez", lineas5)

tabla5 <- lineas5[ini5:fin5]

head(tabla5)

#Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla5 <- trimws(tabla5)

# Verificamos
tabla5

# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla5 <- str_remove_all(tabla5,"\\.")

# Vamos a dividir las cadenas de las filas en 5 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla5 <- str_split_fixed(tabla5, " {2,}", 9)

# Armamos un dataframe
# Desde la fila 1:
tabla5 <- data.frame(matrix(tabla5[1:nrow(tabla5),], ncol=9))

#-----------------------------------------
#tabla2:
ini6 <- grep("Las Cuevas", lineas6) 

fin6 <- grep("Villa Paranacito", lineas6)

tabla6 <- lineas6[ini6:fin6]

head(tabla6)

#Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla6 <- trimws(tabla6)

# Verificamos
tabla6

# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla6 <- str_remove_all(tabla6,"\\.")

# Vamos a dividir las cadenas de las filas en 5 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla6 <- str_split_fixed(tabla6, " {2,}", 9)

# Armamos un dataframe
# Desde la fila 1:
tabla6 <- data.frame(matrix(tabla6[1:nrow(tabla6),], ncol=9))

#-------------------------------------------
#tabla3:
ini7 <- grep("Alcaraz", lineas7) 

fin7 <- grep("Villa Urquiza", lineas7)

tabla7 <- lineas7[ini7:fin7]

head(tabla7)

#Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla7 <- trimws(tabla7)

# Verificamos
tabla7

# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla7 <- str_remove_all(tabla7,"\\.")

# Vamos a dividir las cadenas de las filas en 5 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla7 <- str_split_fixed(tabla7, " {2,}", 9)

# Armamos un dataframe
# Desde la fila 1:
tabla7 <- data.frame(matrix(tabla7[1:nrow(tabla7),], ncol=9))

#-------------------------------------------------------------------------
#tabla4:
ini8 <- grep("General Campos", lineas8) 

fin8 <- grep("Villaguay", lineas8)

tabla8 <- lineas8[ini8:fin8]

head(tabla8)

#Eliminamos los espacios en blanco al inicio y final de las cadenas filas
tabla8 <- trimws(tabla8)

# Verificamos
tabla8

# Como los números aparece con la "." como divisor de miles, lo eliminamos
tabla8 <- str_remove_all(tabla8,"\\.")

# Vamos a dividir las cadenas de las filas en 5 columnas y usamos como
# patrón en espacio " ", pero que aparezca 2 o más veces
tabla8 <- str_split_fixed(tabla8, " {2,}", 9)

# Armamos un dataframe
# Desde la fila 1:
tabla8 <- data.frame(matrix(tabla8[1:nrow(tabla8),], ncol=9))

#-------------------------------------------------------------------------------------
#uno los 4 dataframe en 1:

tabla_final <- rbind(tabla5, tabla6, tabla7, tabla8)


