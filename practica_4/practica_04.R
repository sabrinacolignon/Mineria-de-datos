library(ggplot2)
library(dplyr)

#Ejercicio 1
datos <- read.csv("datos/Entrenamiento_ECI_2020.csv", header = TRUE)

#a) Dimensión de los datos
dim(datos) #El dataset es de 16947x52 
#Resumen de cada datos
summary(datos)
#Datos faltantes
sum(is.na(datos)) #6477 datos faltantes
#Transformación de los datos
str(datos)
#Factores
datos$Pricing..Delivery_Terms_Quote_Appr <- as.factor(datos$Pricing..Delivery_Terms_Quote_Appr)
datos$Pricing..Delivery_Terms_Approved <- as.factor(datos$Pricing..Delivery_Terms_Approved)
datos$Bureaucratic_Code_0_Approval <- as.factor(datos$Bureaucratic_Code_0_Approval)
datos$Bureaucratic_Code_0_Approved <- as.factor(datos$Bureaucratic_Code_0_Approved)
#Fechas
datos$Account_Created_Date <- as.Date(datos$Account_Created_Date, 
                                      format = "%m/%d/%y")

datos$Opportunity_Created_Date <- as.Date(datos$Opportunity_Created_Date, 
                                          format = "%m/%d/%y")

datos$Quote_Expiry_Date <- as.Date(datos$Quote_Expiry_Date, 
                                   format = "%m/%d/%y")

datos$Last_Activity <- as.Date(datos$Last_Activity, 
                               format = "%m/%d/%y")

datos$Last_Modified_Date <- as.Date(datos$Last_Modified_Date, 
                                    format = "%m/%d/%y")

datos$Planned_Delivery_Start_Date <- as.Date(datos$Planned_Delivery_Start_Date, 
                                             format = "%m/%d/%y")

datos$Planned_Delivery_End_Date <- as.Date(datos$Planned_Delivery_End_Date, 
                                           format = "%m/%d/%y")

datos$Actual_Delivery_Date <- as.Date(datos$Actual_Delivery_Date, 
                                      format = "%m/%d/%y")
#b) Variable stage
table(datos$Stage)
datos$Stage <- as.factor(datos$Stage)

#Discretizacion
datos_filtrados <-  filter(datos, Stage == "Closed Lost" | Stage == "Closed Won")
datos_filtrados$Stage <- as.factor(datos_filtrados$Stage)

#Columnas con NA
na_summary <- colSums(is.na(datos_filtrados))
#Nuevo dataframe
columnas_a_excluir <- c("Quote_Expiry_Date", "ASP", "Planned_Delivery_End_Date", "Last_Activity", "Actual_Delivery_Date", "ASP_.converted.")
aux <-  subset(datos_filtrados, select = -which(names(datos_filtrados) %in% columnas_a_excluir))
datos_filtrados <- na.omit(aux)


#c) Boxplot
ggplot(data = datos_filtrados[datos_filtrados$Total_Amount< 4.605e+05,],
       aes(x = Stage, y = Total_Amount, fill = Stage))+ geom_boxplot()

#d)Scatterplots
ggplot(data = datos_filtrados[datos_filtrados$Total_Amount< 4.605e+05,], 
       aes(y = Total_Amount, color = Stage)) +
  geom_point() #PREGUNTAR

#e)
datos_filtrados$Region <- as.factor(datos_filtrados$Region)
datos_filtrados$Bureaucratic_Code <- as.factor(datos_filtrados$Bureaucratic_Code)
# Gráfico
ggplot(datos_filtrados, aes(x = Region, fill = Region))+ geom_bar()+
  labs(title = "Frecuencia de Regiones",
       x = "Region", y = "Frecuencia")

ggplot(datos_filtrados, aes(x = Bureaucratic_Code, fill = Bureaucratic_Code))+ geom_bar()+
  labs(title = "Frecuencia de Códigos Burocráticos",
       x = "Código", y = "Frecuencia")

#f)Datos imputados
mediana_total_amount <- median(datos$Total_Amount, na.rm = TRUE)

# Completar los NA en "Total_Amount" con la mediana
datos$Total_Amount[is.na(datos$Total_Amount)] <- mediana_total_amount
#---------------------------------------------------------------------------------
#Ejercicio 2
library(rmatio)
ruta <- "C:\\Users\\fmasa\\OneDrive\\Escritorio\\Facultad\\Minería de datos\\MineriadeDatos\\MineriaDatos\\datos\\ECGs\\"
datos_señales <- read.csv("./datos/datasetRR.csv")
egcs <- list()
nombres <- list.files(ruta, pattern = "*.mat", full.names = T)
aux <- lapply(nombres,read.mat)

#Calculo de las variables
maximos <- numeric(length(aux))
minimos <- numeric(length(aux))
media <- numeric(length(aux))
mediana <- numeric(length(aux))
tiempo <- numeric(length(aux))
sd <- numeric(length(aux))
for (i in seq_along(aux)) {
  señal <- unlist(aux[[i]])
  maximos[i] <- max(señal)
  minimos[i] <- min(señal)
  media[i] <- mean(señal)
  mediana[i] <- median(señal)
  sd[i] <- sd(señal)
  tiempo[i] <- señal / 300
  
}

#Creamos dataframe
nombres_archivos <- datos_señales$ID
df_señales <- data.frame(ID_Archivo = nombres_archivos,
                         Media = media,
                         Desvio = sd,
                         Mediana = mediana,
                         Máximo = maximos,
                         Mínimo = minimos,
                         Tiempo_total = tiempo)
archivo_csv <- "señales_procesadas.csv"
write.csv(df_señales, archivo_csv, row.names = FALSE)
