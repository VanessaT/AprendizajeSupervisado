library("jsonlite")
source(file.choose("google_api.R"))

# Comienzo Parte 2

#Leo el csv

hogarescsv <- read.csv("hogares.csv")

#Elimino las Columnas no deseadas

hogarescsv$Foto <- NULL
hogarescsv$Piso <- NULL

#Creo una nueva columna llamada Tiempo

hogarescsv$Tiempo <- 0

# Utilizo el API

destino <- "Piazzale Aldo Moro"
api_key <- "AIzaSyDVvFSS-tj9RTr-eyfrIyxtmXXohpUM1mU"

hogarescsv$Dirección <- gsub("\n"," ", hogarescsv$Dirección)

for(i in (1:nrow(hogarescsv))  ){
  origen <- hogarescsv$Dirección[i]
  
  api_url <- get_url(origen, destino, api_key)
  
  datos <- get_data(api_url)
  
  datos <- parse_data(datos)
  
  datos <- as.data.frame(datos)
  
  if(datos$status=="OK"){
      cant <- datos$duration$text
      cant1 <- unique(na.omit(as.numeric(unlist(strsplit(unlist(cant), "[^0-9]+")))))
      if(length(cant1)>1){
        hora <- cant1[1]*60
        minu <- cant1[2]
        total <- minu + hora
      }else{
        total <- cant1
      }
  }
  hogarescsv$Tiempo[i] <- total
}

#Comienzo de Preprocesamiento

# Cambio todo a Minuscula para agilizar las busquedas
hogarescsv$Distrito <- tolower(hogarescsv$Distrito)
hogarescsv$Dirección <- tolower(hogarescsv$Dirección)
hogarescsv$Tipo.de.Inmueble <- tolower(hogarescsv$Tipo.de.Inmueble)
hogarescsv$Descripción <- tolower(hogarescsv$Descripción)
hogarescsv$Habitaciones.Disponibles <- tolower(hogarescsv$Habitaciones.Disponibles)
hogarescsv$Notas <- tolower(hogarescsv$Notas)
hogarescsv$Precio.Mensual <- tolower(hogarescsv$Precio.Mensual)

# Comienzo de Preprocesamiento de las Columnas

#De Tipo de Inmueble
#appartamento: 0.
#monolocale: 1.
#mini appartamento: 2.

hogarescsv$Tipo.de.Inmueble <- gsub("mini appartamento",1, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("mini\nappartamento",1, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("appartamento",0, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("monolocale",2, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("appartameno",0, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("appartamenti",0, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("apartamento",0, hogarescsv$Tipo.de.Inmueble)
hogarescsv$Tipo.de.Inmueble <- gsub("apparrtamento",0, hogarescsv$Tipo.de.Inmueble)

#De Descripción:
# Creo nuevas Columnas

# Utilizo esta funcion de r ifelse(test, yes, no) para rellenar las columnas nuevas

hogarescsv$Ingresso <- -1
hogarescsv$Camera <- -1
hogarescsv$Cucina <- -1
hogarescsv$Bagno <- -1
hogarescsv$Riscaldamento <- -1
hogarescsv$Condominio <- -1
hogarescsv$TodoIncluido <- -1
hogarescsv$DisponibleSexo <- -1
hogarescsv$PrecioMensual <- -1
hogarescsv$DisponibleHabitaciones <- -1

#Elimino cararteres innecesarios
hogarescsv$Descripción <- gsub("/"," ", hogarescsv$Descripción)
hogarescsv$Descripción <- gsub(","," ", hogarescsv$Descripción)
hogarescsv$Descripción <- gsub("-"," ", hogarescsv$Descripción)
hogarescsv$Precio.Mensual <- gsub("€"," ", hogarescsv$Precio.Mensual)
hogarescsv$Precio.Mensual <- gsub(";"," ", hogarescsv$Precio.Mensual)

#Descripcion:

hogarescsv$Ingresso <- ifelse(grepl("ingresso",hogarescsv$Descripción),1,0)
hogarescsv$Camera <- ifelse(grepl("camera|camere|camer",hogarescsv$Descripción),1,0)
hogarescsv$Cucina <- ifelse(grepl("cucina|cottura|cucna",hogarescsv$Descripción),1,0)
hogarescsv$Bagno <- ifelse(grepl("bagno|bagni",hogarescsv$Descripción),1,0)
hogarescsv$Riscaldamento <- ifelse(grepl("riscaldamento",hogarescsv$Precio.Mensual),1,0)
hogarescsv$Condominio <- ifelse(grepl("condominio",hogarescsv$Precio.Mensual),1,0)

#Notas:
hogarescsv$DisponibleSexo[grepl("ragazzi", hogarescsv$Notas)] <-  0
hogarescsv$DisponibleSexo[grepl("ragazze", hogarescsv$Notas)] <-  1
hogarescsv$DisponibleSexo[grepl("/", hogarescsv$Notas)] <- 2
hogarescsv$DisponibleSexo[39] <- 0

#De Habitaciones disponibles:
hogarescsv$DisponibleHabitaciones[grepl("1 singola", hogarescsv$Habitaciones.Disponibles)] <-  1
hogarescsv$DisponibleHabitaciones[grepl("1 singole", hogarescsv$Habitaciones.Disponibles)] <-  1
hogarescsv$DisponibleHabitaciones[grepl("2 singole", hogarescsv$Habitaciones.Disponibles)] <-  2
hogarescsv$DisponibleHabitaciones[grepl("intero appartamento", hogarescsv$Habitaciones.Disponibles)] <-  8
hogarescsv$DisponibleHabitaciones[grepl("1 doppia", hogarescsv$Habitaciones.Disponibles)] <-  5
hogarescsv$DisponibleHabitaciones[grepl("1 posto letto", hogarescsv$Habitaciones.Disponibles)] <-  9
hogarescsv$DisponibleHabitaciones[grepl("4 singole", hogarescsv$Habitaciones.Disponibles)] <-  4
hogarescsv$DisponibleHabitaciones[grepl("3 singole", hogarescsv$Habitaciones.Disponibles)] <-  3
hogarescsv$DisponibleHabitaciones[grepl("mini appartamento", hogarescsv$Habitaciones.Disponibles)] <-  7
hogarescsv$DisponibleHabitaciones[grepl("2 doppie", hogarescsv$Habitaciones.Disponibles)] <-  6
hogarescsv$DisponibleHabitaciones[grepl("monolocale", hogarescsv$Habitaciones.Disponibles)] <-  10

#Todo Incluido
hogarescsv$TodoIncluido <- ifelse(grepl("tutto incluso",hogarescsv$Precio.Mensual),1,0)

#Precio Mensual:

for(i in (1:nrow(hogarescsv))  ){
  valor <- hogarescsv$Precio.Mensual[i]
  valor1 <- unique(na.omit(as.numeric(unlist(strsplit(unlist(valor), "[^0-9]+")))))
  if(length(valor1)>1){
    precio <- lapply(valor1, function(x) x[which.max(abs(x))])
  }else{
    precio <- valor1
  }
  hogarescsv$PrecioMensual[i] <- precio
}

# Borro Columnas No necesarias

hogarescsv$Descripción <- NULL
hogarescsv$Habitaciones.Disponibles <- NULL
hogarescsv$Precio.Mensual <- NULL
hogarescsv$Notas <- NULL

hogarescsv$Tipo.de.Inmueble <- as.numeric(hogarescsv$Tipo.de.Inmueble) 
hogarescsv$Tiempo <- as.numeric(hogarescsv$Tiempo)
hogarescsv$Ingresso <- as.numeric(hogarescsv$Ingresso)
hogarescsv$Camera <- as.numeric(hogarescsv$Camera)
hogarescsv$Cucina <- as.numeric(hogarescsv$Cucina)
hogarescsv$Bagno <- as.numeric(hogarescsv$Bagno)
hogarescsv$Riscaldamento <- as.numeric(hogarescsv$Riscaldamento)
hogarescsv$Condominio <- as.numeric(hogarescsv$Condominio)
hogarescsv$TodoIncluido <- as.numeric(hogarescsv$TodoIncluido)
hogarescsv$DisponibleSexo <- as.numeric(hogarescsv$DisponibleSexo)
hogarescsv$PrecioMensual <- as.numeric(hogarescsv$PrecioMensual)
hogarescsv$DisponibleHabitaciones <- as.numeric(hogarescsv$DisponibleHabitaciones)


# Final de Preprocesamiento

#Comienzo de Regresion Lineal

library("stats")

# Divido el Dataset para hacer regresion entre hombres y mujeres

mujeres <- hogarescsv[hogarescsv$DisponibleSexo==1 | hogarescsv$DisponibleSexo==2, ]
hombres <- hogarescsv[hogarescsv$DisponibleSexo==0 | hogarescsv$DisponibleSexo==2, ]

# Llamo a la funcion
      
modelomujeres <- lm((mujeres$PrecioMensual - mujeres$Tiempo) ~., data=mujeres)

regresionmujeres <- predict(modelomujeres, newdata=mujeres)

modelohombres <- lm((hombres$PrecioMensual - hombres$Tiempo) ~., data=hombres)

regresionhombres <- predict(modelohombres, newdata=hombres)

