#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 24/02/2024
#Fecha de ultima modificacion: 24/02/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot);library(leaflet)
options(scipen = 999)
################################################################################-

## Cargamos la base de datos de origen destino 

destino <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.csv")%>%
  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%select(-"...1")%>%
  rename(producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto=str_to_lower(producto))

destino$mpio_destino[destino$mpio_destino=="Cartagena de Indias"]<-"Cartagena"

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# Agregamos información de las rutas
rutas <- readRDS("01_Datos/Data/Bases_auxiliares/Input/rutas_abastecimiento.rds")
rutas <- na.omit(rutas)

destino$codigo_mpio_origen <- as.numeric(destino$codigo_mpio_origen)

destino <- merge(x=destino,y=rutas,by=c("codigo_mpio_destino","codigo_mpio_origen"),all.x=TRUE)
destino <- na.omit(destino)

# Calculamos  % de prodcto por muncicipio que entra a "Medellin" desde cualquier municipio
abastecimiento_medellin <- destino %>% filter( mpio_destino == 'Medellín')

# Creamos una variable que se llame interno o externo y depende si el abast es desde antioquia o fuera de antioquia 
abastecimiento_medellin  <- abastecimiento_medellin  %>%
  mutate(tipo = ifelse(startsWith(as.character(codigo_mpio_origen), "05"), "interno", "externo"))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio) %>%
  mutate(total_kilogramos_ano = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (por producto)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, producto) %>%
  mutate(total_kilogramos_ano_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes) %>%
  mutate(total_kilogramos_mes = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (por producto)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, producto) %>%
  mutate(total_kilogramos_mes_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por mes - año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, anio) %>%
  mutate(total_kilogramos_año_mes = sum(suma_kg))

# Cantidad de comida en Kg que entra por mes - año a medellin 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, anio,producto) %>%
  mutate(total_kilogramos_año_mes_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(producto) %>%
  mutate(total_kilogramos_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(producto,codigo_mpio_origen) %>%
  mutate(total_kilogramos_producto_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(codigo_mpio_origen) %>%
  mutate(total_kilogramos_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin$total_kilogramos_general <- sum(abastecimiento_medellin$suma_kg)
# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen) %>%
  mutate(total_kilogramos_año_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, codigo_mpio_origen) %>%
  mutate(total_kilogramos_mes_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra por año - mes a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mes) %>%
  mutate(total_kilogramos_año_mes_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,producto) %>%
  mutate(total_kilogramos_año_municipio_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, codigo_mpio_origen,producto) %>%
  mutate(total_kilogramos_mes_municipio_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año - mes a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mes,producto) %>%
  mutate(total_kilogramos_año_mes_municipio_producto = sum(suma_kg))

################################################################################
#IMPORTANCIA MUNICIPIOS
abastecimiento_medellin$importancia_año_mes = (abastecimiento_medellin$total_kilogramos_año_mes_municipio/abastecimiento_medellin$total_kilogramos_año_mes)
abastecimiento_medellin$importancia_año = (abastecimiento_medellin$total_kilogramos_año_municipio/abastecimiento_medellin$total_kilogramos_ano)
abastecimiento_medellin$importancia_mes = (abastecimiento_medellin$total_kilogramos_mes_municipio/abastecimiento_medellin$total_kilogramos_mes)
abastecimiento_medellin$importancia_año_mes_producto = (abastecimiento_medellin$total_kilogramos_año_mes_municipio_producto/abastecimiento_medellin$total_kilogramos_año_mes_producto)
abastecimiento_medellin$importancia_año_producto = (abastecimiento_medellin$total_kilogramos_año_municipio_producto/abastecimiento_medellin$total_kilogramos_ano_producto)
abastecimiento_medellin$importancia_mes_producto = (abastecimiento_medellin$total_kilogramos_mes_municipio_producto/abastecimiento_medellin$total_kilogramos_mes_producto)
abastecimiento_medellin$importancia_producto = (abastecimiento_medellin$total_kilogramos_producto_municipio/abastecimiento_medellin$total_kilogramos_producto)
abastecimiento_medellin$importancia_municipio = (abastecimiento_medellin$total_kilogramos_municipio/abastecimiento_medellin$total_kilogramos_general)
################################################################################

# Km recorridos por comida que entra por año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio) %>%
  mutate(kilometros_ano = mean(distance))

#Km recorridos por comida que entra por año a medellin (por producto)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, producto) %>%
  mutate(kilometros_ano_producto = mean(distance))

# Km recorridos por comida que entra por mes - año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, anio) %>%
  mutate(kilometros_mes = mean(distance))

# Cantidad de comida en Kg que entra por mes - año a medellin 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, anio,producto) %>%
  mutate(kilometros_mes_producto = mean(distance))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen) %>%
  mutate(kilometros_año_municipio = mean(distance))

# Cantidad de comida en Kg que entra por año - mes a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mes) %>%
  mutate(kilometros_año_mes_municipio = mean(distance))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,producto) %>%
  mutate(kilometros_año_municipio_producto = mean(distance))


# Cantidad de comida en Kg que entra por año - mes a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mes,producto) %>%
  mutate(kilometros_año_mes_municipio_producto = mean(distance))


# Tipo año
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo) %>%
  mutate(tipo_año= mean(distance))


# Tipo año - Mes
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo, mes) %>%
  mutate(tipo_año_mes= mean(distance))


# Tipo año prodicto 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo, producto) %>%
  mutate(tipo_año_producto= mean(distance))

# Tipo año prodicto 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo, producto, mes)%>%
  mutate(tipo_año_producto_mes= mean(distance))%>%
  ungroup()

#variables <- colnames(abastecimiento_medellin)

#variables <- variables[grepl("importancia",variables)]
#variables <- c(variables,"route","anio","mes","codigo_mpio_destino","codigo_mpio_origen")

#rutas <- abastecimiento_medellin[,c("codigo_mpio_destino","codigo_mpio_origen","route")]
#rutas <- rutas[!(duplicated(rutas[c("codigo_mpio_destino","codigo_mpio_origen")])),]

#abastecimiento_medellin <- abastecimiento_medellin[,-c(18)]

#saveRDS(rutas,"coordenadas.rds")
         
#abastecimiento_medellin <- abastecimiento_medellin[,variables]
#saveRDS(abastecimiento_medellin,"base_abastecimiento_geografico.rds")

#write.csv(cbind(abastecimiento_medellin[,1:17],abastecimiento_medellin[,19:ncol(abastecimiento_medellin)]), "C:/Users/tusig/OneDrive - Universidad EAFIT/FAO/03_Tableros/rutas_abastecimiento/data/base_abastecimiento_geografico.csv")

#####

# tiempo <- function(opcion1, opcion2 = NULL, opcion3 = NULL, opcion4 = NULL) {
#   df <- abastecimiento_medellin
#   
#   # Si opcion4 no es NULL, filtrar por año
#   if (!is.null(opcion4)) {
#     df <- df %>% filter(anio == opcion4)
#   }
#   
#   if (opcion1 == "total") {
#     df <- df %>%
#       distinct(anio, mes, .keep_all = TRUE) %>%
#       select(anio, mes, mes_y_ano, kilometros_mes)
#     
#     ggplot(df, aes(x = mes_y_ano, y = kilometros_mes)) +
#       geom_line() +
#       labs(title = "Kilómetros promedio que recorren los alimentos que ingresan a Medellín por mes") +
#       ylab("Kilómetros promedio que recorren los alimentos que ingresan a Medellín por mes") +
#       xlab("Información por meses") +
#       theme_minimal() +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  
#     
#   } else if (opcion1 == "mpio_origen") {
#     if (is.null(opcion2)) {
#       stop("Debe proporcionar un municipio cuando opcion1 es 'mpio_origen'")
#     }
#     
#     df <- df %>%
#       distinct(anio, mes, mpio_origen, .keep_all = TRUE) %>%
#       select(anio, mes, mes_y_ano, kilometros_año_mes_municipio, mpio_origen) %>%
#       filter(mpio_origen == opcion2)
#     
#     ggplot(df, aes(x = mes_y_ano, y = kilometros_año_mes_municipio)) +
#       geom_line() +
#       labs(title = "Kilónetros promedio por municipio por mes") +
#       ylab("Kilómetros promedio que recorren los alimentos queingresan a Medellín por mes") +
#       xlab("Información por meses") +
#       theme_minimal() +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#     
#   } else if (opcion1 == "producto") {
#     df <- df %>%
#       distinct(anio, mes, producto, .keep_all = TRUE) %>%
#       select(anio, mes, mes_y_ano, kilometros_mes_producto, producto)
#     
#     if (!is.null(opcion3)) {
#       df <- df %>%
#         filter(producto == opcion3)
#     }
#     
#     ggplot(df, aes(x = mes_y_ano, y = kilometros_mes_producto, color = producto)) +
#       geom_line() +
#       labs(title = paste("Kilómetros promedio que recorren ", opcion3, "por mes")) +
#       ylab("Kilómetros promedio") +
#       xlab("Información Mensual") +
#       theme_minimal() +
#       theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())  # Elimina la cuadrícula
#     
#   } else if (opcion1 == "mpio_origen_producto") {
#     if (is.null(opcion2) || is.null(opcion3)) {
#       stop("Debe proporcionar un municipio y un producto cuando opcion1 es 'mpio_origen_producto'")
#     }
#     
#     df <- df %>%
#       distinct(anio, mes, mpio_origen, producto, mes_y_ano, .keep_all = TRUE) %>%
#       select(anio, mes, mes_y_ano, kilometros_año_mes_municipio_producto, mpio_origen, producto) %>%
#       filter(mpio_origen == opcion2, producto == opcion3)
#     
#     ggplot(df, aes(x = mes_y_ano, y = kilometros_año_mes_municipio_producto, color = producto)) +
#       geom_line() +
#       labs(title = paste("Kilómetros promedio que recorren " , opcion3 , "que provienen del municipio de ", opcion2)) +
#       ylab("Kilómetros promedio") +
#       xlab("Información Mensual") +
#       theme_classic()  
#   }
# }

ruta <- function(opcion1,opcion2 = NULL, opcion3 = NULL,opcion4 = NULL) {
  df <- abastecimiento_medellin
  
  if (opcion2 != "" & opcion3 != "") {
    df <- df %>% filter(anio == opcion2 & mes == opcion3)
  } else if (opcion2 != "") {
    df <- df %>% filter(anio == opcion2)
  } else if (opcion3 != "") {
    df <- df %>% filter(mes == opcion3)
  }
  
  if (!is.null(opcion4)) {
    df <- df %>%
      filter(producto == opcion4)
  }
  
  df <- df[!(duplicated(df[c("codigo_mpio_destino","codigo_mpio_origen")])),]

  map <- leaflet() %>%
        addTiles()
    
  for(i in 1:nrow(df)) {
    dir <- matrix(unlist(df[i,18][[1]]), ncol = 2)
    map <- map %>% addPolylines(data = dir, color = "blue", stroke = 0.05, opacity = 0.8)
  }
  
  map
}

ruta_importancia <- function(opcion1,opcion2 = NULL, opcion3 = NULL,opcion4 = NULL) {
  df <- abastecimiento_medellin
  
  if (opcion2 != "" & opcion3 != "") {
    df <- df %>% filter(anio == opcion2 & mes == opcion3)
    
    if(!is.null(opcion4)) {
      var <- 39
    } else {
      var <- 34
    }
  } else if (opcion2 != "") {
    df <- df %>% filter(anio == opcion2)
    
    if(!is.null(opcion4)) {
      var <- 40
    } else {
      var <- 37
    }
  } else if (opcion3 != "") {
    df <- df %>% filter(mes == opcion3)
    
    if(!is.null(opcion4)) {
      var <- 41
    } else {
      var <- 38
    }
  } else {
    if(!is.null(opcion4)) {
      var <- 42
    } else {
      var <- 43
    }
  }
  
  if (!is.null(opcion4)) {
    df <- df %>%
      filter(producto == opcion4)
  }
  
  df <- df[!(duplicated(df[c("codigo_mpio_destino","codigo_mpio_origen")])),]
  
  colours <- colorRampPalette(c("red", "yellow","green"))(n_distinct(df[,var]))
  
  importancia_ordenada <- unique(df[,var])
  colnames(importancia_ordenada) <- "importancia"
  importancia_ordenada$importancia <- importancia_ordenada[order(importancia_ordenada$importancia),]
  
  aux <- data.frame(importancia = importancia_ordenada$importancia, 
                    colour = colours)
  
  colnames(df)[var] <- "importancia"
  
  df <- merge(x=df,y=aux,by="importancia")
  
  importancia_max <- round(max(df$importancia)*100,2)
  importancia_min <- round(min(df$importancia)*100,2)
  
  
  map <- leaflet() %>%
    addTiles() %>%
    addLegend(
      position = "bottomright",
      colors = colorRampPalette(c("red", "yellow","green"))(10),
      labels = c(importancia_min,"","","","","","","","",importancia_max),
      opacity = 1,
      title = "Importancia (%)"
    )
  
  for(i in 1:nrow(df)) {
    dir <- matrix(unlist(df[i,19][[1]]), ncol = 2)
    map <- map %>% addPolylines(data = dir, color = df$colour[i], stroke = 0.05, opacity = 0.8)
  }
  
  map
}

# 
# # Para obtener el total de kilogramos que ingresan a Medellín por mes
# tiempo("total")
# # Para obtener el total de kilogramos que ingresan a Medellín por mes de un municipio específico
# tiempo("mpio_origen", "ABEJORRAL")
# # Para obtener el total de kilogramos de un producto específico que ingresan a Medellín por mes
# tiempo("producto", "", "arroz")
# # Para obtener el total de kilogramos de un producto específico que ingresan a Medellín por mes de un municipio específico
# #tiempo("mpio_origen_producto", "MEDELLÍN", "arroz")
# # Para obtener el total de kilogramos que ingresan a Medellín por mes en un año específico
# tiempo("total", "" , "", 2018)
# # Para obtener el total de kilogramos que ingresan a Medellín por mes de un municipio específico en un año específico
# #tiempo("mpio_origen", "MEDELLÍN", "", 2021)
# # Para obtener el total de kilogramos de un producto específico que ingresan a Medellín por mes en un año específico
# tiempo("producto", "", "arroz", 2018)
# # Para obtener el total de kilogramos de un producto específico que ingresan a Medellín por mes de un municipio específico en un año específico
# #tiempo("mpio_origen_producto", "MEDELLÍN", "arroz", 2021)
# 
# #RUTAS
# #rutas de los alimentos que ingresan a medellin
# rutas("total",2018)
# #rutas de los alimentos que ingresan a medellin
# rutas("total",2018,"","arroz")
# 
# 
# 
# 
# df <- abastecimiento_medellin
# 
# df <- df %>% filter(anio == 2018)
# 
# df <- df %>% filter(producto == "arroz")
# 
# df <- df[!(duplicated(df[c("codigo_mpio_destino","codigo_mpio_origen")])),]
# 
# map <- leaflet() %>%
#   addTiles()
# 
# for(i in 1:nrow(df)) {
#   dir <- matrix(unlist(df[i,18][[1]]), ncol = 2)
#   map <- map %>% addPolylines(data = dir, color = "blue", stroke = 0.2, opacity = 0.8)
# }
# 
# map