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
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
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
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"),
         producto=str_to_lower(producto), suma_kg=suma_kg/1000,
         mpio_origen=tools::toTitleCase(tolower(mpio_origen)))

destino$mpio_origen[destino$mpio_origen=="Bogotá, D.c."]<-"Bogotá"

destino$mpio_destino[destino$mpio_destino=="Cartagena de Indias"]<-"Cartagena"

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# Calculamos  % de prodcto por muncicipio que entra a "Medellin" desde cualquier municipio
abastecimiento_medellin <- destino %>% filter( mpio_destino == 'Medellín')

# Creamos una variable que se llame interno o externo y depende si el abast es desde antioquia o fuera de antioquia 
abastecimiento_medellin  <- abastecimiento_medellin  %>%
  mutate(tipo = ifelse(startsWith(as.character(codigo_mpio_origen), "05"), "interno", "externo"))


# Cantidad de comida en Kg que entra a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  #group_by(anio) %>%
  mutate(total_kilogramos = sum(suma_kg))

# Cantidad de comida en Kg que entra a medellin (por producto)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(producto) %>%
  mutate(total_kilogramos_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(codigo_mpio_origen, mpio_origen) %>%
  mutate(total_kilogramos_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra a medellin de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(codigo_mpio_origen,mpio_origen,producto) %>%
  mutate(total_kilogramos_municipio_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio) %>%
  mutate(total_kilogramos_ano = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (por producto)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, producto) %>%
  mutate(total_kilogramos_ano_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por mes - año a medellin (todos los productos)
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, anio) %>%
  mutate(total_kilogramos_mes = sum(suma_kg))

# Cantidad de comida en Kg que entra por mes - año a medellin 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(mes, anio,producto) %>%
  mutate(total_kilogramos_mes_producto = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen, mpio_origen) %>%
  mutate(total_kilogramos_año_municipio = sum(suma_kg))


# Cantidad de comida en Kg que entra por año - mes a medellin (todos los productos) de x municipio
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mpio_origen,mes) %>%
  mutate(total_kilogramos_año_mes_municipio = sum(suma_kg))

# Cantidad de comida en Kg que entra por año a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mpio_origen,producto) %>%
  mutate(total_kilogramos_año_municipio_producto = sum(suma_kg))


# Cantidad de comida en Kg que entra por año - mes a medellin (todos los productos) de x municipio - producto
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, codigo_mpio_origen,mpio_origen,mes,producto) %>%
  mutate(total_kilogramos_año_mes_municipio_producto = sum(suma_kg))


# Tipo año
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo) %>%
  mutate(tipo_año= sum(suma_kg))


# Tipo año - Mes
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo, mes) %>%
  mutate(tipo_año_mes= sum(suma_kg))


# Tipo año prodicto 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo, producto) %>%
  mutate(tipo_año_producto= sum(suma_kg))

# Tipo año prodicto 
abastecimiento_medellin <-abastecimiento_medellin %>%
  group_by(anio, tipo, producto, mes)%>%
  mutate(tipo_año_producto_mes= sum(suma_kg))%>%
  ungroup()


## Creamos los indices (valor en %)
# Mensual: 
# Tenemos total de kilogramos por mes y por producto 
# Total kilogarmos por municipio (%) para evaluar la importancia de  cada municipio
abastecimiento_medellin$mes_municipio_porcentaje <- abastecimiento_medellin$total_kilogramos_año_mes_municipio/abastecimiento_medellin$total_kilogramos_mes  
# Total Kilogramo por municipio y producto (%) para evaluar la importancia de cada municipio por producto  
abastecimiento_medellin$mes_municipio_producto_porcentaje <- abastecimiento_medellin$total_kilogramos_año_mes_municipio_producto/abastecimiento_medellin$total_kilogramos_mes_producto  
#Cuanto viene de afuera del departamento por mes, todos los productos 
# % de alimento interno vs % de alimento externo por año y por mes
abastecimiento_medellin$mes_tipo_porcentaje <- abastecimiento_medellin$tipo_año_mes /abastecimiento_medellin$total_kilogramos_mes 
#Cuanto viene de afuera del departamento por mes, por producto 
# % de alimento interno vs % de alimento externo por mes para cada uno de los productps 
abastecimiento_medellin$mes_tipo_producto_porcentaje <- abastecimiento_medellin$tipo_año_producto_mes /abastecimiento_medellin$total_kilogramos_mes_producto 




## Creamos los indices (valor en %)
# Anual: 
# Tenemos total de kilogramos por año y producto 
# Total kilogarmos por municipio (%) para evaluar la importancia de  cada municipio por año
abastecimiento_medellin$año_municipio_porcentaje <- abastecimiento_medellin$total_kilogramos_año_municipio/abastecimiento_medellin$total_kilogramos_ano 
# Total Kilogramo por municipio y producto (%) para evaluar la importancia de cada municipio por producto  
abastecimiento_medellin$año_municipio_producto_porcentaje <- abastecimiento_medellin$total_kilogramos_año_municipio_producto/abastecimiento_medellin$total_kilogramos_ano_producto
#Cuanto viene de afuera del departamento por año, todos los productos 
# % de alimento interno vs % de alimento externo por año 
abastecimiento_medellin$año_tipo_porcentaje <- abastecimiento_medellin$tipo_año /abastecimiento_medellin$total_kilogramos_ano 
#Cuanto viene de afuera del departamento por año, por producto 
# % de alimento interno vs % de alimento externo por año para cada uno de los productps 
abastecimiento_medellin$año_tipo_producto_porcentaje <- abastecimiento_medellin$tipo_año_producto/abastecimiento_medellin$total_kilogramos_ano_producto

## Creamos los indices (valor en %)
# Total datos: 
# kg por municipio destino en todos los años posibles
abastecimiento_medellin$municipio_porcentaje<-abastecimiento_medellin$total_kilogramos_municipio/abastecimiento_medellin$total_kilogramos
# kg por municipio destino según producto
abastecimiento_medellin$municipio_producto_porcentaje<-abastecimiento_medellin$total_kilogramos_municipio_producto/abastecimiento_medellin$total_kilogramos_producto
# kg por producto enviado de Antioquia
abastecimiento_medellin$producto_porcentaje<-abastecimiento_medellin$total_kilogramos_producto/abastecimiento_medellin$total_kilogramos

abastecimiento_medellin<-abastecimiento_medellin%>%
  select(mpio_origen,producto,anio,mes,mes_y_ano,c(19,20,22,24,29:39))

saveRDS(abastecimiento_medellin, "02_Indices/Output/base_indices_abastecimiento.rds")

