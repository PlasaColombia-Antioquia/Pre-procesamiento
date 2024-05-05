#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 01/04/2024
#Fecha de ultima modificacion: 02/04/2024
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
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate( suma_kg=suma_kg/1000)

destino$mpio_destino[destino$mpio_destino=="Cartagena de Indias"]<-"Cartagena"

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# Calculamos  % de prodcto por muncicipio que sale a "Medellin" desde cualquier municipio
proviene_antioquia <- destino %>% filter( depto_origen == 'ANTIOQUIA')

# Creamos una variable que se llame interno o externo y depende si el abast es desde antioquia o fuera de antioquia 
proviene_antioquia  <- proviene_antioquia  %>%
  mutate(tipo = ifelse(startsWith(as.character(codigo_mpio_destino), "5"), "interno", "externo"))


# Cantidad de comida en Kg que sale de Antioquia (todos los productos)
proviene_antioquia <-proviene_antioquia %>%
  #group_by(anio) %>%
  mutate(total_toneladas = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (por producto)
proviene_antioquia <-proviene_antioquia %>%
  group_by(producto) %>%
  mutate(total_toneladas_producto = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (todos los productos) de x municipio
proviene_antioquia <-proviene_antioquia %>%
  group_by(codigo_mpio_destino, mpio_destino) %>%
  mutate(total_toneladas_municipio = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (todos los productos) de x municipio - producto
proviene_antioquia <-proviene_antioquia %>%
  group_by(codigo_mpio_destino, mpio_destino,producto) %>%
  mutate(total_toneladas_municipio_producto = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (todos los productos)
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio) %>%
  mutate(total_toneladas_anio = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (por producto)
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, producto) %>%
  mutate(total_toneladas_anio_producto = sum(suma_kg))

# Cantidad de comida en Kg que sale por mes - año a medellin (todos los productos)
proviene_antioquia <-proviene_antioquia %>%
  group_by(mes, anio) %>%
  mutate(total_toneladas_mes = sum(suma_kg))

# Cantidad de comida en Kg que sale por mes - año a medellin 
proviene_antioquia <-proviene_antioquia %>%
  group_by(mes, anio,producto) %>%
  mutate(total_toneladas_mes_producto = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (todos los productos) de x municipio
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, codigo_mpio_destino, mpio_destino) %>%
  mutate(total_toneladas_anio_municipio = sum(suma_kg))


# Cantidad de comida en Kg que sale por año - mes a medellin (todos los productos) de x municipio
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, codigo_mpio_destino, mpio_destino,mes) %>%
  mutate(total_toneladas_anio_mes_municipio = sum(suma_kg))

# Cantidad de comida en Kg que sale de Antioquia (todos los productos) de x municipio - producto
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, codigo_mpio_destino, mpio_destino,producto) %>%
  mutate(total_toneladas_anio_municipio_producto = sum(suma_kg))


# Cantidad de comida en Kg que sale por año - mes a medellin (todos los productos) de x municipio - producto
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, codigo_mpio_destino, mpio_destino,mes,producto) %>%
  mutate(total_toneladas_anio_mes_municipio_producto = sum(suma_kg))


# Tipo año
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, tipo) %>%
  mutate(tipo_anio= sum(suma_kg))


# Tipo año - Mes
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, tipo, mes) %>%
  mutate(tipo_anio_mes= sum(suma_kg))


# Tipo año prodicto 
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, tipo, producto) %>%
  mutate(tipo_anio_producto= sum(suma_kg))

# Tipo año prodicto 
proviene_antioquia <-proviene_antioquia %>%
  group_by(anio, tipo, producto, mes)%>%
  mutate(tipo_anio_producto_mes= sum(suma_kg))%>%
  ungroup()


## Creamos los indices (valor en %)
# Mensual: 
# Tenemos total de toneladas por mes y por producto 
# Total kilogarmos por municipio (%) para evaluar la importancia de  cada municipio
proviene_antioquia$mes_municipio_porcentaje <- proviene_antioquia$total_toneladas_anio_mes_municipio/proviene_antioquia$total_toneladas_mes  
# Total Kilogramo por municipio y producto (%) para evaluar la importancia de cada municipio por producto  
proviene_antioquia$mes_municipio_producto_porcentaje <- proviene_antioquia$total_toneladas_anio_mes_municipio_producto/proviene_antioquia$total_toneladas_mes_producto  
#Cuanto va de afuera del departamento por mes, todos los productos 
# % de alimento interno vs % de alimento externo por año y por mes
proviene_antioquia$mes_tipo_porcentaje <- proviene_antioquia$tipo_anio_mes /proviene_antioquia$total_toneladas_mes 
#Cuanto va de afuera del departamento por mes, por producto 
# % de alimento interno vs % de alimento externo por mes para cada uno de los productps 
proviene_antioquia$mes_tipo_producto_porcentaje <- proviene_antioquia$tipo_anio_producto_mes /proviene_antioquia$total_toneladas_mes_producto 




## Creamos los indices (valor en %)
# Anual: 
# Tenemos total de toneladas por año y producto 
# Total kilogarmos por municipio (%) para evaluar la importancia de  cada municipio por año
proviene_antioquia$anio_municipio_porcentaje <- proviene_antioquia$total_toneladas_anio_municipio/proviene_antioquia$total_toneladas_anio 
# Total Kilogramo por municipio y producto (%) para evaluar la importancia de cada municipio por producto  
proviene_antioquia$anio_municipio_producto_porcentaje <- proviene_antioquia$total_toneladas_anio_municipio_producto/proviene_antioquia$total_toneladas_anio_producto
#Cuanto va de afuera del departamento por año, todos los productos 
# % de alimento interno vs % de alimento externo por año 
proviene_antioquia$anio_tipo_porcentaje <- proviene_antioquia$tipo_anio /proviene_antioquia$total_toneladas_anio 
#Cuanto va de afuera del departamento por año, por producto 
# % de alimento interno vs % de alimento externo por año para cada uno de los productps 
proviene_antioquia$anio_tipo_producto_porcentaje <- proviene_antioquia$tipo_anio_producto/proviene_antioquia$total_toneladas_anio_producto

## Creamos los indices (valor en %)
# Total datos: 
# kg por municipio destino en todos los años posibles
proviene_antioquia$municipio_porcentaje<-proviene_antioquia$total_toneladas_municipio/proviene_antioquia$total_toneladas
# kg por municipio destino según producto
proviene_antioquia$municipio_producto_porcentaje<-proviene_antioquia$total_toneladas_municipio_producto/proviene_antioquia$total_toneladas_producto
# kg por producto enviado de Antioquia
proviene_antioquia$producto_porcentaje<-proviene_antioquia$total_toneladas_producto/proviene_antioquia$total_toneladas

proviene_antioquia<-proviene_antioquia%>%
  select(anio, mes, mes_y_ano, mpio_destino, producto, c(16:24,29:39))

saveRDS(proviene_antioquia, "02_Indices/Output/base_indices_sale_antioquia.rds")


