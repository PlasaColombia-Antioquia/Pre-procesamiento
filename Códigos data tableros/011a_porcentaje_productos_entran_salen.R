#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 07/04/2024
#Fecha de ultima modificacion: 08/04/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
options(scipen = 999)
################################################################################-

#Subir base de datos completa

municipios_dane <- read_excel("01_Datos/Data/Bases_auxiliares/Input/municipios_dane.xlsx")%>%
  mutate(cod_mpio = as.numeric(paste0(cod_depto,cod_muni)))%>%
  select(Municipio, Departamento, cod_depto, cod_mpio)

municipios_dane$Municipio[municipios_dane$Municipio=="Bogotá, D.C."] = "Bogotá"

destino <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.csv")%>%
  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%select(-"...1")%>%
  rename(producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto=str_to_lower(producto),
         codigo_depto_destino = as.numeric(str_sub(codigo_mpio_destino, end = -4)))%>%
  left_join(municipios_dane, by = c("codigo_mpio_destino" = "cod_mpio"))%>%
  mutate(suma_kg=suma_kg/1000)%>%filter(mpio_destino=="Medellín")

destino$depto_origen[destino$depto_origen=="BOGOTÁ, D. C."]<-"BOGOTÁ, D.C."
destino$depto_origen[destino$depto_origen=="BOGOTÁ, D.C."]<-"BOGOTÁ"

destino$depto_origen <- str_to_title(destino$depto_origen)
destino$producto <- str_to_title(destino$producto)

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# PRODUCTOS ENTRAN----

# Cantidad de toneladas que llegan
producto_entra<-destino%>%
  #group_by(producto)%>%
  mutate(toneladas_total=sum(suma_kg))
  
# Cantidad de toneladas que llegan por producto
producto_entra<-producto_entra%>%
  group_by(producto)%>%
  mutate(toneladas_total_producto=sum(suma_kg))

# Cantidad de toneladas que llegan por año
producto_entra<-producto_entra%>%
  group_by(anio)%>%
  mutate(toneladas_total_anio=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año
producto_entra<-producto_entra%>%
  group_by(producto, anio)%>%
  mutate(toneladas_total_producto_anio=sum(suma_kg))

# Cantidad de toneladas que llegan por año por mes
producto_entra<-producto_entra%>%
  group_by(anio, mes)%>%
  mutate(toneladas_total_anio_mes=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año por mes
producto_entra<-producto_entra%>%
  group_by(producto, anio, mes)%>%
  mutate(toneladas_total_producto_anio_mes=sum(suma_kg))

# Cantidad de toneladas que llegan por año por mes por dpto de procedencia
producto_entra<-producto_entra%>%
  group_by(anio, mes, depto_origen)%>%
  mutate(toneladas_total_anio_mes_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año por mes por dpto de procedencia
producto_entra<-producto_entra%>%
  group_by(producto, anio, mes, depto_origen)%>%
  mutate(toneladas_total_producto_anio_mes_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por año por dpto de procedencia
producto_entra<-producto_entra%>%
  group_by(anio, depto_origen)%>%
  mutate(toneladas_total_anio_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año por dpto de procedencia
producto_entra<-producto_entra%>%
  group_by(producto, anio, depto_origen)%>%
  mutate(toneladas_total_producto_anio_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por dpto de procedencia
producto_entra<-producto_entra%>%
  group_by(depto_origen)%>%
  mutate(toneladas_total_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por dpto de procedencia
producto_entra<-producto_entra%>%
  group_by(producto, depto_origen)%>%
  mutate(toneladas_total_producto_dpto=sum(suma_kg))%>%
  ungroup()

# Crear los porcentajes
# Porcentaje que llega por producto
producto_entra<-producto_entra%>%
  mutate(porcentaje_producto=toneladas_total_producto/toneladas_total)

# Porcentaje que llega por producto por año
producto_entra<-producto_entra%>%
  mutate(porcentaje_producto_anio=toneladas_total_producto_anio/toneladas_total_anio)

# Porcentaje que llega por producto por año por mes
producto_entra<-producto_entra%>%
  mutate(porcentaje_producto_anio_mes=toneladas_total_producto_anio_mes/toneladas_total_anio_mes)

# Porcentaje que llega por producto por año por mes por dpto de procedencia
producto_entra<-producto_entra%>%
  mutate(porcentaje_producto_anio_mes_dpto=toneladas_total_producto_anio_mes_dpto/toneladas_total_anio_mes_dpto)

# Porcentaje que llega por producto por año por dpto de procedencia
producto_entra<-producto_entra%>%
  mutate(porcentaje_producto_anio_dpto=toneladas_total_producto_anio_dpto/toneladas_total_anio_dpto)

# Porcentaje que llega por producto por dpto de procedencia
producto_entra<-producto_entra%>%
  mutate(porcentaje_producto_dpto=toneladas_total_producto_dpto/toneladas_total_dpto)%>%
  select(anio, mes, producto, depto_origen, c(27:33), grupo_alimento)


saveRDS(producto_entra, "02_Indices/Output/base_porcentaje_productos_entran.rds")


# PRODUCTOS SALES----

destino <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.csv")%>%
  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%select(-"...1")%>%
  rename(producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto=str_to_lower(producto),
         codigo_depto_destino = as.numeric(str_sub(codigo_mpio_destino, end = -4)))%>%
  left_join(municipios_dane, by = c("codigo_mpio_destino" = "cod_mpio"))%>%
  mutate(suma_kg=suma_kg/1000)%>%filter(depto_origen=="ANTIOQUIA")

destino$depto_origen <- str_to_title(destino$depto_origen)
destino$producto <- str_to_title(destino$producto)

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# Cantidad de toneladas que llegan
producto_sale<-destino%>%
  #group_by(producto)%>%
  mutate(toneladas_total=sum(suma_kg))

# Cantidad de toneladas que llegan por producto
producto_sale<-producto_sale%>%
  group_by(producto)%>%
  mutate(toneladas_total_producto=sum(suma_kg))

# Cantidad de toneladas que llegan por año
producto_sale<-producto_sale%>%
  group_by(anio)%>%
  mutate(toneladas_total_anio=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año
producto_sale<-producto_sale%>%
  group_by(producto, anio)%>%
  mutate(toneladas_total_producto_anio=sum(suma_kg))

# Cantidad de toneladas que llegan por año por mes
producto_sale<-producto_sale%>%
  group_by(anio, mes)%>%
  mutate(toneladas_total_anio_mes=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año por mes
producto_sale<-producto_sale%>%
  group_by(producto, anio, mes)%>%
  mutate(toneladas_total_producto_anio_mes=sum(suma_kg))

# Cantidad de toneladas que llegan por año por mes por dpto de procedencia
producto_sale<-producto_sale%>%
  group_by(anio, mes, mpio_destino)%>%
  mutate(toneladas_total_anio_mes_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año por mes por dpto de procedencia
producto_sale<-producto_sale%>%
  group_by(producto, anio, mes, mpio_destino)%>%
  mutate(toneladas_total_producto_anio_mes_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por año por dpto de procedencia
producto_sale<-producto_sale%>%
  group_by(anio, mpio_destino)%>%
  mutate(toneladas_total_anio_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por año por dpto de procedencia
producto_sale<-producto_sale%>%
  group_by(producto, anio, mpio_destino)%>%
  mutate(toneladas_total_producto_anio_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por dpto de procedencia
producto_sale<-producto_sale%>%
  group_by(mpio_destino)%>%
  mutate(toneladas_total_dpto=sum(suma_kg))

# Cantidad de toneladas que llegan por producto por dpto de procedencia
producto_sale<-producto_sale%>%
  group_by(producto, mpio_destino)%>%
  mutate(toneladas_total_producto_dpto=sum(suma_kg))%>%
  ungroup()

# Crear los porcentajes
# Porcentaje que llega por producto
producto_sale<-producto_sale%>%
  mutate(porcentaje_producto=toneladas_total_producto/toneladas_total)

# Porcentaje que llega por producto por año
producto_sale<-producto_sale%>%
  mutate(porcentaje_producto_anio=toneladas_total_producto_anio/toneladas_total_anio)

# Porcentaje que llega por producto por año por mes
producto_sale<-producto_sale%>%
  mutate(porcentaje_producto_anio_mes=toneladas_total_producto_anio_mes/toneladas_total_anio_mes)

# Porcentaje que llega por producto por año por mes por dpto de procedencia
producto_sale<-producto_sale%>%
  mutate(porcentaje_producto_anio_mes_dpto=toneladas_total_producto_anio_mes_dpto/toneladas_total_anio_mes_dpto)

# Porcentaje que llega por producto por año por dpto de procedencia
producto_sale<-producto_sale%>%
  mutate(porcentaje_producto_anio_dpto=toneladas_total_producto_anio_dpto/toneladas_total_anio_dpto)

# Porcentaje que llega por producto por dpto de procedencia
producto_sale<-producto_sale%>%
  mutate(porcentaje_producto_dpto=toneladas_total_producto_dpto/toneladas_total_dpto)%>%
  select(anio, mes, producto, mpio_destino, c(27:33), grupo_alimento)


saveRDS(producto_sale, "02_Indices/Output/base_porcentaje_productos_salen.rds")
