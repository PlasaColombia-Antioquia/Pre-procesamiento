#Proyecto FAO
#INDICE Herfindahl–Hirschman - Exportaciones 1 - Hacia donde va la comida que se produce en Antioquia
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse); library(knitr); library(kableExtra);library(DT)
options(scipen = 999)
################################################################################
rm(list = ls())

## Cargamos la base de datos de origen destino 

destino <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.csv")%>%
  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%select(-"...1")%>%
  rename(producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto = str_to_upper(producto))

destino$mpio_destino[destino$mpio_destino=="Cartagena de Indias"]<-"Cartagena"

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# Filtramos por los productos con origen en Antioquia
destino <- destino %>% filter(depto_origen == "ANTIOQUIA")

# INFORMACION GENERAL 
# TENIENDO EN CUENTA TODOS LOS AÑOS 

data_total <- destino %>% 
  group_by(mpio_destino)%>%
  summarise(total_municipio = sum(suma_kg)) %>%
  ungroup()

total_produccion <- sum(data_total$total_municipio)
data_total$participacion <- data_total$total_municipio / total_produccion 

IHH_total <- data_total %>%
  summarise(IHH = sum((participacion)^2))

# INFORMACION GENERAL
#TENIENDO EN CUENTA TODOS LOS AÑOS - POR PRODUCTO
data_total_producto  <- destino %>%
  group_by(mpio_destino, producto)%>%
  summarise(total_municipio_producto = sum(suma_kg)) %>%
  ungroup()

total_producto <- destino %>%
  group_by(producto) %>%
  summarise(total_producto = sum(suma_kg))%>%
  ungroup()

data_total_producto <- merge(data_total_producto,total_producto , by = "producto")
data_total_producto$participacion <- data_total_producto$total_municipio_producto / data_total_producto$total_producto

IHH_total_producto <- data_total_producto %>%
  group_by(producto) %>%
  summarise(IHH = sum((participacion)^2))%>%
  ungroup()


# BASE 3:
# INFORMACION MENSUAL
# SIN DISRIMINAR POR PRODUCTO 

data_mensual_total <- destino %>%
  group_by(mes_y_ano, mpio_destino) %>%
  summarise(suma_mensual = sum(suma_kg))%>%
  ungroup()
data_mensual_aux <- destino %>%
  group_by(mes_y_ano) %>%
  summarise(suma_total_mes = sum(suma_kg))
data_mensual_total <- merge(data_mensual_total, data_mensual_aux, by = "mes_y_ano")
data_mensual_total$participacion <- data_mensual_total$suma_mensual/data_mensual_total$suma_total_mes
data_mensual_total$mes_y_ano <- as.Date(data_mensual_total$mes_y_ano, format = "%Y-%m-%d")
data_mensual_total$year <- as.numeric(substr(data_mensual_total$`mes_y_ano`, 1, 4))
data_mensual_total$month <- as.numeric(substr(data_mensual_total$`mes_y_ano`, 6, 7))

# Queremos medir el nivel de concentracion de los productos 
IHH_mensual_total <- data_mensual_total %>%
  group_by(mes_y_ano) %>%
  summarise(IHH = sum((participacion)^2)) %>%
  ungroup()
IHH_mensual_total$mes_y_ano <- as.Date(IHH_mensual_total$mes_y_ano, format = "%Y-%m-%d")
IHH_mensual_total$year <- as.numeric(substr(IHH_mensual_total$`mes_y_ano`, 1, 4))
IHH_mensual_total$month <- as.numeric(substr(IHH_mensual_total$`mes_y_ano`, 6, 7))


#BASE 4 
# INFORMACION MENSUAL
# DISRIMINANDO POR PRODUCTO 

data_mensual_producto <- destino %>%
  group_by(mes_y_ano, mpio_destino, producto) %>%
  summarise(suma_mensual = sum(suma_kg))

data_mensual_producto_aux <- destino %>%
  group_by(mes_y_ano,producto) %>%
  summarise(suma_total_mensual = sum(suma_kg))

data_mensual_producto <- merge(data_mensual_producto , data_mensual_producto_aux, by = c("mes_y_ano" , "producto"))
data_mensual_producto$mes_y_ano <- as.Date(data_mensual_producto$mes_y_ano, format = "%Y-%m-%d")
data_mensual_producto$year <- as.numeric(substr(data_mensual_producto$`mes_y_ano`, 1, 4))
data_mensual_producto$month <- as.numeric(substr(data_mensual_producto$`mes_y_ano`, 6, 7))
data_mensual_producto$participacion <- data_mensual_producto$suma_mensual/data_mensual_producto$suma_total_mensual

# Queremos medir el nivel de concentracion de los productos 
IHH_mensual_producto <- data_mensual_producto %>%
  group_by(mes_y_ano, producto) %>%
  summarise(IHH = sum((participacion)^2))
IHH_mensual_producto$mes_y_ano <- as.Date(IHH_mensual_producto$mes_y_ano, format = "%Y-%m-%d")
IHH_mensual_producto$year <- as.numeric(substr(IHH_mensual_producto$`mes_y_ano`, 1, 4))
IHH_mensual_producto$month <- as.numeric(substr(IHH_mensual_producto$`mes_y_ano`, 6, 7))


#BASE 5 
# INFORMACION ANUAL
# SIN DISRIMINAR POR PRODUCTO

data_anual_todas <- destino %>%
  group_by(anio, mpio_destino) %>%
  summarise(suma_anual= sum(suma_kg))
data_anual_todas_aux <- destino %>%
  group_by(anio) %>%
  summarise(suma_total_anual = sum(suma_kg))
data_anual_todas <- merge(data_anual_todas , data_anual_todas_aux, by = c("anio"))
data_anual_todas$participacion <-data_anual_todas$suma_anual/data_anual_todas$suma_total_anual
data_anual_todas <- rename(data_anual_todas, year = anio)
# Queremos medir el nivel de concentracion de los productos 
IHH_anual_total <- data_anual_todas %>%
  group_by(year) %>%
  summarise(IHH = sum((participacion)^2))


#BASE 6 
# INFORMACION ANUAL
# DISCRIMINANDO POR PRODUCTO

data_anual_producto <- destino %>%
  group_by(anio, mpio_destino,producto) %>%
  summarise(suma_anual = sum(suma_kg)) %>%
  ungroup()
data_anual_producto_aux <- destino %>%
  group_by(anio,producto) %>%
  summarise(suma_tottal_anual = sum(suma_kg)) %>%
  ungroup()
data_anual_producto <- merge(data_anual_producto , data_anual_producto_aux , by = c("anio","producto"))
data_anual_producto$participacion <- data_anual_producto$suma_anual/data_anual_producto$suma_tottal_anual

data_anual_producto <- rename(data_anual_producto, year = anio)

# Queremos medir el nivel de concentracion de los productos 
IHH_anual_producto <- data_anual_producto %>%
  group_by(year, producto) %>%
  summarise(IHH = sum((participacion)^2))


# Exportamos las bases a csv
# Indice vulnerabilidad anual para todos los productos 
# Guardar el dataframe en un archivo .RData
saveRDS(IHH_anual_total, "02_Indices/Output/base_indice_anual_total_destino.rds")
saveRDS(IHH_anual_producto, "02_Indices/Output/base_indice_anual_producto_desti.rds")
saveRDS(IHH_mensual_producto,  "02_Indices/Output/base_indice_mensual_producto_destino.rds")
saveRDS(IHH_mensual_total, "02_Indices/Output/base_indice_mensual_total_destino.rds")




