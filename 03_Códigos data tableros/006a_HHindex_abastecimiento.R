#Proyecto FAO
#INDICE Herfindahl–Hirschman - Abastecimiento tablero 1 
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse)
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

destino <- destino %>% filter(codigo_mpio_destino == "5001")


# Creamos un indice total para todos los productos 
data_total <- destino %>%
  group_by(producto) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()

total <- sum(data_total$suma_producto)

data_total$participacion <- data_total$suma_producto/total
data_total$IHH_total <- sum((data_total$participacion)^2)

IHH_total <- data_total %>%
  summarise(IHH = sum((participacion)^2))



# Informacion Mensual 

data_mensual <- destino %>%
  group_by(mes_y_ano, producto) %>%
  summarise(suma_mensual = sum(suma_kg)) %>%
  ungroup()

data_mensual <- data_mensual %>%
  group_by(mes_y_ano) %>%
  mutate(suma_total_mensual = sum(suma_mensual))

data_mensual$participacion <- (data_mensual$suma_mensual/data_mensual$suma_total_mensual)

data_mensual$mes_y_ano <- as.Date(data_mensual$mes_y_ano, format = "%Y-%m-%d")
data_mensual$year <- as.numeric(substr(data_mensual$`mes_y_ano`, 1, 4))
data_mensual$month <- as.numeric(substr(data_mensual$`mes_y_ano`, 6, 7))

# Queremos medir el nivel de concentracion de los productos 
IHH_mensual <- data_mensual %>%
  group_by(mes_y_ano) %>%
  summarise(IHH = sum((participacion)^2)) %>%
  ungroup()

IHH_mensual$mes_y_ano <- as.Date(IHH_mensual$mes_y_ano, format = "%Y-%m-%d")
IHH_mensual$year <- as.numeric(substr(IHH_mensual$mes_y_ano, 1, 4))
IHH_mensual$month <- as.numeric(substr(IHH_mensual$mes_y_ano, 6, 7))
IHH_mensual$month <- month(IHH_mensual$month, label = TRUE)



# Ahora construimos la base de datos anual
data_anual <- destino %>%
  group_by(anio, producto) %>%
  summarise(suma_anual = sum(suma_kg)) %>%
  ungroup()

data_anual <- data_anual %>%
  group_by(anio) %>%
  mutate(suma_total_anual = sum(suma_anual))

data_anual$participacion <- (data_anual$suma_anual/data_anual$suma_total_anual)

IHH_anual <- data_anual %>%
  group_by(anio) %>%
  summarise(IHH = sum((participacion)^2))
IHH_anual <- rename(IHH_anual, year = anio)


saveRDS(IHH_anual,"02_Indices/Output/IHH_anual_abastecimiento.rds")
saveRDS(IHH_mensual,"02_Indices/Output/IHH_mensual_abastecimiento.rds")
saveRDS(IHH_total,"02_Indices/Output/IHH_total_abastecimiento.rds")

