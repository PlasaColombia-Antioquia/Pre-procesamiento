#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 25/02/2024
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

#Subir base de datos completa

municipios_dane <- read_excel("01_Datos/Data/Bases_auxiliares/Input/municipios_dane.xlsx")%>%
  select(Municipio, Departamento, cod_depto)

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
  left_join(municipios_dane, by = c("mpio_destino" = "Municipio"))%>%
  mutate(suma_kg=suma_kg/1000)

destino$mpio_destino[destino$mpio_destino=="Cartagena de Indias"]<-"Cartagena"


destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

# Cantidad de comida en Kg que sale por dpto (todos los productos)
antioquia <- destino %>%
  group_by(codigo_depto_destino) %>%
  mutate(total_kilogramos = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto según destino (todos los productos)
antioquia <- antioquia %>%
  group_by(depto_origen, codigo_depto_destino) %>%
  mutate(total_kilogramos_destino = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto (c/u de los productos)
antioquia <- antioquia %>%
  group_by(producto, codigo_depto_destino) %>%
  mutate(total_kilogramos_prod = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto según destino (c/u de los productos)
antioquia <- antioquia %>%
  group_by(producto, depto_origen, codigo_depto_destino) %>%
  mutate(total_kilogramos_destino_prod = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto (todos los productos) por año
antioquia <- antioquia %>%
  group_by(codigo_depto_destino, anio) %>%
  mutate(total_kilogramos_ano = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto según destino (todos los productos) por año
antioquia <- antioquia %>%
  group_by(depto_origen, anio, codigo_depto_destino) %>%
  mutate(total_kilogramos_ano_destino = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto (c/u de los productos) por año
antioquia <- antioquia %>%
  group_by(producto, codigo_depto_destino, anio) %>%
  mutate(total_kilogramos_ano_prod = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto según destino (c/u de los productos)por año
antioquia <- antioquia %>%
  group_by(producto, depto_origen, anio, codigo_depto_destino) %>%
  mutate(total_kilogramos_ano_destino_prod = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto (todos los productos) por mes año
antioquia <- antioquia %>%
  group_by(codigo_depto_destino, mes_y_ano) %>%
  mutate(total_kilogramos_mes = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto según destino (todos los productos) por mes año
antioquia <- antioquia %>%
  group_by(depto_origen, mes_y_ano, codigo_depto_destino) %>%
  mutate(total_kilogramos_mes_destino = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto (c/u de los productos) por mes año
antioquia <- antioquia %>%
  group_by(producto, codigo_depto_destino, mes_y_ano) %>%
  mutate(total_kilogramos_mes_prod = sum(suma_kg))

# Cantidad de comida en Kg que sale por dpto según destino (c/u de los productos) por mes año
antioquia <- antioquia %>%
  group_by(producto, depto_origen, mes_y_ano, codigo_depto_destino) %>%
  mutate(total_kilogramos_mes_destino_prod = sum(suma_kg))%>%
  ungroup()

#Crear las variables porcentuales

# % de lo que sale de cada dpto que va hacia Medellín - Ant total
antioquia$porcentaje_dpto<-antioquia$total_kilogramos_destino/antioquia$total_kilogramos

# % de lo que sale de cada dpto que va hacia Medellín - Ant total por producto
antioquia$porcentaje_dpto_prod<-antioquia$total_kilogramos_destino_prod/antioquia$total_kilogramos_prod

# % de lo que sale de cada dpto que va hacia Medellín - Ant anualmente total
antioquia$porcentaje_dpto_anio<-antioquia$total_kilogramos_ano_destino/antioquia$total_kilogramos_ano

# % de lo que sale de cada dpto que va hacia Medellín - Ant anualmente por producto
antioquia$porcentaje_dpto_anio_producto<-antioquia$total_kilogramos_ano_destino_prod/antioquia$total_kilogramos_ano_prod

# % de lo que sale de cada dpto que va hacia Medellín - Ant mensual total
antioquia$porcentaje_dpto_mes<-antioquia$total_kilogramos_mes_destino/antioquia$total_kilogramos_mes

# % de lo que sale de cada dpto que va hacia Medellín - Ant mensual por producto
antioquia$porcentaje_dpto_mes_producto<-antioquia$total_kilogramos_mes_destino_prod/antioquia$total_kilogramos_mes_prod

antioquia<-antioquia%>%select(anio,codigo_depto_destino,depto_origen, mes,mpio_destino, producto,c(27:32))%>%
  filter(depto_origen == "ANTIOQUIA")

saveRDS(antioquia, "02_Indices/Output/base_indices_cadena_antioquia.rds")




