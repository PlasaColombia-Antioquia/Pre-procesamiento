#Proyecto FAO
#INDICE DE VULNERABILIDAD 
################################################################################
#Autores: Juan Carlos, Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 14/03/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(haven); library(DT);
options(scipen = 999)
################################################################################
rm(list = ls())
source("02_Indices/Src/007a_HHINDEX_participacion_municipios.R")
distancias <- readRDS("01_Datos/Data/Bases_auxiliares/Input/distancias.rds")
distancias <- as.data.frame(distancias)
distancias <- distancias[,"05001", drop = FALSE]

# ARGENTINA   01    7095
# BOLIVIA     02    5425
# BRASIL      03    3460 
# CANADA      04    6168
# CHILE       05    6469
# CHINA       06    15323
# ECUADOR     07    994
# ESTADOS UNIDOS 08 3989
# MEXICO      09    2942
# PAISES BAJOS 010  8859
# PERU         011  1460
# URUGUAY      012  4816
# VENEZUELA    013  1061

dpais <- data.frame("01" = 7095, "02" = 5425, "03" = 3460,"04" = 6168, "05" = 6469 , "06" = 15323 , "07" = 994 , "08" = 3989 , "09" = 2942 , "010" = 8859 , "011" = 1460, "012" = 4816, "013" = 1061)
dpais <- as.data.frame(t(dpais))
colnames(dpais)[1] <- "05001"
# Distancias incluyendo proxi a otros paises
distancias <- rbind(distancias, dpais)
distancias$codigo_mpio_origen <- rownames(distancias)
# En la base de destino incluimos los nuevos codigos para otros paises
# codigo_mpio_origen
# mpio_origen
# Cambiar los valores de "codigo_mpio_origen" a "X1" donde "mpio_origen" es "ARGENTINA"
destino$codigo_mpio_origen[destino$mpio_origen == "ARGENTINA"] <- "X01"
destino$codigo_mpio_origen[destino$mpio_origen == "BOLIVIA"] <- "X02"
destino$codigo_mpio_origen[destino$mpio_origen == "BRASIL"] <- "X03"
destino$codigo_mpio_origen[destino$mpio_origen == "CANADÁ"] <- "X04"
destino$codigo_mpio_origen[destino$mpio_origen == "CHILE"] <- "X05"
destino$codigo_mpio_origen[destino$mpio_origen == "CHINA"] <- "X06"
destino$codigo_mpio_origen[destino$mpio_origen == "ECUADOR"] <- "X07"
destino$codigo_mpio_origen[destino$mpio_origen == "ESTADOS UNIDOS DE AMÉRICA"] <- "X08"
destino$codigo_mpio_origen[destino$mpio_origen == "MÉXICO"] <- "X09"
destino$codigo_mpio_origen[destino$mpio_origen == "PAÍSES BAJOS"] <- "X10"
destino$codigo_mpio_origen[destino$mpio_origen == "PERÚ"] <- "X11"
destino$codigo_mpio_origen[destino$mpio_origen == "URUGUAY"] <- "X12"
destino$codigo_mpio_origen[destino$mpio_origen == "VENEZUELA"] <- "X13"
# INPUTAMOS DISTANCIAS QUE NO ESTAN EN LA BASE
distancias$`05001`[distancias$codigo_mpio_origen == "05480"] <- 226
distancias$`05001`[distancias$codigo_mpio_origen == "05756"] <- 115
distancias$`05001`[distancias$codigo_mpio_origen == "52473"] <- 434
distancias$`05001`[distancias$codigo_mpio_origen == "27615"] <- 151
distancias$`05001`[distancias$codigo_mpio_origen == "05475"] <- 160
distancias$`05001`[distancias$codigo_mpio_origen == "05652"] <- 101


# INDICE DE VULNERABILIDAD TOTAL 
# Insumo 1 IHH "IHHTOTAL" + PONDERADO DISTANCIAS
# Distancias ponderadas
distancias_ponderadas_total <- destino %>%
  group_by(codigo_mpio_origen, mpio_origen) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()
total <- sum(distancias_ponderadas_total$suma_producto)
distancias_ponderadas_total$participacion <- distancias_ponderadas_total$suma_producto/total
# Pegamos las distanicias 
distancias_ponderadas_total <- merge(distancias_ponderadas_total , distancias, by = "codigo_mpio_origen")
min_val_dist <- min(distancias_ponderadas_total$`05001`, na.rm = TRUE)
max_val_dist <- max(distancias_ponderadas_total$`05001`, na.rm = TRUE)
distancias_ponderadas_total$distancia_ponderada_norm <- (distancias_ponderadas_total$`05001` - min_val_dist) / (max_val_dist - min_val_dist) 
distancias_ponderadas_total$distancia_ponderada <- distancias_ponderadas_total$participacion * distancias_ponderadas_total$distancia_ponderada_norm
suma_total <- sum(distancias_ponderadas_total$distancia_ponderada)
indice_v_general <- (suma_total + IHH_total)/2
indice_v_general <- as.data.frame(indice_v_general)
# Renombra la columna de indice_v_general a indice_vulnerabilidad
indice_v_general <- rename(indice_v_general, indice_vulnerabilidad = IHH)

# INDICE TOTAL (TODOS LOS AÑOS) POR PRODUCTO
distancias_ponderadas_producto <- destino %>%
  group_by(codigo_mpio_origen, mpio_origen, producto) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_producto_aux <- destino %>%
  group_by(producto) %>%
  summarise(suma_producto_total = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_producto <- merge(distancias_ponderadas_producto, distancias_ponderadas_producto_aux  , by = "producto")
distancias_ponderadas_producto$participacion <- distancias_ponderadas_producto$suma_producto / distancias_ponderadas_producto$suma_producto_total
# pegamos las distancias 
distancias_ponderadas_producto <- merge(distancias_ponderadas_producto , distancias, by = "codigo_mpio_origen")
# Normalizamos las distancias 
min_max_scale <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
distancias_ponderadas_producto <- distancias_ponderadas_producto %>%
  group_by(producto) %>%
  mutate(distancia_norm = min_max_scale(`05001`))
distancias_ponderadas_producto$distancia_norm[is.nan(distancias_ponderadas_producto$distancia_norm)] <- 1
distancias_ponderadas_producto$distancia_ponderada <- distancias_ponderadas_producto$participacion * distancias_ponderadas_producto$distancia_norm
indice_v_general_producto <- distancias_ponderadas_producto %>%
  group_by(producto) %>%
  summarise(distancia_ponderada = sum(distancia_ponderada))%>%
  ungroup()
indice_v_general_producto <- merge(indice_v_general_producto, IHH_total_producto, by = "producto")
indice_v_general_producto$indice_vulnerabilidad <-  (indice_v_general_producto$distancia_ponderada + indice_v_general_producto$IHH)/2


# INDICE ANUAL PARA TODOS LOS PRODUCTOS 
distancias_ponderadas_anual <- destino %>%
  group_by(codigo_mpio_origen, mpio_origen, anio) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_anual_aux <- destino %>%
  group_by(anio) %>%
  summarise(suma_producto_total = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_anual <- merge(distancias_ponderadas_anual, distancias_ponderadas_anual_aux  , by = "anio")
distancias_ponderadas_anual$participacion <- distancias_ponderadas_anual$suma_producto / distancias_ponderadas_anual$suma_producto_total
# pegamos las distancias 
distancias_ponderadas_anual <- merge(distancias_ponderadas_anual , distancias, by = "codigo_mpio_origen")
# Normalizamos las distancias 
min_max_scale <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
distancias_ponderadas_anual <- distancias_ponderadas_anual %>%
  group_by(anio) %>%
  mutate(distancia_norm = min_max_scale(`05001`))
distancias_ponderadas_anual$distancia_ponderada <- distancias_ponderadas_anual$participacion * distancias_ponderadas_anual$distancia_norm
indice_v_anual <- distancias_ponderadas_anual %>%
  group_by(anio) %>%
  summarise(distancia_ponderada = sum(distancia_ponderada))%>%
  ungroup()
indice_v_anual <- merge(indice_v_anual, IHH_anual_total, by = "anio")
indice_v_anual$indice_vulnerabilidad <-  (indice_v_anual$distancia_ponderada + indice_v_anual$IHH)/2


# INDICE ANUAL POR PRODUCTO 
distancias_ponderadas_anual_producto <- destino %>%
  group_by(codigo_mpio_origen, mpio_origen, anio,producto) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_anual_producto_aux <- destino %>%
  group_by(anio,producto) %>%
  summarise(suma_producto_total = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_anual_producto <- merge(distancias_ponderadas_anual_producto, distancias_ponderadas_anual_producto_aux  , by = c("anio","producto"))
distancias_ponderadas_anual_producto$participacion <- distancias_ponderadas_anual_producto$suma_producto / distancias_ponderadas_anual_producto$suma_producto_total
# pegamos las distancias 
distancias_ponderadas_anual_producto <- merge(distancias_ponderadas_anual_producto,distancias, by = "codigo_mpio_origen")
# Normalizamos las distancias
min_max_scale <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
distancias_ponderadas_anual_producto <- distancias_ponderadas_anual_producto %>%
  group_by(anio , producto) %>%
  mutate(distancia_norm = min_max_scale(`05001`)) %>%
  ungroup()
distancias_ponderadas_anual_producto$distancia_norm[is.nan(distancias_ponderadas_anual_producto$distancia_norm)] <- 1
distancias_ponderadas_anual_producto$distancia_ponderada <- distancias_ponderadas_anual_producto$participacion * distancias_ponderadas_anual_producto$distancia_norm

indice_v_anual_producto <- distancias_ponderadas_anual_producto %>%
  group_by(producto , anio) %>%
  summarise(distancia_ponderada = sum(distancia_ponderada))%>%
  ungroup()
indice_v_anual_producto <- merge(indice_v_anual_producto, IHH_anual_producto, by = c("anio","producto"))
indice_v_anual_producto$indice_vulnerabilidad <-  (indice_v_anual_producto$distancia_ponderada + indice_v_anual_producto$IHH)/2


# INDICE MENSUAL TODOS LOS PRODUCTOS
distancias_ponderadas_mensual <- destino %>%
  group_by(codigo_mpio_origen, mpio_origen, anio , mes) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_mensual_aux <- destino %>%
  group_by(anio,mes) %>%
  summarise(suma_producto_total = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_mensual <- merge(distancias_ponderadas_mensual, distancias_ponderadas_mensual_aux , by= c("anio","mes"))
distancias_ponderadas_mensual$participacio <- distancias_ponderadas_mensual$suma_producto / distancias_ponderadas_mensual$suma_producto_total
# pegamos las distancias 
distancias_ponderadas_mensual <- merge(distancias_ponderadas_mensual,distancias, by = "codigo_mpio_origen")
# Normalizamos las distancias
min_max_scale <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
distancias_ponderadas_mensual <- distancias_ponderadas_mensual  %>%
  group_by(anio , mes) %>%
  mutate(distancia_norm = min_max_scale(`05001`)) %>%
  ungroup()
distancias_ponderadas_mensual$distancia_ponderada <- distancias_ponderadas_mensual$participacio * distancias_ponderadas_mensual$distancia_norm
indice_v_mensual <- distancias_ponderadas_mensual %>%
  group_by(mes , anio) %>%
  summarise(distancia_ponderada = sum(distancia_ponderada))%>%
  ungroup()
IHH_mensual_total <- rename(IHH_mensual_total, anio = year)
IHH_mensual_total <- rename(IHH_mensual_total, mes = month)
indice_v_mensual <- merge(indice_v_mensual, IHH_mensual_total, by = c("anio","mes"))
indice_v_mensual$indice_vulnerabilidad <-  (indice_v_mensual$distancia_ponderada + indice_v_mensual$IHH)/2

# INDICE MENSUAL POR PRODUCTO
distancias_ponderadas_mensual_producto <- destino %>%
  group_by(codigo_mpio_origen, mpio_origen, anio , mes, producto) %>%
  summarise(suma_producto = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_mensual_producto_aux <- destino %>%
  group_by(anio,mes,producto) %>%
  summarise(suma_producto_total = sum(suma_kg))%>%
  ungroup()
distancias_ponderadas_mensual_producto <- merge(distancias_ponderadas_mensual_producto, distancias_ponderadas_mensual_producto_aux , by= c("anio","mes","producto"))
distancias_ponderadas_mensual_producto$participacion <- distancias_ponderadas_mensual_producto$suma_producto / distancias_ponderadas_mensual_producto$suma_producto_total
# pegamos las distancias 
distancias_ponderadas_mensual_producto <- merge(distancias_ponderadas_mensual_producto,distancias, by = "codigo_mpio_origen")
# Normalizamos las distancias
min_max_scale <- function(x) {
  return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}
distancias_ponderadas_mensual_producto <- distancias_ponderadas_mensual_producto  %>%
  group_by(anio , mes , producto) %>%
  mutate(distancia_norm = min_max_scale(`05001`)) %>%
  ungroup()
distancias_ponderadas_mensual_producto$distancia_norm[is.nan(distancias_ponderadas_mensual_producto$distancia_norm)] <- 1
distancias_ponderadas_mensual_producto$distancia_ponderada <- distancias_ponderadas_mensual_producto$participacion * distancias_ponderadas_mensual_producto$distancia_norm
indice_v_mensual_producto <- distancias_ponderadas_mensual_producto %>%
  group_by(mes, anio, producto) %>%
  summarise(distancia_ponderada = sum(distancia_ponderada))%>%
  ungroup()
IHH_mensual_producto <- rename(IHH_mensual_producto, anio = year)
IHH_mensual_producto <- rename(IHH_mensual_producto, mes = month)
indice_v_mensual_producto <- merge(indice_v_mensual_producto, IHH_mensual_producto, by = c("anio","mes","producto"))
indice_v_mensual_producto$indice_vulnerabilidad <-  (indice_v_mensual_producto$distancia_ponderada + indice_v_mensual_producto$IHH)/2


# Elimminamos columnas extra 
indice_v_anual <- indice_v_anual[ , c("anio", "indice_vulnerabilidad")]
indice_v_anual_producto <- indice_v_anual_producto[ , c("anio","producto","indice_vulnerabilidad")]
indice_v_general_producto <- indice_v_general_producto[, c("producto","indice_vulnerabilidad")]
indice_v_mensual <- indice_v_mensual[ ,c("anio","mes","mes_y_ano","indice_vulnerabilidad")]
indice_v_mensual_producto <- indice_v_mensual_producto[ , c("anio","mes","producto","mes_y_ano","indice_vulnerabilidad")]


# Exportamos las bases a csv
# Indice vulnerabilidad anual para todos los productos 
saveRDS(indice_v_anual,  "02_Indices/Output/base_indice_anual_todos.rds")
saveRDS(indice_v_anual_producto,  "02_Indices/Output/base_indice_anual_productos.rds")
saveRDS(indice_v_general,"02_Indices/Output/base_indice.rds")
saveRDS(indice_v_general_producto, "02_Indices/Output/base_indice_productos.rds")
saveRDS(indice_v_mensual, "02_Indices/Output/base_indice_mensual.rds")
saveRDS(indice_v_mensual_producto, "02_Indices/Output/base_indice_mensual_productos.rds")
