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
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%
  select(-"...1")%>%
  rename(producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(suma_kg=suma_kg/1000000)

destino$mpio_destino[destino$mpio_destino=="Cartagena de Indias"]<-"Cartagena"
destino$depto_origen[destino$depto_origen =="BOGOTÁ, D. C."] <- "BOGOTÁ, D.C."

destino$anio <- year(destino$mes_y_ano)
destino$mes <- month(destino$mes_y_ano)

destino$depto_origen <- ifelse(destino$depto_origen == "ANTIOQUIA", "ANTIOQUIA", "OTROS")
destino$mpio_destino <- ifelse(destino$mpio_destino == "Medellín", "ANTIOQUIA", "OTROS")

#Informacion total
#Anual
#Todo lo que sale de Antioquia y otros dptos
destino1_anual <- destino %>%
  group_by(anio,depto_origen) %>%
  summarise(sale_kg = sum(suma_kg))%>%
  ungroup()%>%rename(depto=depto_origen)

# Todo lo que ingresa a Antioquia y otros dptos
destino2_anual <- destino %>% 
  group_by(anio,mpio_destino) %>%
  summarise(ingresa_kg = sum(suma_kg)) %>%
  ungroup()%>%rename(depto=mpio_destino)

# Unimos la información anual

data_anual <- destino1_anual %>% left_join(destino2_anual, by = c("anio", "depto"))%>%
  filter(depto=="ANTIOQUIA")
data_anual$total_importado <- data_anual$sale_kg - data_anual$ingresa_kg

########
# Informacion Anual por producto 
# Sale de Antioquia por producto
destino_producto1_anual <- destino %>%
  group_by(anio,depto_origen,producto) %>%
  summarise(sale_kg = sum(suma_kg))%>%
  ungroup()%>%rename(depto=depto_origen)

# ingresa a Antioquia por producto
destino_producto2_anual <- destino %>% 
  group_by(anio,mpio_destino,producto) %>%
  summarise(ingresa_kg = sum(suma_kg)) %>%
  ungroup()%>%rename(depto=mpio_destino)

# Aseguramos que esten todas las posebles combinaciones
info_com_anual<-rbind(destino_producto1_anual[1:3],destino_producto2_anual[1:3])%>%unique()

#Unimos los datos
data_anual_producto<-info_com_anual %>% left_join(destino_producto1_anual, by = c("anio", "depto","producto"))%>%
  left_join(destino_producto2_anual, by = c("anio", "depto","producto"))%>%
  filter(depto=="ANTIOQUIA")

# NA es porque no hay entrada o saleda del producto para esa fecha en Ant
data_anual_producto$ingresa_kg[is.na(data_anual_producto$ingresa_kg)]<-0
data_anual_producto$sale_kg[is.na(data_anual_producto$sale_kg)]<-0

data_anual_producto$total_importado <- data_anual_producto$sale_kg - data_anual_producto$ingresa_kg

######
# Informacion total
# Mensual 
# Todo lo sale de Antio y otros dptos
destino1 <- destino %>%
  group_by(mes,anio,depto_origen) %>%
  summarise(sale_kg = sum(suma_kg))%>%
  ungroup()%>%rename(depto=depto_origen)

# Todo lo que ingresa a Antioquia y otros dptos
destino2 <- destino %>% 
  group_by(mes,anio,mpio_destino) %>%
  summarise(ingresa_kg = sum(suma_kg)) %>%
  ungroup()%>%rename(depto=mpio_destino)


# Unimos la información mensual

data_mensual <- destino1 %>% left_join(destino2, by = c("mes", "anio", "depto"))%>%
  filter(depto=="ANTIOQUIA")

data_mensual$total_importado <- data_mensual$sale_kg - data_mensual$ingresa_kg
data_mensual$fecha <- make_date(data_mensual$anio, data_mensual$mes)



# Data Mensual por producto 
# Sale de Ant por producto
destino_producto1 <- destino %>%
  group_by(mes,anio,depto_origen,producto) %>%
  summarise(sale_kg = sum(suma_kg))%>%
  ungroup()%>%rename(depto=depto_origen)

# ingresa a Ant por producto
destino_producto2 <- destino %>% 
  group_by(mes,anio,mpio_destino,producto) %>%
  summarise(ingresa_kg = sum(suma_kg)) %>%
  ungroup()%>%rename(depto=mpio_destino)

# Aseguramos que esten todas las posebles combinaciones
info_com<-rbind(destino_producto1[1:4],destino_producto2[1:4])%>%unique()

#Unimos los datos
data_mensual_producto<-info_com %>% left_join(destino_producto1, by = c("mes", "anio", "depto","producto"))%>%
  left_join(destino_producto2, by = c("mes", "anio", "depto","producto"))%>%
  filter(depto=="ANTIOQUIA")

# NA es porque no hay entrada o saleda del producto para esa fecha en Ant
data_mensual_producto$ingresa_kg[is.na(data_mensual_producto$ingresa_kg)]<-0
data_mensual_producto$sale_kg[is.na(data_mensual_producto$sale_kg)]<-0

data_mensual_producto$total_importado <- data_mensual_producto$sale_kg - data_mensual_producto$ingresa_kg
data_mensual_producto$fecha <- make_date(data_mensual_producto$anio, data_mensual_producto$mes)



saveRDS(data_mensual, "02_Indices/Output/neto_mensual.rds")
saveRDS(data_anual,  "02_Indices/Output/neto_anual.rds")
saveRDS(data_mensual_producto,  "02_Indices/Output/neto_mensual_producto.rds")
saveRDS(data_anual_producto,  "02_Indices/Output/neto_anual_producto.rds")


