#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################-
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 21/02/2024
#Fecha de ultima modificacion: 25/02/2024
################################################################################-
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################-
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse);library(gridExtra);library(corrplot)
options(scipen = 999)
################################################################################-

data<-read_excel("01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_mes_filtrados.xlsx")#%>%
  #select(-"...1")

data_1<-data%>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(anio = year(mes_y_ano))

saveRDS(data_1, "02_Indices/Output/base_precios_cambio.rds")

# Supongamos que "fecha", "ciudad", "producto" y "precio" son las columnas relevantes en tu dataframe df
# BASE DE DATOS DE PRECIOS  
# VARIACION MES A MES 

data2 <- data %>%
  arrange(ciudad, producto, mes_y_ano) %>%
  group_by(ciudad, producto) %>%
  mutate(cambio_pct = (precio_prom / lag(precio_prom) - 1) * 100)


# VARIACION MES - AÑO - PRODUCTO

df <- data2 %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month")) %>%
  drop_na(mes_y_ano) %>%
  ungroup() %>%
  complete(ciudad, producto, mes_y_ano = seq.Date(min(mes_y_ano, na.rm = TRUE), max(mes_y_ano, na.rm = TRUE), by = "month")) %>%
  group_by(ciudad, producto)

df <- df %>%
  arrange(ciudad, producto, mes_y_ano) %>%
  group_by(ciudad, producto) %>%
  mutate(cambio_pct_anual = (precio_prom / lag(precio_prom, 12) - 1) * 100)%>%ungroup()%>%
  mutate(producto=str_to_lower(producto), anio = year(mes_y_ano))#%>%drop_na(cambio_pct_anual)

#Destino----
destino <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.csv")%>%
  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%select(-"...1")%>%
  rename(ciudad=mpio_destino, producto=alimento) %>%
  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  mutate(producto=str_to_lower(producto))

destino$ciudad[destino$ciudad=="Cartagena de Indias"]<-"Cartagena"

#Union de precios y cantidades

df2<-destino%>%
  left_join(df, by = c("ciudad", "mes_y_ano", "producto"))%>%
  drop_na(codigo_mpio_destino)

#Distancias----

distancias <- readRDS("01_Datos/Data/Bases_auxiliares/Input/distancias.rds") %>%
  as.data.frame() %>%
  rownames_to_column(var = "codigo_mpio_origen") %>%
  gather(key = "codigo_mpio_destino", value = "distancia", -codigo_mpio_origen)%>%
  mutate(codigo_mpio_destino=as.numeric(codigo_mpio_destino))

#Unir precios, cantidades y distancias <3

complet<-df2%>%left_join(distancias, by = c("codigo_mpio_destino","codigo_mpio_origen"))%>%
  drop_na(precio_prom)%>%
  select(ciudad,producto,precio_prom,mes_y_ano, anio, distancia,suma_kg)

saveRDS(complet, "02_Indices/Output/base_precios_cantidades_distancias.rds")

#Promedio mes----

mes_cant<-destino%>%mutate(mes = month(mes_y_ano, label = TRUE))%>%
  filter(ciudad=="Medellín")%>%group_by(ciudad, producto, mes)%>%
  summarise(cantidad=mean(suma_kg))

mes_precio<-df%>%mutate(mes = month(mes_y_ano, label = TRUE))%>%
  filter(ciudad=="Medellín")%>%drop_na(precio_prom)%>%
  group_by(ciudad, producto, mes)%>%
  summarise(precio_prom=mean(precio_prom, na.rm = TRUE))

graficar_producto <- function(df, alimento) {
  # Filtra los datos para el producto específico, a menos que alimento sea "total"
  if (alimento != "total") {
    df <- df %>% filter(producto == alimento)
  }
  
  datos_producto <- df %>%filter(ciudad=="Medellín")%>%
    mutate(mes = month(mes_y_ano)) %>%
    group_by(mes) %>%
    summarise(cantidad = mean(suma_kg, na.rm = TRUE))
  
  # Crea el gráfico
  ggplot(datos_producto, aes(x = mes, y = cantidad)) +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 12, 1)) + 
    labs(title = paste("Cantidad mensual de", alimento),
         x = "Mes",
         y = "Cantidad") +
    theme_minimal()
}

graficar_precio <- function(df, alimento) {
  # Filtra los datos para el producto específico, a menos que alimento sea "total"
  if (alimento != "total") {
    df <- df %>% filter(producto == alimento)
  }
  
  datos_producto <- df %>%filter(ciudad=="Medellín")%>%
    mutate(mes = month(mes_y_ano)) %>%
    group_by(mes) %>%
    summarise(precio_prom = mean(precio_prom, na.rm = TRUE))
  
  # Crea el gráfico
  ggplot(datos_producto, aes(x = mes, y = precio_prom)) +
    geom_line() +
    scale_x_continuous(breaks = seq(1, 12, 1)) + 
    labs(title = paste("Precio mensual de", alimento),
         x = "Mes",
         y = "Precio/Kg") +
    theme_minimal()
}


graficar_producto(destino, "total")
graficar_producto(destino, "zanahoria")

graficar_precio(df, "papa criolla")

#Entradas a medellín en promedio por producto

entrd_prom <- complet%>%
  filter(ciudad=="Medellín")%>%
  group_by(producto)%>%
  summarise(cantid_prom=mean(suma_kg, na.rm = TRUE),
            precio_prom=mean(precio_prom, na.rm = TRUE),
            distan_prom=mean(distancia, na.rm = TRUE))

#% Que entra de cada municipio

enttrd_mun <- destino %>%
  filter(ciudad == "Medellín") %>%
  group_by(codigo_mpio_destino, mpio_origen, depto_origen, codigo_mpio_origen, mes_y_ano) %>%
  summarise(total_kilos = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(mes_y_ano) %>%
  mutate(porcent = total_kilos / sum(total_kilos),
         mes_kilos=sum(total_kilos))%>%ungroup()%>%
  mutate(id_time=1,time=mes_y_ano,variable="% por municipio de lo que llega a Medellín",value=porcent,
         producto= NA)%>%
  select(codigo_mpio_origen,codigo_mpio_destino,producto,id_time,time,variable,value)

# Sale de Antioquia a otras mayoristas
salida_mun <- destino %>%
  filter(ciudad != "Medellín") %>%
  group_by(ciudad, mpio_origen, depto_origen, codigo_mpio_origen, mes_y_ano) %>%
  summarise(total_kilos = sum(suma_kg, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(mes_y_ano) %>%
  mutate(porcent = total_kilos / sum(total_kilos),
         mes_kilos=sum(total_kilos))



