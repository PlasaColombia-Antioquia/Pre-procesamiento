#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 21/02/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Limpiar el entorno de trabajo
rm(list=ls())
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse)
options(scipen = 999)
################################################################################


#Unir todas las bases precios----
# Define la ruta de la carpeta
ruta_carpeta <- "01_Datos/Data/Precios_Mayorista/Output/"

# Obtiene la lista de archivos en la carpeta que comienzan con "base_precios_mayorista_"
archivos <- list.files(path = ruta_carpeta, pattern = "^base_precios_mayorista_diario", full.names = TRUE)

# Lee cada archivo y los une uno debajo del otro
df <- do.call(rbind, lapply(archivos, read_csv))%>%select(producto,ciudad,precio,fecha,nom_archivo)%>%
  filter(precio>0)

df$ciudad[df$ciudad=="armenia"]<-"Armenia"
df$ciudad[df$ciudad=="bogota"]<-"Bogotá"
df$ciudad[df$ciudad=="Bogotá, D.C."]<-"Bogotá"
df$ciudad[df$ciudad=="Bogotá..Corabastos"]<-"Bogotá"
df$ciudad[df$ciudad=="bucaramanga"]<-"Bucaramanga"
df$ciudad[df$ciudad=="Bucaramanga\r\nCentroabastos"]<-"Bucaramanga"
df$ciudad[df$ciudad=="Bucaramanga\r\r\nCentroabastos"]<-"Bucaramanga"
df$ciudad[df$ciudad=="cali"]<-"Cali"
df$ciudad[df$ciudad=="Cartagena..Bazurto"]<-"Cartagena"
df$ciudad[df$ciudad=="Cartagena"]<-"Cartagena de Indias"
df$ciudad[df$ciudad=="cucuta"]<-"Cúcuta"
df$ciudad[df$ciudad=="Cúcuta\r\nCenabastos"]<-"Cúcuta"
df$ciudad[df$ciudad=="Cúcuta\r\r\nCenabastos"]<-"Cúcuta"
df$ciudad[df$ciudad=="Ibagué\r\r\nPlaza"]<-"Ibagué"
df$ciudad[df$ciudad=="Ibagué\r\nPlaza"]<-"Ibagué"
df$ciudad[df$ciudad=="manizales"]<-"Manizales"
df$ciudad[df$ciudad=="medellin"]<-"Medellín"
df$ciudad[df$ciudad=="Medellín\r\nCMA"]<-"Medellín"
df$ciudad[df$ciudad=="Medellín\r\r\nCMA"]<-"Medellín"
df$ciudad[df$ciudad=="Medellín..CMA"]<-"Medellín"
df$ciudad[df$ciudad=="neiva"]<-"Neiva"
df$ciudad[df$ciudad=="pasto"]<-"Pasto"
df$ciudad[df$ciudad=="pereira"]<-"Pereira"
df$ciudad[df$ciudad=="Pereira...Mercasa"]<-"Pereira"
df$ciudad[df$ciudad=="santa"]<-"Santa Marta"
df$ciudad[df$ciudad=="Santa"]<-"Santa Marta"
df$ciudad[df$ciudad=="Santa.Marta"]<-"Santa Marta"
df$ciudad[df$ciudad=="tunja"]<-"Tunja"
df$ciudad[df$ciudad=="Valledupar\r\nMercabastos"]<-"Valledupar"
df$ciudad[df$ciudad=="Villavicencio..CAV"]<-"Villavicencio"

df$producto[df$producto=="Aguacate papelillo"]<-"Aguacate Papelillo"
df$producto[df$producto=="Arveja verde en vaina"]<-"Arveja Verde En Vaina"
df$producto[df$producto=="Banano criollo"]<-"Banano Criollo"
df$producto[df$producto=="Cebolla cabezona blanca"]<-"Cebolla Cabezona Blanca"
df$producto[df$producto=="Cebolla junca"]<-"Cebolla Junca"
df$producto[df$producto=="Chócolo mazorca"]<-"Chócolo Mazorca"
df$producto[df$producto=="Chocolo mazorca"]<-"Chócolo Mazorca"
df$producto[df$producto=="Fríjol verde en vaina"]<-"Fríjol Verde En Vaina"
df$producto[df$producto=="Guayaba pera"]<-"Guayaba Pera"
df$producto[df$producto=="Lechuga batavia"]<-"Lechuga Batavia"
df$producto[df$producto=="Limón común"]<-"Limón Común"
df$producto[df$producto=="Mango tommy"]<-"Mango Tommy"
df$producto[df$producto=="Mora de Castilla"]<-"Mora De Castilla"
df$producto[df$producto=="Papa  negra"]<-"Papa negra"
df$producto[df$producto=="Papa  negra+"]<-"Papa negra"
df$producto[df$producto=="Papa  criolla"]<-"Papa criolla"
df$producto[df$producto=="Papa negr"]<-"Papa negra"
df$producto[df$producto=="Papa nwgra"]<-"Papa negra"
df$producto[df$producto=="Papaya maradol"]<-"Papaya Maradol"
df$producto[df$producto=="Pepino cohombro"]<-"Pepino Cohombro"
df$producto[df$producto=="Plátano guineo"]<-"Plátano Guineo"
df$producto[df$producto=="Plátano hartón verde"]<-"Plátano Hartón Verde"
df$producto[df$producto=="Tomate de árbol"]<-"Tomate De Árbol"

# Define la lista de palabras
#palabras <- c("Arroz", "Papa ", "Huevo", "Plátano", "Tomate", "Zanahoria", "Carne De Cerdo", "Fríjol", "Lechuga", "Yuca", "Cebolla", "Repollo", "Cilantro", "Limón")

# Filtra las filas en las que la columna producto contiene al menos una de las palabras de la lista
df_filtrado <- df #%>%
  #filter(sapply(palabras, function(x) str_detect(producto, x)) %>% apply(1, any))


#Eliminar atípicos----

# Ouliers utilizando Mahalanobis

# Inicializa la lista
todo <- list()
contador <- 0

# Itera sobre cada combinación de destino, origen y alimento
for (alimento in unique(df_filtrado$producto)) {
    for (destino in unique(df_filtrado$ciudad)) {
      
      
      #print(paste("Procesando grupo:", destino, origen, alimento))
      # Incrementa el contador
      contador <- contador + 1
      print(contador)
      
      # Crea un subconjunto de df_filtrado_resumen
      df_temp <- df_filtrado %>% 
        filter(ciudad == destino, producto == alimento)
      
      # Calcula la distancia de Mahalanobis si el subconjunto tiene más de una fila y la varianza de precio no es 0
      if (nrow(df_temp) > 1 && var(df_temp$precio) != 0) {
        df_temp$mahalanobis <- mahalanobis(matrix(df_temp$precio, ncol = 1), 
                                           center = mean(df_temp$precio), 
                                           cov = matrix(var(df_temp$precio), ncol = 1))
        
        # Calcula el percentil 95 de la distancia de Mahalanobis
        mahalanobis_95 <- quantile(df_temp$mahalanobis, 0.95)
        
        # Agrega una columna que indica si la distancia de Mahalanobis está en el 95% de la distribución
        df_temp$in_95_percent <- ifelse(df_temp$mahalanobis <= mahalanobis_95, TRUE, FALSE)
      }
      
      # Almacena el subconjunto en la lista
      todo[[paste(destino, alimento, sep = "_")]] <- df_temp
      
    }
  }


df_mah <- bind_rows(todo)%>%filter(in_95_percent==TRUE)

# Vamos a sacar cantidad promedio mensual
fecha <- as.Date(df_mah$fecha)
df_mah$mes_y_ano <- format(df_mah$fecha, "%Y-%m")
df_mah$anio <- format(df_mah$fecha, "%Y")

# Agrupamos la base a nivel mensual (promediamos de los kg - meses)
data_1 <- df_mah %>%
  group_by(producto,ciudad,mes_y_ano) %>%
  summarise(precio_prom = mean(precio, na.rm = TRUE))%>%
  mutate(producto=paste0(toupper(substr(producto, 1, 1)), tolower(substr(producto, 2, nchar(producto)))))

writexl::write_xlsx(data_1, "01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_mes_filtrados.xlsx")
