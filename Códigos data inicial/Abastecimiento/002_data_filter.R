#Proyecto FAO
# Filtros de la base 
################################################################################
#Autores: Juliana Lalinde, Laura Quintero, German Angulo
#Fecha de creacion: 13/02/2024
#Fecha de ultima modificacion: 13/02/2024
rm(list = ls())
################################################################################
# Paquetes 
################################################################################
library(readr)
library(lubridate)
library(dplyr)
library(ggplot2)
library(zoo)
library(mvoutlier)
options(scipen = 999)
################################################################################

# Abrimos los datos
data <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_microdatos_antioquia.csv")
# Eliminamos procesados
data <- subset(data, data$grupo_alimento != "PROCESADOS" )
# Eliminamos los productos no priorizados 
# Grupos priorizados
# Arroz, papa, huevo, platano, tomate, zanahoria, carne de cerdo, frijol, lechiga, yuca, cebolla, repollo y cilantro, limon 
data <- data %>%
  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de| pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))
# Ouliers utilizando Mahalanobis
# Inicializa la lista
todo <- list()
contador <- 0

# Itera sobre cada combinación de destino, origen y alimento
for (destino in unique(data$mpio_destino)) {
  for (origen in unique(data$mpio_origen)) {
    for (producto in unique(data$alimento)) {
      
      # Crea un subconjunto de data_resumen
      df_temp <- data %>% 
        filter(mpio_destino == destino, mpio_origen == origen, producto == alimento)
      
      # Si df_temp está vacío, salta al siguiente ciclo del bucle
      if (nrow(df_temp) == 0) {
        next
      }
      
      # Calcula la distancia de Mahalanobis si el subconjunto tiene más de una fila y la varianza de cantidad_kg no es 0
      if (nrow(df_temp) > 1 && var(df_temp$cantidad_kg) != 0) {
        df_temp$mahalanobis <- mahalanobis(matrix(df_temp$cantidad_kg, ncol = 1), 
                                           center = mean(df_temp$cantidad_kg), 
                                           cov = matrix(var(df_temp$cantidad_kg), ncol = 1))
        
        # Calcula el percentil 95 de la distancia de Mahalanobis
        mahalanobis_95 <- quantile(df_temp$mahalanobis, 0.95)
        
        # Agrega una columna que indica si la distancia de Mahalanobis está en el 95% de la distribución
        df_temp$in_95_percent <- ifelse(df_temp$mahalanobis <= mahalanobis_95, TRUE, FALSE)
      }
      
      # Almacena el subconjunto en la lista
      todo[[paste(destino, origen, producto, sep = "_")]] <- df_temp
      
    }
  }
}

# Unimos las bases resultado 
data_final <- bind_rows(todo)

# Exportamos la base con los resultados de mahalanobits
write.csv(data_final, "01_Datos/Data/Abastecimiento_microdatos/Output/base_mahalanobits.csv")


# Agrupamos la base original (sin quitar ouliers) por mes - destino - origen
# Vamos a sacar cantidad promedio mensual
fecha <- as.Date(data_final$fecha)
data_final$mes_y_ano <- format(fecha, "%Y-%m")


# Agrupamos la base a nivel mensual (promediamos de los kg - meses)
data_ouliers <- data_final %>%
  group_by(codigo_mpio_destino, codigo_mpio_origen, mpio_destino, depto_origen,mpio_origen,grupo_alimento,alimento,mes_y_ano) %>%
  summarise(suma_kg = sum(cantidad_kg, na.rm = TRUE))
# Exportamos la base de datos mensual. Teniendo outliers
write.csv(data_ouliers, "01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_outliers.csv")


# Volvemos a la base de datos "data_final"
# Tenemos 1503946 obs antes de quitar los outliers; quedamos con 1434455

data_final_no_outliers <- data_final %>% filter(in_95_percent != "FALSE")

# Agrupamos la base a nivel mensual (promediamos de los kg - meses)
data_no_outliers <- data_final_no_outliers %>%
  group_by(codigo_mpio_destino, codigo_mpio_origen, mpio_destino, depto_origen,mpio_origen,grupo_alimento,alimento,mes_y_ano) %>%
  summarise(suma_kg = sum(cantidad_kg, na.rm = TRUE))

saveRDS(data_no_outliers, "01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.rds")







