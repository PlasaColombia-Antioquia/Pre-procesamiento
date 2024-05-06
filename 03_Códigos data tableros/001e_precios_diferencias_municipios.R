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
  library(readxl);library(reshape2);library(ggplot2);library(gganimate);library(dplyr);
  library(readr);library(lubridate);library(zoo);library(stringr);library(tidyr);library(ggrepel)
  ################################################################################-
  
  base_codigo_municipios <- read_csv("01_Datos/Data/Bases_auxiliares/Output/base_codigo_municipios.csv")%>%
    janitor::clean_names()%>%filter(codigo_mpio!="05059")%>%
    select(municipio, departamento, cod_depto)
  
  base_codigo_municipios$municipio[base_codigo_municipios$municipio == "Cartagena de Indias"] <- "Cartagena"
  
  data <- read_excel("01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_mes_filtrados.xlsx")
  
  #Añadir información del año y del mes
  data$year <- as.numeric(substr(data$mes_y_ano,1,4))
  data$mes <- as.numeric(substr(data$mes_y_ano,6,length(data$mes_y_ano)))
  
  data$ciudad[data$ciudad == "Cartagena de Indias"] <- "Cartagena"
  
  #Cambiar la base de datos de long a wide
  data <- dcast(data,producto + mes_y_ano + year + mes ~ ciudad,value.var = "precio_prom")
  
  #Mantener unicamente producto, mes y año en el que tengamos información de referencia de Medellín
  data <- data[!is.na(data$Medellín),]
  
  data$precio_medellin_ano_mes_producto <- data$Medellín
  
  #Devolver el formato a long manteniendo a Medellín como columna de referencia
  data <- melt(data,id.vars=c("producto","mes_y_ano","year","mes","precio_medellin_ano_mes_producto"),variable.name = "ciudad",value.name = "precio_prom")
  data$ciudad <- as.character(data$ciudad)
  
  data <- na.omit(data)
  
  colnames(data)[7] <- "precio_prom_ano_mes_producto"
  
  #PRECIOS PROMEDIOS POR MUNICIPIO
  #Precio promedio anual y mensual
  data <- data %>%
    group_by(year,mes,ciudad) %>%
    mutate(precio_prom_ano_mes = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(year,mes,ciudad) %>%
    mutate(sd_precio_prom_ano_mes = sd(precio_prom_ano_mes_producto))
  
  #Precio promedio mensual por producto
  data <- data %>%
    group_by(mes,producto,ciudad) %>%
    mutate(precio_prom_mes_producto = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(mes,producto,ciudad) %>%
    mutate(sd_precio_prom_mes_producto = sd(precio_prom_ano_mes_producto))
  
  #Precio promedio mensual
  data <- data %>%
    group_by(mes,ciudad) %>%
    mutate(precio_prom_mes = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(mes,ciudad) %>%
    mutate(sd_precio_prom_mes = sd(precio_prom_ano_mes_producto))
  
  #Precio promedio anual por producto
  data <- data %>%
    group_by(year,producto,ciudad) %>%
    mutate(precio_prom_ano_producto = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(year,producto,ciudad) %>%
    mutate(sd_precio_prom_ano_producto = sd(precio_prom_ano_mes_producto))
  
  #Precio promedio anual
  data <- data %>%
    group_by(year,ciudad) %>%
    mutate(precio_prom_ano = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(year,ciudad) %>%
    mutate(sd_precio_prom_ano = sd(precio_prom_ano_mes_producto))
  
  #Precio promedio
  data <- data %>%
    group_by(ciudad) %>%
    mutate(precio_prom = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(ciudad) %>%
    mutate(sd_precio_prom = sd(precio_prom_ano_mes_producto))
  
  #Precio promedio
  data <- data %>%
    group_by(producto,ciudad) %>%
    mutate(precio_prom_producto = mean(precio_prom_ano_mes_producto))
  
  data <- data %>%
    group_by(producto,ciudad) %>%
    mutate(sd_precio_prom_producto = sd(precio_prom_ano_mes_producto))
  
  #PRECIOS PROMEDIOS DE MEDELLIN
  data_medellin <- data[,c("producto","year","mes","precio_medellin_ano_mes_producto")]
  data_medellin <- data_medellin[!(duplicated(data_medellin[c("producto","year","mes")])),]
  
  #Precio promedio anual y mensual
  data_medellin <- data_medellin  %>%
    group_by(year,mes) %>%
    mutate(precio_medellin_ano_mes = mean(precio_medellin_ano_mes_producto))
  
  #Precio promedio mensual por producto
  data_medellin <- data_medellin  %>%
    group_by(mes,producto) %>%
    mutate(precio_medellin_mes_producto = mean(precio_medellin_ano_mes_producto))
  
  #Precio promedio mensual
  data_medellin <- data_medellin  %>%
    group_by(mes) %>%
    mutate(precio_medellin_mes = mean(precio_medellin_ano_mes_producto))
  
  #Precio promedio anual por producto
  data_medellin <- data_medellin  %>%
    group_by(year,producto) %>%
    mutate(precio_medellin_ano_producto = mean(precio_medellin_ano_mes_producto))
  
  #Precio promedio anual
  data_medellin  <- data_medellin %>%
    group_by(year) %>%
    mutate(precio_medellin_ano = mean(precio_medellin_ano_mes_producto))
  
  #Precio promedio
  data_medellin$precio_medellin <- mean(data_medellin$precio_medellin_ano_mes_producto)
  
  #Precio promedio
  data_medellin <- data_medellin %>%
    group_by(producto) %>%
    mutate(precio_medellin_producto = mean(precio_medellin_ano_mes_producto))
  
  data <- merge(data,data_medellin,by=c("producto","year","mes","precio_medellin_ano_mes_producto"))
  
  #COMPARACIÓN DE PRECIOS DE MUNICIPIOS CON MEDELLÍN
  #Precio promedio anual, mensual y producto
  #data$comparacion_anual_mensual_producto <- (data$precio_prom_ano_mes_producto/data$precio_medellin_ano_mes_producto)-1
  data$comparacion_anual_mensual_producto <- data$precio_prom_ano_mes_producto-data$precio_medellin_ano_mes_producto
  
  #Precio promedio anual y mensual
  #data$comparacion_anual_mensual <- (data$precio_prom_ano_mes/data$precio_medellin_ano_mes)-1
  data$comparacion_anual_mensual <- data$precio_prom_ano_mes-data$precio_medellin_ano_mes
  
  #Precio promedio mensual por producto
  #data$comparacion_mensual_producto <- (data$precio_prom_mes_producto/data$precio_medellin_mes_producto)-1
  data$comparacion_mensual_producto <- data$precio_prom_mes_producto-data$precio_medellin_mes_producto
  
  #Precio promedio mensual
  #data$comparacion_mensual <- (data$precio_prom_mes/data$precio_medellin_mes)-1
  data$comparacion_mensual <- data$precio_prom_mes-data$precio_medellin_mes
  
  #Precio promedio anual por producto
  #data$comparacion_anual_producto <- (data$precio_prom_ano_producto/data$precio_medellin_ano_producto)-1
  data$comparacion_anual_producto <- data$precio_prom_ano_producto-data$precio_medellin_ano_producto
  
  #Precio promedio anual
  #data$comparacion_anual <- (data$precio_prom_ano/data$precio_medellin_ano)-1
  data$comparacion_anual <- data$precio_prom_ano-data$precio_medellin_ano
  
  #Precio promedio
  #data$comparacion <- (data$precio_prom/data$precio_medellin)-1
  data$comparacion <- data$precio_prom-data$precio_medellin
  
  #Precio promedio
  #data$comparacion_producto <- (data$precio_prom_producto/data$precio_medellin_producto)-1
  data$comparacion_producto <- data$precio_prom_producto-data$precio_medellin_producto
  
  #AGREGAR CANTIDADES
  #Destino----
  #destino <- read_csv("01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_mensual_no_outliers.csv")%>%
  #  filter(alimento %in% c("Arroz","Papa Betina","Papa capira","Papa criolla","Papa Morasurco","Papa nevada","Papa parda pastusa","Papa R-12","Papa rubí","Papa sabanera","Papa superior","Papa suprema","Papa única","Papas negras otras","Huevo","Huevo otros",
  #                         "Plátano guineo","Plátano hartón verde","Plátanos otros","Tomate chonto","Tomate larga vida","Tomate Riogrande",
  #                         "Tomates otros","Zanahoria","Carne de cerdo","Carne de pollo","Carne de res","Fríjol","Fríjol verde","Lechuga Batavia",
  #                         "Lechuga crespa","Lechugas otras","Yuca","Cebolla Cabezona","Cebolla cabezona blanca","Cebolla cabezona roja","Cebolla junca","Repollo",
  #                         "Cilantro","Limón común","Limón mandarino","Limón Tahití","Limones otros"))%>%select(-"...1")%>%
  #  rename(ciudad=mpio_destino, producto=alimento) %>%
  #  mutate(mes_y_ano = floor_date(as.Date(as.yearmon(mes_y_ano, "%Y-%m"), frac = 1), "month"))%>%
  #  mutate(producto=str_to_lower(producto))
  
  #destino <- destino[,c("ciudad","producto","mes_y_ano","suma_kg")]
  
  #destino <- destino %>%
  #            group_by(ciudad,producto,mes_y_ano) %>%
  #            mutate(kg_mensual = sum(suma_kg))
  
  #destino <- destino[,-c(4)]
  
  #destino <- destino[!(duplicated(destino[c("ciudad","producto","mes_y_ano")])),]
  
  #Union de precios y cantidades
  
  #data$mes_y_ano = floor_date(as.Date(as.yearmon(data$mes_y_ano, "%Y-%m"), frac = 1), "month")
  
  #data$producto <- str_to_lower(data$producto)
  
  data$axis <- 1
  
  data <- data %>%
    left_join(base_codigo_municipios, by = c("ciudad"="municipio"))
  
  #data2 <- merge(x = data, y = destino,by = c("ciudad", "mes_y_ano", "producto"))
  
# Esta base se utiliza para el mapa 
saveRDS(data, "02_Indices/Output/base_precios_vs_medellin.rds")
  
  # Partimos las bases de datos

data <- data[, c("producto", "year", "mes", "ciudad", "comparacion_anual_mensual_producto", "comparacion_anual_mensual", "comparacion_mensual_producto", "comparacion_mensual", "comparacion_anual_producto", "comparacion_anual", "comparacion", "comparacion_producto","cod_depto", "axis", "departamento", grep("^sd_", names(data), value = TRUE))]

  
# Base para la informacion anual - mensual - producto
data_comparacion_anual_mensual_producto <- data[,c("producto","year","mes","ciudad","departamento","comparacion_anual_mensual_producto","cod_depto")]
data_comparacion_anual_mensual_producto$sd_ <- 1
data_comparacion_anual_mensual_producto <- split(data_comparacion_anual_mensual_producto, data_comparacion_anual_mensual_producto$producto)
# Base para la informacion anual - mensual
data_comparacion_anual_mensual <- data[,c("producto","year","mes","ciudad","departamento","comparacion_anual_mensual","sd_precio_prom_ano_mes","cod_depto")]
data_comparacion_anual_mensual <- data_comparacion_anual_mensual %>%
  distinct(departamento, ciudad, mes, year,  .keep_all = TRUE)
data_comparacion_anual_mensual$producto <- NULL
# Base para la informacion mensual - produto 
data_comparacion_mensual_producto <- data[,c("producto","year","mes","ciudad","departamento","comparacion_mensual_producto","sd_precio_prom_mes_producto","cod_depto")]
data_comparacion_mensual_producto <- data_comparacion_mensual_producto %>%
  distinct(departamento,ciudad,mes,producto, .keep_all = TRUE)
mean_value <- mean(data_comparacion_mensual_producto$sd_precio_prom_mes_producto, na.rm = TRUE)
data_comparacion_mensual_producto$sd_precio_prom_mes_producto[is.na(data_comparacion_mensual_producto$sd_precio_prom_mes_producto)] <- mean_value
# Base para la informacion mensual
data_comparacion_mensual <- data[,c("producto","year","mes","ciudad","departamento","comparacion_mensual","sd_precio_prom_mes","cod_depto")]
data_comparacion_mensual <- data_comparacion_mensual %>% 
  distinct(departamento,ciudad,mes, .keep_all = TRUE)
data_comparacion_mensual$producto <- NULL
# Base para la informacion anual  - ptoducto
data_comparacion_anual_producto <- data[,c("producto","year","mes","ciudad","departamento","comparacion_anual_producto","sd_precio_prom_ano_producto","cod_depto")]
data_comparacion_anual_producto <- data_comparacion_anual_producto %>% 
  distinct(departamento,ciudad,year,producto, .keep_all = TRUE)
mean_value <- mean(data_comparacion_anual_producto$sd_precio_prom_ano_producto, na.rm = TRUE)
data_comparacion_anual_producto$sd_precio_prom_ano_producto[is.na(data_comparacion_anual_producto$sd_precio_prom_ano_producto)] <- mean_value
# Base para la informacion anual 
data_comparacion_anual <- data[,c("producto","year","mes","ciudad","departamento","comparacion_anual","sd_precio_prom_ano","cod_depto")]
data_comparacion_anual <- data_comparacion_anual %>% 
  distinct(departamento,ciudad,year, .keep_all = TRUE)
data_comparacion_anual$producto <- NULL
# Base para la informacion general - producto
data_comparacion_producto <- data[,c("producto","year","mes","ciudad","departamento","comparacion_producto","sd_precio_prom_producto","cod_depto")]
data_comparacion_producto <- data_comparacion_producto %>% 
  distinct(departamento, ciudad, producto, .keep_all = TRUE)
data_comparacion_producto$year <- NULL
data_comparacion_producto$mes <- NULL
mean_value <-mean(data_comparacion_producto$sd_precio_prom_producto, na.rm = TRUE)
data_comparacion_producto$sd_precio_prom_producto[is.na(data_comparacion_producto$sd_precio_prom_producto)] <- mean_value
# Base para la informacion general
data_comparacion <- data[,c("producto","year","mes","ciudad","departamento","comparacion","sd_precio_prom","cod_depto")]
data_comparacion <- data_comparacion %>% 
  distinct(departamento,ciudad, .keep_all = TRUE)
data_comparacion$year <- NULL
data_comparacion$producto <- NULL
data_comparacion$mes <- NULL



# Guardamos las bases de datos
# Base para la informacion anual - mensual - producto
saveRDS(data_comparacion_anual_mensual_producto, "02_Indices/Output/base_precios_data_comparacion_anual_mensual_producto.rds")
saveRDS(data_comparacion_anual_mensual, "02_Indices/Output/base_precios_data_comparacion_anual_mensual.rds")
saveRDS(data_comparacion_mensual_producto, "02_Indices/Output/base_precios_data_comparacion_mensual_producto.rds")
saveRDS(data_comparacion_mensual, "02_Indices/Output/base_precios_data_comparacion_mensual.rds")
saveRDS(data_comparacion_anual_producto, "02_Indices/Output/base_precios_data_comparacion_anual_producto.rds")
saveRDS(data_comparacion_anual, "02_Indices/Output/base_precios_data_comparacion_anual.rds")
saveRDS(data_comparacion_producto, "02_Indices/Output/base_precios_data_comparacion_producto.rds")
saveRDS(data_comparacion, "02_Indices/Output/base_precios_data_comparacion.rds")





