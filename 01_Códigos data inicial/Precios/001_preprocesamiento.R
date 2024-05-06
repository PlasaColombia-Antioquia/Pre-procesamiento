################################################################################
#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################
#Autores: Juliana Lalinde, Laura Quintero
#Fecha de creacion: 11/02/2024
#Fecha de ultima modificacion: 11/02/2024
################################################################################

# Cargar las bibliotecas necesarias
pacman::p_load(readr,readxl,dplyr,glue,openxlsx,foreign,janitor)

### PRECIOS MAYORISTAS #########################################################
# Limpiar el entorno de trabajo
rm(list=ls())

for(year in 2013:2023) {
  if(year < 2018) {
    data <- read_excel("01_Datos/Data/Precios_Mayorista/Input/series-historicas-precios-mayoristas-2013-2017.xlsx",sheet = paste0("1.",year-2012))
  } else if (year == 2018) {
    data <- read_excel(paste0("01_Datos/Data/Precios_Mayorista/Input/series-historicas-precios-mayoristas-2018.xlsx"),sheet = "2018")
  } else {
    for(aux in 1:12) {
      mes <- read_excel(paste0("01_Datos/Data/Precios_Mayorista/Input/series-historicas-precios-mayoristas-",year,".xlsx"),sheet = aux+1)
      if(aux == 1) {
        data <- mes
      } else {
        data <- rbind(data,mes)
      }
    }
  }
  
  #Eliminar las filas extra de que pueda tener la tabla por formato
  data <- data[which(Reduce(`|`, lapply(data, grepl, pattern = "Fecha"))):nrow(data),]
  
  #Cambiar nombres de columnas
  colnames(data) <- data[1,]
  data <- data[2:nrow(data),]
  colnames(data)[4] <- "Fuente"
  
  #Eliminar columnas de N/A
  data <- data[,!names(data) %in% c("NA")]
  
  #Convertir el formato de las fechas
  data$Fecha <- excel_numeric_to_date(as.numeric(data$Fecha))
  
  #Separar municipio y mercado mayorista de destino
  data <- cbind(data,sub('.*,', '', data$Fuente))
  data$Fuente <- sub(',.*', '', data$Fuente)
  
  #Organizar nombres de columnas
  colnames(data) <- c("fecha","grupo_alimento","alimento","mpio_destino","precio","mercado_destino")
  
  #Mantener unicamente registros que involucren Antioquia
  data <- data[data$mpio_destino == "Medellín" | data$mpio_destino == "El Santuario (Antioquia)" | data$mpio_destino == "La Ceja (Antioquia)" | data$mpio_destino == "La Unión (Antioquia)" | data$mpio_destino == "Marinilla (Antioquia)" | data$mpio_destino == "Peñol (Antioquia)" | data$mpio_destino == "Rionegro (Antioquia)" | data$mpio_destino == "San Vicente (Antioquia)" | data$mpio_destino == "Santa Bárbara (Antioquia)" | data$mpio_destino == "Sonsón (Antioquia)" | data$mpio_destino == "Yarumal (Antioquia)" | data$mpio_destino == "Yolombó (Antioquia)",]
  
  data <- na.omit(data)
  
  if(year == 2013) {
    precios_mayoristas <- data
  } else {
    precios_mayoristas <- rbind(precios_mayoristas,data)
  }  
}

precios_mayoristas <- rename(precios_mayoristas, mpio_mercado_destino=mpio_destino)
precios_mayoristas$mpio_destino <- sapply(strsplit(precios_mayoristas$mpio_mercado_destino, " \\("), `[`, 1)
precios_mayoristas$mpio_destino <- ifelse(precios_mayoristas$mpio_destino == "San Vicente", "San Vicente Ferrer", precios_mayoristas$mpio_destino)


# Cargamos la base de datos del DANE
municipios <- read.csv("01_Datos/Data/Bases_auxiliares/Output/base_codigo_municipios.csv")
municipios <- rename(municipios, mpio_destino = Municipio, codigo_mpio_destino= codigo_mpio, cod_dpto_destino = cod_depto)
precios_mayoristas <- merge(precios_mayoristas,municipios, by = "mpio_destino", all.x = TRUE)

#Guardar la base
write.csv(precios_mayoristas, "01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_antioquia.csv")

