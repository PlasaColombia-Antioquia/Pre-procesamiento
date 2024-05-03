################################################################################
#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################
#Autores: Juliana Lalinde, Laura Quintero
#Fecha de creacion: 11/02/2024
#Fecha de ultima modificacion: 07/02/2024
################################################################################

# Cargar las bibliotecas necesarias
pacman::p_load(readr,readxl,dplyr,glue,openxlsx,foreign,janitor)

### ABASTECIMIENTO - MICRODATOS ################################################
# Limpiar el entorno de trabajo
rm(list=ls())


  for(year in 2013:2023) {
    for(half in 1:2) {
      if(year == 2022) {
        data <- read_excel(paste0("01_Datos/Data/Abastecimiento_microdatos/Input/microdato-abastecimiento-2022.xlsx"),sheet = paste0("2.",half))
      } else if (year == 2023) {
        data <- read_excel(paste0("01_Datos/Data/Abastecimiento_microdatos/Input/anex-SIPSAbastecimiento-Microdatos-2023.xlsx"),sheet = paste0("2.",half))
      } else {
        data <- read_excel(paste0("01_Datos/Data/Abastecimiento_microdatos/Input/microdato-abastecimiento-",year,".xlsx"),sheet = paste0("1.",half))
      }
      
      #Eliminar las filas extra de que pueda tener la tabla por formato
      data <- data[which(Reduce(`|`, lapply(data, grepl, pattern = "Cuidad, Mercado Mayorista"))):nrow(data),]
      
      #Cambiar nombres de columnas
      colnames(data) <- data[1,]
      data <- data[2:nrow(data),]
      
      #Eliminar columnas de N/A
      data <- data[,!names(data) %in% c("NA")]
      
      #Convertir el formato de las fechas
      if(year==2020){
        data$Fecha <- as.Date(data$Fecha, format = "%d/%m/%y")
      }else{
        data$Fecha <- excel_numeric_to_date(as.numeric(data$Fecha))
      }
      
      
      #Organizar número de departamento y municipio
      data$`Código Departamento` <- substr(data$`Código Departamento`,2,3)
      data$`Código Municipio` <- substr(data$`Código Municipio`,2,6)
      
      #Separar municipio y mercado mayorista de destino
      data <- cbind(sub('.*,', '', data$`Cuidad, Mercado Mayorista`),data)
      data$`Cuidad, Mercado Mayorista` <- sub(',.*', '', data$`Cuidad, Mercado Mayorista`)
      
      #Organizar nombres de columnas
      colnames(data) <- c("mercado_destino","mpio_destino","fecha","codigo_depto_origen","codigo_mpio_origen","depto_origen","mpio_origen","grupo_alimento","alimento","cantidad_kg")
      
      #Mantener unicamente registros que involucren Antioquia
      #data <- data[(data$depto_origen=="ANTIOQUIA" | data$mpio_destino == "Medellín"),]
      
      data <- na.omit(data)
      
      if(year == 2013 & half == 1) {
        abastecimiento_microdatos <- data
      } else {
        abastecimiento_microdatos <- rbind(abastecimiento_microdatos,data)
      }
    }
  }



abastecimiento_microdatos <- rename(abastecimiento_microdatos, mpio_destino_dpto = mpio_destino)


abastecimiento_microdatos$mpio_destino <- sapply(abastecimiento_microdatos$mpio_destino_dpto, function(x) {
  if (grepl(" \\(", x)) {
    return(strsplit(x, " \\(")[[1]][1])
  } else {
    return(x)
  }
})


# Cargar la base de municipios 
municipios <- read.csv("01_Datos/Data/Bases_auxiliares/Output/base_codigo_municipios.csv")

municipios <- municipios[,c("Municipio","codigo_mpio","Departamento")] 

municipios <- rename(municipios, mpio_destino = Municipio, codigo_mpio_destino= codigo_mpio)





# abastecimiento_microdatos
abastecimiento_microdatos$mpio_destino <- ifelse(abastecimiento_microdatos$mpio_destino == "Cartagena", "Cartagena de Indias", abastecimiento_microdatos$mpio_destino )
abastecimiento_microdatos <- merge(abastecimiento_microdatos,municipios, by = "mpio_destino", all.x = TRUE)
abastecimiento_microdatos<-abastecimiento_microdatos%>%filter(codigo_mpio_destino != 5059)
num_na <- sum(is.na(abastecimiento_microdatos$codigo_mpio_destino))




#Guardar la base
#saveRDS(abastecimiento_microdatos, "FAO/01_Analisis_Empirico/01_Datos/Data/Abastecimiento_microdatos/Output/abastecimiento_microdatos_antioquia.rds")
write.csv(abastecimiento_microdatos, "01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_microdatos_antioquia.csv", row.names = FALSE)


abastecimiento_origen_antioquia <- data[data$depto_origen=="ANTIOQUIA",]
write.csv(abastecimiento_origen_antioquia, "01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_microdatos_antioquia_origen.csv")

abastecimiento_destino_antioquia <- data[data$mpio_destino == "Medellín",]
write.csv(abastecimiento_destino_antioquia, "01_Datos/Data/Abastecimiento_microdatos/Output/base_abastecimiento_microdatos_antioquia_destino.csv")

