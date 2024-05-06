#Proyecto FAO
#Procesamiento datos SIPSA
################################################################################
#Autores: Juliana Lalinde, Laura Quintero, Germán Angulo
#Fecha de creacion: 19/02/2024
#Fecha de ultima modificacion: 21/02/2024
################################################################################
# Paquetes 
################################################################################
library(readr);library(lubridate);library(dplyr);library(ggplot2);library(zoo);library(readxl)
library(glue);library(tidyverse)
options(scipen = 999)
################################################################################

#mayoristas----
# Obtiene una lista de los archivos .xls que comienzan con "mayoristas_"
archivos <- list.files(path = "01_Datos/Data/Precios_Mayorista/Input/Precios diarios", pattern = "mayoristas_.*")

# Inicializa una lista para almacenar los dataframes
dataframes <- list()

errores <- list()

# Itera sobre los archivos, lee cada uno y añádelo a la lista

for (archivo in archivos) {
  tryCatch({
    df_temp <- read_excel(glue("01_Datos/Data/Precios_Mayorista/Input/Precios diarios/{archivo}"), skip = 0) %>%
      drop_na("...2") %>%
      janitor::row_to_names(row_number = 1) %>%
      select(which(!is.na(names(.)))) %>%
      rename(producto=`Precio $/Kg`) %>%
      drop_na(producto)
    
    num_columnas <- ncol(df_temp)
    
    dataframes[[archivo]] <- df_temp %>%
      gather(key = "ciudad", value = "precio",c(2:num_columnas) ) %>%
      mutate(producto=str_replace(producto,"\\*.*",""),precio=as.numeric(precio), 
             ciudad=str_replace(word(ciudad, 1),",.*",""),fecha=archivo)
  }, error = function(e) {
    errores[[archivo]] <- e
  })
}

# Combina todos los dataframes en una sola base de datos
df_final <- bind_rows(dataframes)%>%mutate(nom_archivo=fecha)
df_final$fecha <- str_replace_all(df_final$fecha, "mayoristas_anexo_|mayoristas_|\\.xlsx|\\.xls", "")
df_final$fecha <- str_replace_all(df_final$fecha, "ene_", "enero_")
df_final$fecha <- parse_date_time(df_final$fecha, orders = c("B_d_Y", "b_d_Y"))
#Guardar la base
write.csv(df_final, "01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_diario_2013-2021.csv")

#Corroborando los 20 que faltan
n<-data_frame(archivos)

m<-data_frame(archivos=unique(df_final$nom_archivo))%>%mutate(esta=1)#%>%rename(m$`unique(df_final$fecha)`)

o<-n%>%left_join(m, by = "archivos")


#anex_----
# Obtiene una lista de los archivos .xls que comienzan con "anex_"
archivos <- list.files(path = "01_Datos/Data/Precios_Mayorista/Input/Precios diarios", pattern = "anex_.*")

# Inicializa una lista para almacenar los dataframes
dataframes2 <- list()

errores2 <- list()

# Itera sobre los archivos, lee cada uno y añádelo a la lista

for (archivo in archivos) {
  tryCatch({
    df_temp <- read_excel(glue("01_Datos/Data/Precios_Mayorista/Input/Precios diarios/{archivo}"), skip = 0) %>%
      drop_na("...2") %>%
      janitor::row_to_names(row_number = 1) %>%
      select(which(!is.na(names(.)))) %>%
      rename(producto=`Precio $/Kg`) %>%
      drop_na(producto)
    
    num_columnas <- ncol(df_temp)
    
    dataframes2[[archivo]] <- df_temp %>%
      gather(key = "ciudad", value = "precio",c(2:num_columnas) ) %>%
      mutate(producto=str_replace(producto,"\\*.*",""),precio=as.numeric(precio), 
             ciudad=str_replace(word(ciudad, 1),",.*",""),fecha=archivo)
  }, error = function(e) {
    errores2[[archivo]] <- e
  })
}

# Combina todos los dataframes en una sola base de datos
df_final2 <- bind_rows(dataframes2)%>%mutate(nom_archivo=fecha)
df_final2$fecha <- str_replace_all(df_final2$fecha, "anex_|mayoristas_|\\.xlsx|\\.xls", "")
df_final2$fecha <- df_final2$fecha %>%str_replace_all("abr_", "abril_") %>%
  str_replace_all("ene_", "enero_")%>%str_replace_all("dic_", "diciembre_")%>%
  str_replace_all("ago_", "agosto_") 
df_final2$fecha <- parse_date_time(df_final2$fecha, orders = c("B_d_Y", "b_d_Y"))

#Guardar la base
write.csv(df_final2, "01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_diario_2022.csv")

#Corroborando los 20 que faltan
n2<-data_frame(archivos)
m2<-data_frame(archivos=unique(df_final2$fecha))%>%mutate(esta=1)#%>%rename(m$`unique(df_final$fecha)`)

o2<-n2%>%left_join(m2, by = "archivos")


#anex_----
# Obtiene una lista de los archivos .xls que comienzan con "anex_"
archivos <- list.files(path = "01_Datos/Data/Precios_Mayorista/Input/Precios diarios", pattern = "anex-S.*")

# Inicializa una lista para almacenar los dataframes
dataframes3 <- list()

errores3 <- list()

# Itera sobre los archivos, lee cada uno y añádelo a la lista

for (archivo in archivos) {
  tryCatch({
    df_temp <- read_excel(glue("01_Datos/Data/Precios_Mayorista/Input/Precios diarios/{archivo}"), skip = 0) %>%
      drop_na("...2") %>%
      janitor::row_to_names(row_number = 1) %>%
      select(which(!is.na(names(.)))) %>%
      rename(producto=`Precio $/Kg`) %>%
      drop_na(producto)
    
    num_columnas <- ncol(df_temp)
    
    dataframes3[[archivo]] <- df_temp %>%
      gather(key = "ciudad", value = "precio",c(2:num_columnas) ) %>%
      mutate(producto=str_replace(producto,"\\*.*",""),precio=as.numeric(precio), 
             ciudad=str_replace(word(ciudad, 1),",.*",""),fecha=archivo)
  }, error = function(e) {
    errores3[[archivo]] <- e
  })
}

# Combina todos los dataframes en una sola base de datos
df_final3 <- bind_rows(dataframes3)%>%mutate(nom_archivo=fecha)
df_final3$fecha <- str_replace_all(df_final3$fecha, "anex-SIPSADiario-|\\.xlsx|\\.xls", "")
# Reemplaza los nombres de los meses en español por sus equivalentes en inglés
df_final3$fecha <- str_replace_all(df_final3$fecha, c("ene" = "Jan", "feb" = "Feb", "mar" = "Mar", "abr" = "Apr", "may" = "May", "jun" = "Jun", "jul" = "Jul", "ago" = "Aug", "sep" = "Sep", "oct" = "Oct", "nov" = "Nov", "dic" = "Dec"))
# Convierte la cadena de caracteres a formato de fecha
df_final3$fecha <- dmy(df_final3$fecha)
#Guardar la base
write.csv(df_final3, "01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_diario_2023.csv")

 #Corroborando los 20 que faltan
n3<-data_frame(archivos)
m3<-data_frame(archivos=unique(df_final3$fecha))%>%mutate(esta=1)#%>%rename(m$`unique(df_final$fecha)`)

o3<-n3%>%left_join(m3, by = "archivos")

#Para pruebas
nombre_base_datos <- "mayoristas_anexo_ene_02_2013"
ultimos_caracteres <- str_sub(nombre_base_datos, -11)

#Individuales----
# Lee el archivo Excel y realiza las transformaciones
feb_8 <- read_excel("01_Datos/Data/Precios_Mayorista/Input/Precios diarios/mayoristas_febrero_8_2017.xls", skip = 0) %>%
  drop_na("...2") %>%
  janitor::row_to_names(row_number = 1)

# Reemplaza los nombres de columnas NA y asegúrate de que sean únicos
names(feb_8) <- make.names(names(feb_8), unique = TRUE)

# Renombra la primera columna a "producto"
feb_8 <- rename(feb_8, producto = names(feb_8)[1])%>%select(-starts_with("NA.."))%>%
  drop_na(producto)%>%gather(key = "ciudad", value = "precio",c(2:12) )%>%
  mutate(producto=str_replace(producto,"\\*.*",""),precio=as.numeric(precio), 
         ciudad=str_replace(word(ciudad, 1),",.*",""),fecha=str_sub("febrero_8_2017"),
         nom_archivo="mayoristas_febrero_8_2017.xls")

feb_8$fecha <- as.Date(feb_8$fecha, format = "%B_%d_%Y")


# Lee el archivo Excel y limpia los nombres de las columnas
mar_10 <- read_excel("01_Datos/Data/Precios_Mayorista/Input/Precios diarios/anex_mar_10_2022.xlsx", skip = 0) %>%
  drop_na("...2") %>%
  janitor::row_to_names(row_number = 1) %>%
  janitor::clean_names()

# Haz que los nombres de las columnas sean únicos
names(mar_10) <- make.unique(names(mar_10))

# Selecciona solo las columnas con nombres no NA
mar_10 <- mar_10 %>% 
  select(which(!is.na(names(.)))) %>% 
  select(-starts_with("na")) %>% 
  rename(producto = precio_kg) %>% 
  drop_na(producto) %>% 
  mutate_at(vars(c(2:14)), as.numeric) %>% 
  mutate(pereira = rowMeans(cbind(pereira_mercasa, pereira_mercasa_2), na.rm = TRUE))%>%
  select(-starts_with("pereira_mercasa")) %>% gather(key = "ciudad", value = "precio",c(2:13) )%>%
  mutate(producto=str_replace(producto,"\\*.*",""),precio=as.numeric(precio), 
         ciudad=str_replace(word(ciudad, 1),"_.*",""),fecha=str_sub("marzo_10_2022"),
         nom_archivo="anex_mar_10_2022.xlsx")

mar_10$fecha <- as.Date(mar_10$fecha, format = "%B_%d_%Y")


#Nov_2021----

nov_2021 <- read_excel("01_Datos/Data/Precios_Mayorista/Input/Precios diarios/Sem_22nov2021_26nov2021.xlsx", skip = 5, sheet = "Precios_mayoristas")%>%
  janitor::clean_names()%>%rename(precio="x8")%>%mutate(nom_archivo="Sem_22nov2021_26nov2021.xlsx")%>%
  select(producto,ciudad,precio,fecha,nom_archivo)%>%drop_na(producto)
indv<-rbind(feb_8,mar_10,nov_2021)

write.csv(indv, "01_Datos/Data/Precios_Mayorista/Output/base_precios_mayorista_diario_dia_indv.csv")
