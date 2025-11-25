#Areas pequeñas

# Cargar librerías necesarias
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(readr)
library(xlsx)


# Cargar los datos
datosM <- read.xlsx("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/DatosOrigenAreasPequenas1.xlsx",1)

# Explorar la estructura de los datos
str(datosM)
summary(datosM)

# Manejar valores faltantes (por ejemplo, imputar o eliminar)
datosM <- datosM %>%
  mutate(across(everything(), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

# Verificar los nombres de las columnas
names(datosM)


#paso 2
# Cargar librerías
library(dplyr)

# Seleccionar columnas de interés usando dplyr::select
columnas_seleccionadas <- datosM %>%
  dplyr::select(IngresoProm, IngresoMed, PIBmun, `VA estatal`, ocupados, Mahalanobis_Distance, ImssIssste, MONTO_PREDIAL, Productividad)

# Verificar las columnas seleccionadas
head(columnas_seleccionadas)

# Calcular la matriz de correlación
correlation_matrix <- cor(columnas_seleccionadas, use = "complete.obs")  # use = "complete.obs" ignora los NA
print(correlation_matrix)


# Verificar la estructura de las columnas seleccionadas
str(columnas_seleccionadas)

# Verificar valores faltantes
summary(columnas_seleccionadas)

# Convertir columnas a numéricas si es necesario
columnas_seleccionadas <- columnas_seleccionadas %>%
  mutate(across(everything(), as.numeric))

# Reintentar calcular la matriz de correlación
correlation_matrix <- cor(columnas_seleccionadas, use = "complete.obs")
print(correlation_matrix)



# Visualizar relaciones clave
ggplot(columnas_seleccionadas, aes(x = PIBmun, y = IngresoProm)) +
  geom_point() +
  geom_smooth(method = "lm")



# Cargar librería para visualización de correlacion
library(ggplot2)
library(reshape2)

# Convertir la matriz de correlación a un formato largo
correlation_melted <- melt(correlation_matrix)

# Crear el heatmap
ggplot(correlation_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(title = "Matriz de Correlación", x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#paso 3
# Cargar librerías
library(dplyr)


# Verificar nombres de las columnas
names(datosM)

# Renombrar columnas si es necesario
datosM <- datosM %>%
  rename(
    MONTO_PREDIAL = `MONTO_PREDIAL`,  # Usa comillas invertidas si hay espacios o caracteres especiales
    MONTO_RECAUDADO_PERCAPITA = `MONTO_RECAUDADO_PERCAPITA`
  )

# Seleccionar columnas usando dplyr::select
columnas_seleccionadas <- datosM %>%
  dplyr::select(MONTO_PREDIAL, MONTO_RECAUDADO_PERCAPITA)

# Verificar la estructura de las columnas seleccionadas
str(columnas_seleccionadas)

# Convertir columnas a numéricas si es necesario
datosM <- datosM %>%
  mutate(
    MONTO_PREDIAL = as.numeric(MONTO_PREDIAL),
    MONTO_RECAUDADO_PERCAPITA = as.numeric(MONTO_RECAUDADO_PERCAPITA)
  )

# Calcular el resumen de las columnas seleccionadas
summary(columnas_seleccionadas)
         

#paso 4

# Cargar librerías
library(dplyr)
library(lme4)

# Escalar las variables predictoras
datosM <- datosM %>%
  mutate(across(c(VABruto, Ocupados, ImssIssste, MONTO_PREDIAL, Mahalanobis_Distance ), scale))

# Verificar las variables escaladas
summary(datosM %>% select(VABruto, Ocupados, ImssIssste, MONTO_PREDIAL, Mahalanobis_Distance))



# Cargar librería para modelos mixtos
library(lme4)

# Modelo lineal mixto
modelo <- lmer(IngresoProm ~ VABruto + Ocupados + ImssIssste + MONTO_PREDIAL + Mahalanobis_Distance + (1 | Municipio) + (1 | Ano), data = datosM)
summary(modelo)


modelo1 <- lmer(IngresoProm ~ IngresoMed+ PIBmun+ `VA estatal`+ ocupados+ Mahalanobis_Distance+ ImssIssste+ MONTO_PREDIAL+ Productividad + (1 | Municipio) + (1 | Ano), data = datosM)


#paso 5
#validar el modelo
# Predecir IngresoProm en los datos completos
datosM$IngresoProm_predicho <- predict(modelo, newdata = datosM)

# Calcular el error (RMSE)
rmse <- sqrt(mean((datosM$IngresoProm - datosM$IngresoProm_predicho)^2, na.rm = TRUE))
print(paste("RMSE:", rmse))

#paso 6, hacer predicciones
# Filtrar datos donde falta IngresoProm
datos_faltantes <- datosM %>% filter(is.na(IngresoProm))

# Predecir IngresoProm en los datos faltantes
datos_faltantes$IngresoProm_predicho <- predict(modelo, newdata = datos_faltantes, allow.new.levels = TRUE)

# Combinar los datos completos y las predicciones
datos_finales <- datosM %>%
  mutate(IngresoProm = ifelse(is.na(IngresoProm), IngresoProm_predicho, IngresoProm))


# Visualizar predicciones vs observaciones
ggplot(datos_finales, aes(x = IngresoProm, y = IngresoProm_predicho)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  labs(title = "Ingreso Promedio Observado vs Predicho", x = "Observado", y = "Predicho")

# Exportar los datos finales
write.csv(datos_finales, "Datos_Ingreso_Predicho.csv", row.names = FALSE)






####otrea forma

# Cargar librerías necesarias
install.packages("lme4")  # Instalar el paquete lme4 si no lo tienes
library(lme4)
library(dplyr)

# Cargar datos (supongamos que tu base de datos se llama 'datos')
datos <- read.csv("tu_base_de_datos.csv")


# Función para aplicar SAE por estado y año
estimar_ingresos_SAE <- function(datos_estado_ano) {
  # Filtrar datos con valores observados de ingreso
  datos_observados <- datos_estado_ano %>% filter(!is.na(IngresoProm))
  
  # Datos para predicción (municipios sin ingreso observado)
  datos_predecir <- datos_estado_ano %>% filter(is.na(IngresoProm))
  
  # Ajustar un modelo lineal mixto (mixed-effects model)
  modelo <- lmer(
    IngresoProm ~ POB_15_64 + Ocupados + PEA + Mahalanobis_Distance + MONTO_PREDIAL +VABruto+ (1 | Municipio),  # Efectos fijos y aleatorios
    data = datos_observados  # Datos con valores observados
  )
  
  # Predecir ingresos para municipios sin datos observados
  predicciones <- predict(modelo, newdata = datos_predecir, allow.new.levels = TRUE)
  
  # Combinar resultados
  datos_estado_ano <- datos_estado_ano %>%
    mutate(IngresoProm_estimado = ifelse(is.na(IngresoProm), predicciones, IngresoProm))
  
  return(datos_estado_ano)
}

# Aplicar la función para cada estado y año
resultados <- datos %>%
  group_by(Entidad, Ano) %>%
  group_modify(~ estimar_ingresos_SAE(.x))

# Guardar resultados (opcional)
write.csv(resultados, "resultados_ingresos_estimados.csv", row.names = FALSE)


####


# Cargar librerías necesarias
install.packages(c("sf", "gstat", "tmap", "ggplot2"))  # Instalar paquetes si no los tienes
library(sf)
library(gstat)
library(tmap)
library(ggplot2)

# 1. Cargar el shapefile municipal
shapefile <- st_read("/Users/dmares/Documents/Cursos tomados/Geo-SIG/Practicas/889463807469_s/MG_2020_Integrado/conjunto_de_datos/00mun.shp")  # Reemplaza con la ruta a tu shapefile

# 2. Cargar los datos de ingresos (supongamos que ya tienes los datos en un data.frame)
datos <- read.csv("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/DatosOrigenAreasPequenas.csv")  # Reemplaza con la ruta a tus datos

# 3. Combinar los datos de ingresos con el shapefile
# Supongamos que tienes una columna "CVE_MUN" en ambos conjuntos de datos
datos_geo <- merge(shapefile, datos, by = "CVE_MUN")

# 4. Verificar el sistema de referencia espacial (CRS)
st_crs(datos_geo)  # Debe mostrar un CRS válido (por ejemplo, EPSG:4326 para lat/lon)

# 5. Convertir los datos a un objeto "sf" (si no lo están ya)
datos_geo <- st_as_sf(datos_geo)


# Imputar valores faltantes con la media
datos_geo <- datos_geo %>%
  mutate(
    IngresoProm = ifelse(is.na(IngresoProm), mean(IngresoProm, na.rm = TRUE), IngresoProm),
    Mahalanobis_Distance = ifelse(is.na(Mahalanobis_Distance), mean(Mahalanobis_Distance, na.rm = TRUE), Mahalanobis_Distance)
  )


library(gstat)

# 6. Ajustar un modelo de Kriging
# Ajustar el modelo de Kriging con datos imputados
modelo_kriging <- gstat(
  formula = IngresoProm ~ Mahalanobis_Distance,  # Fórmula del modelo
  data = datos_geo,                             # Datos con valores imputados
  nmax = 50                                     # Número máximo de vecinos a considerar
)




# 7. Realizar la interpolación (Kriging)
# Crear una cuadrícula para las predicciones (puedes usar el shapefile como base)
cuadricula <- st_make_grid(shapefile, n = 100)  # Crear una cuadrícula de 100x100 celdas
valor_constante <- mean(datos_geo$Mahalanobis_Distance, na.rm = TRUE)
cuadricula <- st_sf(geometry = cuadricula, Mahalanobis_Distance = valor_constante)


###opción 1 para incorporar Mahalanobis_Distance
#modelo_mahalanobis <- gstat(formula = Mahalanobis_Distance ~ 1, data = datos_geo)
# cuadricula <- st_sf(geometry = cuadricula, Mahalanobis_Distance = predict(modelo_mahalanobis, newdata = cuadricula)$var1.pred)


predicciones <- predict(modelo_kriging, newdata = cuadricula)

# 8. Visualizar los resultados
# Usar tmap para visualizar los ingresos estimados
tm_shape(predicciones) +
  tm_raster(col = "var1.pred", palette = "YlOrRd", title = "Ingreso Promedio Estimado") +
  tm_shape(datos_geo) +
  tm_dots(size = 0.1, col = "red", alpha = 0.5) +
  tm_layout(legend.outside = TRUE)

# Alternativa: Usar ggplot2 para visualizar
ggplot() +
  geom_sf(data = predicciones, aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c(option = "plasma", name = "Ingreso Promedio Estimado") +
  theme_minimal()



#trabajar por año:
# Cargar librerías necesarias
library(sf)
library(gstat)
library(dplyr)
library(purrr)

# 1. Dividir los datos por año
datos_por_ano <- split(datos_geo, datos_geo$Ano)

# 2. Función para aplicar Kriging por año
interpolar_por_ano <- function(datos_ano) {
  # Filtrar datos con valores observados de ingreso
  datos_observados <- datos_ano %>% filter(!is.na(IngresoProm))
  
  # Verificar si hay suficientes datos para ajustar el modelo
  if (nrow(datos_observados) < 5) {
    # Si no hay suficientes datos, devolver NA para los ingresos estimados
    datos_ano <- datos_ano %>%
      mutate(IngresoProm_estimado = NA)
    return(datos_ano)
  }
  
  # Ajustar un modelo de Kriging
  modelo_kriging <- gstat(
    formula = IngresoProm ~ Mahalanobis_Distance,  # Fórmula del modelo
    data = datos_observados,                      # Datos observados
    nmax = 50                                     # Número máximo de vecinos
  )
  
  # Predecir ingresos para todos los municipios (incluso los sin datos observados)
  predicciones <- predict(modelo_kriging, newdata = datos_ano)
  
  # Combinar resultados
  datos_ano <- datos_ano %>%
    mutate(IngresoProm_estimado = ifelse(is.na(IngresoProm), predicciones$var1.pred, IngresoProm))
  
  return(datos_ano)
}

# 3. Aplicar la función para cada año
resultados_por_ano <- map_dfr(datos_por_ano, interpolar_por_ano)

# 4. Guardar resultados (opcional)
write.csv(resultados_por_ano, "resultados_ingresos_estimados_por_ano.csv", row.names = FALSE)

# Visualizar resultados para un año específico (por ejemplo, 2020)
resultados_2020 <- resultados_por_ano %>% filter(Ano == 2020)

tm_shape(resultados_2020) +
  tm_polygons(col = "IngresoProm_estimado", palette = "YlOrRd", title = "Ingreso Promedio Estimado (2020)") +
  tm_layout(legend.outside = TRUE)

###




###3modelo lineal simple para grupos pequeños
# Función para aplicar SAE por estado y año
estimar_ingresos_SAE <- function(datos_estado_ano) {
  # Filtrar datos con valores observados de ingreso
datos_observados <- datos_estado_ano %>% filter(!is.na(IngresoProm))
  
  # Verificar si hay suficientes datos para ajustar el modelo mixto
if (nrow(datos_observados) < 2) {
    # Si no hay suficientes datos, ajustar un modelo lineal simple
modelo <- lm(IngresoProm ~ POB_15_64 + Ocupados + VABruto+ PEA+Mahalanobis_Distance+MONTO_PREDIAL, data = datos_observados)
  } else {
    # Ajustar un modelo lineal mixto (mixed-effects model)
modelo <- lmer(IngresoProm ~ POB_15_64 + Ocupados + VABruto+ PEA+Mahalanobis_Distance+MONTO_PREDIAL + (1 | Municipio), data = datos_observados)
  }
  
  # Datos para predicción (municipios sin ingreso observado)
datos_predecir <- datos_estado_ano %>% filter(is.na(IngresoProm))
  
  # Predecir ingresos para municipios sin datos observados
predicciones <- predict(modelo, newdata = datos_predecir, allow.new.levels = TRUE)
  
  # Combinar resultados
datos_estado_ano <- datos_estado_ano %>%
    mutate(IngresoProm_estimado = ifelse(is.na(IngresoProm), predicciones, IngresoProm))
  
  return(datos_estado_ano)
}


View(datos_estado_ano)


# Calcular métricas de validación
validacion <- resultados %>%
  filter(!is.na(IngresoProm)) %>%  # Solo municipios con datos observados
  summarise(
    RMSE = sqrt(mean((IngresoProm - IngresoProm_estimado)^2)),  # Error Cuadrático Medio
    R2 = cor(IngresoProm, IngresoProm_estimado)^2  # Coeficiente de Determinación
  )

print(validacion)






#usamos proceso de Kriging en etapas
# Cargar librerías necesarias
library(sf)
library(gstat)
library(dplyr)
library(purrr)

# 1. Dividir los datos por año
datos_geo <- merge(shapefile, datos, by = "CVE_MUN")
datos_por_ano <- split(datos_geo, datos_geo$Ano)

# 2. Función para aplicar Kriging por año
interpolar_por_ano <- function(datos_ano, ano) {
  # Filtrar datos con valores observados de ingreso
  datos_observados <- datos_ano %>% filter(!is.na(IngresoProm))
  
  # Verificar si hay suficientes datos para ajustar el modelo
  if (nrow(datos_observados) < 5) {
    # Si no hay suficientes datos, devolver NA para los ingresos estimados
    datos_ano <- datos_ano %>%
      mutate(IngresoProm_estimado = NA)
    return(datos_ano)
  }
  
  # Ajustar un modelo de Kriging
  modelo_kriging <- gstat(
    formula = IngresoProm ~ Mahalanobis_Distance,  # Fórmula del modelo
    data = datos_observados,                      # Datos observados
    nmax = 50                                     # Número máximo de vecinos
  )
  
  # Predecir ingresos para todos los municipios (incluso los sin datos observados)
  predicciones <- predict(modelo_kriging, newdata = datos_ano)
  
  # Combinar resultados
  datos_ano <- datos_ano %>%
    mutate(IngresoProm_estimado = ifelse(is.na(IngresoProm), predicciones$var1.pred, IngresoProm))
  
  # Liberar memoria
  gc()
  
  # Guardar resultados parciales
  write.csv(datos_ano, paste0("resultados_ingresos_estimados_", ano, ".csv"), row.names = FALSE)
  
  return(datos_ano)
}

# 3. Aplicar la función para cada año y guardar resultados parciales
resultados_por_ano <- map2_dfr(datos_por_ano, names(datos_por_ano), ~ interpolar_por_ano(.x, .y))

# 4. Combinar todos los resultados en un solo archivo (opcional)
resultados_finales <- list.files(pattern = "resultados_ingresos_estimados_*") %>%
  map_dfr(read.csv)

write.csv(resultados_finales, "resultados_ingresos_estimados_finales.csv", row.names = FALSE)


#####
###uso de  Inverso de la Distancia Ponderada (IDW)
# Cargar librerías necesarias
library(sf)
library(gstat)
library(dplyr)
library(purrr)

# 1. Convertir los datos a formato sf (si no lo están ya)
# Supongamos que tienes una columna de geometría en tu shapefile
datos_geo <- merge(shapefile, datos, by = "CVE_MUN")
datos_por_ano <- split(datos_geo, datos_geo$Ano)
datos_geo <- st_as_sf(datos_geo)

# Verificar el sistema de referencia espacial (CRS)
st_crs(datos_geo)  # Debe mostrar un CRS válido (por ejemplo, EPSG:4326 para lat/lon)

# 2. Dividir los datos por año
datos_por_ano <- split(datos_geo, datos_geo$Ano)

# 3. Función para aplicar IDW por año
interpolar_por_ano_idw <- function(datos_ano, ano) {
  # Filtrar datos con valores observados de ingreso
  datos_observados <- datos_ano %>% filter(!is.na(IngresoProm))
  
  # Verificar si hay suficientes datos para ajustar el modelo
  if (nrow(datos_observados) < 5) {
    # Si no hay suficientes datos, devolver NA para los ingresos estimados
    datos_ano <- datos_ano %>%
      mutate(IngresoProm_estimado = NA)
    return(datos_ano)
  }
  
  # Ajustar un modelo de IDW
  modelo_idw <- gstat(
    formula = IngresoProm ~ 1,  # Fórmula del modelo (sin predictores)
    data = datos_observados,    # Datos observados (en formato sf)
    nmax = 50                   # Número máximo de vecinos
  )
  
  # Predecir ingresos para todos los municipios (incluso los sin datos observados)
  predicciones <- predict(modelo_idw, newdata = datos_ano)
  
  # Combinar resultados
  datos_ano <- datos_ano %>%
    mutate(IngresoProm_estimado = ifelse(is.na(IngresoProm), predicciones$var1.pred, IngresoProm))
  
  # Liberar memoria
  gc()
  
  # Guardar resultados parciales
  write.csv(datos_ano, paste0("resultados_ingresos_estimados_", ano, ".csv"), row.names = FALSE)
  
  return(datos_ano)
}

# 4. Aplicar la función para cada año y guardar resultados parciales
resultados_por_ano_idw <- map2_dfr(datos_por_ano, names(datos_por_ano), ~ interpolar_por_ano_idw(.x, .y))

# 5. Combinar todos los resultados en un solo archivo (opcional)
resultados_finales <- list.files(pattern = "resultados_ingresos_estimados_*") %>%
  map_dfr(read.csv)

write.csv(resultados_finales, "resultados_ingresos_estimados_finales.csv", row.names = FALSE)





# Función para aplicar Kriging por año
interpolar_por_ano_kriging <- function(datos_ano, ano) {
  # Filtrar datos con valores observados de ingreso
  datos_observados <- datos_ano %>% filter(!is.na(IngresoProm))
  
  # Verificar si hay suficientes datos para ajustar el modelo
  if (nrow(datos_observados) < 5) {
    # Si no hay suficientes datos, devolver NA para los ingresos estimados
    datos_ano <- datos_ano %>%
      mutate(IngresoProm_estimado = NA)
    return(datos_ano)
  }
  
  # Ajustar un modelo de Kriging
  modelo_kriging <- gstat(
    formula = IngresoProm ~ Mahalanobis_Distance,  # Fórmula del modelo
    data = datos_observados,                      # Datos observados (en formato sf)
    nmax = 50                                     # Número máximo de vecinos
  )
  
  # Predecir ingresos para todos los municipios (incluso los sin datos observados)
  predicciones <- predict(modelo_kriging, newdata = datos_ano)
  
  # Combinar resultados
  datos_ano <- datos_ano %>%
    mutate(IngresoProm_estimado = ifelse(is.na(IngresoProm), predicciones$var1.pred, IngresoProm))
  
  # Liberar memoria
  gc()
  
  # Guardar resultados parciales
  write.csv(datos_ano, paste0("resultados_ingresos_estimados_", ano, ".csv"), row.names = FALSE)
  
  return(datos_ano)
}

# Aplicar la función para cada año y guardar resultados parciales
resultados_por_ano_kriging <- map2_dfr(datos_por_ano, names(datos_por_ano), ~ interpolar_por_ano_kriging(.x, .y))




# Cargar librerías necesarias
library(readr)
library(dplyr)

# 1. Listar los archivos CSV generados
archivos_csv <- list.files(pattern = "resultados_ingresos_estimados_*")

# 2. Función para leer cada archivo CSV de manera segura
leer_csv_seguro <- function(archivo) {
  tryCatch({
    # Leer el archivo CSV con readr
    datos <- read_csv(
      archivo,
      col_types = cols(.default = "c"),  # Leer todas las columnas como texto
      na = c("", "NA")                   # Tratar cadenas vacías como NA
    )
    
    # Verificar que el archivo tenga al menos algunas columnas clave
    columnas_minimas <- c("Municipio", "IngresoProm")  # Columnas mínimas requeridas
    if (!all(columnas_minimas %in% names(datos))) {
      message("El archivo ", archivo, " no tiene las columnas mínimas requeridas.")
      return(NULL)  # Devolver NULL si faltan columnas clave
    }
    
    return(datos)  # Devolver los datos si están correctos
  }, error = function(e) {
    message("Error al leer el archivo: ", archivo)
    message("Mensaje de error: ", e$message)
    return(NULL)  # Devolver NULL si hay un error
  })
}

# 3. Leer y combinar todos los archivos CSV
resultados_finales <- map_dfr(archivos_csv, leer_csv_seguro)

# 4. Guardar los resultados combinados en un solo archivo CSV
write_csv(resultados_finales, "resultados_ingresos_estimados_finales.csv")






# Cargar librerías necesarias
library(data.table)

# 1. Listar los archivos CSV generados
archivos_csv <- list.files(pattern = "resultados_ingresos_estimados_*")

# 2. Leer y combinar todos los archivos CSV sin verificar columnas
resultados_finales <- rbindlist(lapply(archivos_csv, fread), fill = TRUE)

# 3. Guardar los resultados combinados en un solo archivo CSV
fwrite(resultados_finales, "resultados_ingresos_estimados_finales.csv")
