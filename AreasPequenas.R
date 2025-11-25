##Areas pequeñas

library(readr)
install.packages("openxlsx")
library(openxlsx)

datos <- read.xlsx("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/DatosOrigenAreasPequenas1.xlsx",1)
str(datos)
summary(datos)



#necesito agregar la varianza
library(dplyr)

#####
###Calcular distancia de Mahalanobis en el mismo excel
library(stats)
vars <- c("IngresoProm", "IngresoMed", "PIBmun", "Mahalanobis_Distance", "MONTO_RECAUDADO_PERCAPITA")
data_subset <- datos[, vars]
# Calcular la media y matriz de covarianza
media <- colMeans(data_subset, na.rm = TRUE)
cov_mat <- cov(data_subset, use = "complete.obs")

apply(data_subset, 2, var, na.rm = TRUE)

# Calcular la distancia de Mahalanobis
datos$Mahalanobis_Distance <- mahalanobis(data_subset, center = media, cov = cov_mat)

#no sale, entonces corregimos matriz de covarianzas
# Escalar las variables
data_scaled <- scale(data_subset)
# Calcular la media y la matriz de covarianza de las variables escaladas
media <- colMeans(data_scaled, na.rm = TRUE)
cov_mat <- cov(data_scaled, use = "complete.obs")

# Calcular la distancia de Mahalanobis
datos$Dist_Mahalanobis <- mahalanobis(data_scaled, center = media, cov = cov_mat)




# Guardar la base de datos actualizada con la nueva columna
write.csv(datos, "datos_con_distancia_mahalanobis.csv", row.names = FALSE)

# Verificar la nueva columna
summary(datos$Dist_Mahalanobis)
# Verificar que la columna de distancia de Mahalanobis esté presente
head(datos$Dist_Mahalanobis)



#########Aquí calculamos otra varianza, la del ingreso
# Calcular la varianza del ingreso a nivel estatal por año
varianza_estatal <- datos %>%
  group_by(Entidad, Ano) %>%  # Agrupar por estado y año
  summarise(var_ingreso_estatal = var(IngresoProm, na.rm = TRUE), .groups = "drop")

# Unir la varianza a la base de datos original
datos <- left_join(datos, varianza_estatal, by = c("Entidad", "Ano"))
datos <- datos %>%
  group_by(CVE_MUN) %>%
  mutate(IngresoProm = zoo::na.approx(IngresoProm, na.rm = FALSE))
View(datos)
##no tenemos datos completos de: SeguPopu + MatTotal2 +


variables_aux <- c("IngresoProm", "IngresoMed", "PIBmun", "Mahalanobis_Distance",  "POB_15_64", "POB_60_MAS", "POB_65_MAS",
                   "RAZ_DEP_ADU", "PEA", "ocupados", 
                   "ImssIssste", "MONTO_PREDIAL", "MONTO_RECAUDADO_PERCAPITA")
colSums(is.na(datos[, variables_aux]))

datos <- datos %>%
  mutate(across(c("IngresoMed", "PIBmun", "Mahalanobis_Distance",  "POB_15_64", "POB_60_MAS", "POB_65_MAS",
                  "RAZ_DEP_ADU", "PEA", "ocupados", 
                  "ImssIssste", "MONTO_PREDIAL", "MONTO_RECAUDADO_PERCAPITA"), 
                ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))

names(datos)


#calculamos relacion entre variables

correlacion <- cor(datos[, c("VA.estatal", "POB_15_64", "RAZ_DEP_ADU", "PEA", "ocupados", 
                             "Mahalanobis_Distance", "ImssIssste", "MONTO_PREDIAL", 
                             "MONTO_RECAUDADO_PERCAPITA", "var_ingreso_estatal")])
print(correlacion)

# Instalar el paquete car si no lo tienes
install.packages("car")
library(car)

# Ajustar un modelo lineal para calcular el VIF
modelo <- lm(VABruto ~ POB_15_64 + RAZ_DEP_ADU + PEA + Ocupados + 
               Mahalanobis_Distance + ImssIssste + MONTO_PREDIAL + 
               MONTO_RECAUDADO_PERCAPITA + var_ingreso_estatal, data = datos)

# Calcular el VIF
vif(modelo)



# Supongamos que tu variable de interés es 'IngresoProm'
# y que ya tienes las variables de predicción listas

# Crear el conjunto de datos para el análisis de áreas pequeñas
# Puedes especificar tu variable de respuesta (como IngresoProm) y las variables predictoras
#"IngresoMed", "PIBmun", "Mahalanobis_Distance",  "POB_15_64", "POB_60_MAS", "POB_65_MAS",
#"RAZ_DEP_ADU", "PEA", "Ocupados", 
#"ImssIssste", "MONTO_PREDIAL", "MONTO_RECAUDADO_PERCAPITA"

# Ejemplo con un modelo simple 
datos_ap <- data.frame(
  IngresoProm = datos$IngresoProm,
  IngresoMed = datos$IngresoMed,
  Dist_Mahalanobis = datos$Mahalanobis_Distance,
  ImssIssste = datos$ImssIssste,
  VABruto = datos$PIBmun,
  MONTO_PREDIAL = datos$MONTO_PREDIAL,
  Poblac=datos$POB_15_64,
  PEA=datos$PEA,
  Entidad=datos$Entidad
)

#####una propuesta de modelo para cálculo de áreas pequeñas:
# Estimación de áreas pequeñas usando el paquete emdi

library(emdi)
# Puede que sea necesario transformar algunas variables en tipo factor
datos_ap <- datos %>%
  select(Ano, IngresoProm, Mahalanobis_Distance, ImssIssste, PIBmun, MONTO_PREDIAL, PEA, CVE_MUN, Entidad)

modelo_ebp <- ebp(
  fixed = IngresoProm ~ .,     # Modelo de regresión: IngresoProm como dependiente
  pop_data = datos_ap,         # Datos poblacionales
  pop_domains = "CVE_MUN",     # Variable de dominio (municipios)
  smp_data = datos_muestra,    # Datos muestrales
  smp_domains = "CVE_MUN"     # Variable de dominio en la muestra
#  method = "regression"                # Método de estimación
)

# Ver los resultados del modelo
summary(model)
  


#si lo separamos por año:

pop_data <- datos_ap  # Utilizamos todo el conjunto de datos (sin filtrar por muestra)
smp_data <- pop_data
# Crear una nueva columna de dominio compuesto: 'Entidad_Ano'
datos_ap$Entidad_Ano <- paste(datos_ap$Entidad, datos_ap$Ano, sep = "_")
# Verificar si la nueva columna fue creada correctamente
head(datos_ap$Entidad_Ano)


# Asignamos la nueva columna 'Entidad_Ano' como dominio
pop_data$Entidad_Ano <- paste(pop_data$Entidad, pop_data$Ano, sep = "_")
smp_data$Entidad_Ano <- paste(smp_data$Entidad, smp_data$Ano, sep = "_")
colnames(smp_data)


# Ahora definimos los dominios para la población y muestra
model <- emdi::ebp(
  fixed = IngresoProm ~ IngresoMed + Dist_Mahalanobis + ImssIssste + VABruto + MONTO_PREDIAL 
  , 
  pop_data = pop_data,  # Todos los municipios de todos los años para la población
  smp_data = smp_data,  # Usamos los mismos datos para la muestra
  pop_domains = "Entidad_Ano",  # Usamos la nueva columna de dominio
  smp_domains = "Entidad_Ano",  # Usamos la misma columna de dominio para la muestra
  L = 20,  # Número de repeticiones para el método
  MSE = FALSE,  # No calcular el error cuadrático medio
  B = 20,  # Número de simulaciones
  na.rm = TRUE,  # Eliminar valores faltantes
  transformation = "log"
  )


# Ver resultados del modelo
summary(model)
View(model)

# Obtener las estimaciones de ingreso promedio
predicciones <- model$ind


###son resultados estatales
# Guardar los resultados en un archivo CSV
write.csv(predicciones, "predicciones_ingreso_prom.csv", row.names = FALSE)







library(ggplot2)

# Convertir las predicciones a un data.frame si no lo es ya
predicciones_df <- as.data.frame(predicciones)

# Crear un gráfico de las predicciones con los nombres correctos de las columnas
ggplot(predicciones_df, aes(x = paste(Entidad, Ano, sep = "-"), y = IngresoProm)) + 
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Ingreso Promedio Estimado por Municipio y Año",
    x = "Municipio-Año",
    y = "Ingreso Promedio"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



###########
######Ahora, se obtiene también la estimación de áreas pequeñas para la productividad
library(dbplyr)

datos_apP <- datos %>%
  select(Ano, IngresoProm, Productividad, Mahalanobis_Distance, ImssIssste, PIBmun, MONTO_PREDIAL, PEA, CVE_MUN, Entidad)


pop_dataP <- datos_apP  # Utilizamos todo el conjunto de datos (sin filtrar por muestra)
smp_dataP <- pop_dataP
# Crear una nueva columna de dominio compuesto: 'Entidad_Ano'
datos_apP$Entidad_Ano <- paste(datos_apP$Entidad, datos_apP$Ano, sep = "_")
# Verificar si la nueva columna fue creada correctamente
head(datos_apP$Entidad_Ano)


# Asignamos la nueva columna 'Entidad_Ano' como dominio
pop_dataP$Entidad_Ano <- paste(pop_dataP$Entidad, pop_dataP$Ano, sep = "_")
smp_dataP$Entidad_Ano <- paste(smp_dataP$Entidad, smp_dataP$Ano, sep = "_")

#### Ahora definimos los dominios para la población y muestra
modelP <- emdi::ebp(
  fixed = Productividad ~ IngresoProm + Mahalanobis_Distance + ImssIssste + PIBmun + MONTO_PREDIAL + PEA, 
  pop_data = pop_dataP,  # Todos los municipios de todos los años para la población
  smp_data = smp_dataP,  # Usamos los mismos datos para la muestra
  pop_domains = "Entidad_Ano",  # Usamos la nueva columna de dominio
  smp_domains = "Entidad_Ano",  # Usamos la misma columna de dominio para la muestra
  L = 50,  # Número de repeticiones para el método
  MSE = FALSE,  # No calcular el error cuadrático medio
  B = 50,  # Número de simulaciones
  na.rm = TRUE,  # Eliminar valores faltantes
  transformation = "no" #o se puede usar transformación box.cox, o se usa por default transofmraicon logaritmica
  )


# Ver resultados del modelo
summary(modelP)
View(modelP)

dim(prediccionesP)  # Verifica el tamaño del dataframe
head(prediccionesP) # Muestra las primeras filas


# Obtener las estimaciones de ingreso promedio
prediccionesP <- modelP[["ind"]]
View(prediccionesP)

# Guardar los resultados en un archivo CSV
write.csv(prediccionesP, file = "predicciones_produc_prom.csv", row.names = FALSE)




#buscamos un mejor modelo de productividad, algo que no logramos =(
modelPR <- emdi::ebp(
  fixed = Productividad ~  IngresoProm, 
  pop_data = pop_dataP,  # Todos los municipios de todos los años para la población
  smp_data = smp_dataP,  # Usamos los mismos datos para la muestra
  pop_domains = "Entidad_Ano",  # Usamos la nueva columna de dominio
  smp_domains = "Entidad_Ano",  # Usamos la misma columna de dominio para la muestra
  L = 50,  # Número de repeticiones para el método
  MSE = FALSE,  # No calcular el error cuadrático medio
  B = 50,  # Número de simulaciones
  na.rm = TRUE  # Eliminar valores faltantes
)


# Ver resultados del modelo
summary(modelPR)







####un último código.
####Verificar dominios en ambos conjuntos de datos

unique(smp_data$Entidad)  # Dominios en los datos de la muestra
unique(pop_data$Entidad)  # Dominios en los datos de la población

# Filtrar la muestra para asegurarse de que solo contiene los dominios que están en la población
smp_data <- smp_data[smp_data$Entidad %in% unique(pop_data$Entidad), ]

# Ahora se puede ejecutar el modelo de áreas pequeñas
model <- emdi::ebp(
  fixed = IngresoProm ~ Mahalanobis_Distance + ImssIssste + PIBmun + MONTO_PREDIAL + Productividad,  # Fórmula de regresión
  pop_data = pop_data,  # Usamos todos los municipios de todos los años para la población
  smp_data = smp_data,  # Usamos los mismos datos para la muestra#
  pop_domains = "Entidad_Ano",  # Establecemos los dominios como entidades (estados)
  smp_domains = "Entidad_Ano",  # Establecemos los dominios como entidades (estados) para la muestra
  L = 20,  # Número de repeticiones para el método
  threshold = NULL,  # Umbral de estimación (si aplica)
  MSE = FALSE,  # No calcular el error cuadrático medio
  B = 20,  # Número de simulaciones
  seed = 123,  # Semilla para la aleatoriedad
  na.rm = TRUE,  # Eliminar valores faltantes
  transformation = "log"
  )

# Ver los resultados del modelo
summary(model)









###########
######Ahora, ingreso laboral promedio HOMBRES

datos_H <- datos_finales %>%
  select(Ano, IngresoPromH, Mahalanobis_Distance, ImssIssste, VABruto, MONTO_PREDIAL, PEA, CVE_MUN, Entidad)


pop_dataH <- datos_H  # Utilizamos todo el conjunto de datos (sin filtrar por muestra)
smp_dataH <- pop_dataH
# Crear una nueva columna de dominio compuesto: 'Entidad_Ano'
datos_H$Entidad_Ano <- paste(datos_H$Entidad, datos_H$Ano, sep = "_")
# Verificar si la nueva columna fue creada correctamente
head(datos_H$Entidad_Ano)


# Asignamos la nueva columna 'Entidad_Ano' como dominio
pop_dataH$Entidad_Ano <- paste(pop_dataH$Entidad, pop_dataH$Ano, sep = "_")
smp_dataH$Entidad_Ano <- paste(smp_dataH$Entidad, smp_dataH$Ano, sep = "_")

# Ahora definimos los dominios para la población y muestra
modelH <- emdi::ebp(
  fixed =  IngresoPromH  ~ Mahalanobis_Distance + ImssIssste + VABruto + MONTO_PREDIAL + PEA, 
  pop_data = pop_dataH,  # Todos los municipios de todos los años para la población
  smp_data = smp_dataH,  # Usamos los mismos datos para la muestra
  pop_domains = "Entidad_Ano",  # Usamos la nueva columna de dominio
  smp_domains = "Entidad_Ano",  # Usamos la misma columna de dominio para la muestra
  L = 50,  # Número de repeticiones para el método
  MSE = FALSE,  # No calcular el error cuadrático medio
  B = 50,  # Número de simulaciones
  na.rm = TRUE  # Eliminar valores faltantes
)


# Ver resultados del modelo
summary(modelH)
View(modelH)


# Obtener las estimaciones de ingreso promedio
prediccionesH <- modelH[["ind"]]
View(prediccionesH)

# Guardar los resultados en un archivo CSV
write.csv(prediccionesH, file = "predicciones_hom.csv", row.names = FALSE)





###########
######Ahora, ingreso laboral promedio MUJERES


datos_M <- datos_finales %>%
  select(Ano, IngresoPromM, Mahalanobis_Distance, ImssIssste, VABruto, MONTO_PREDIAL, PEA, CVE_MUN, Entidad)


pop_dataM <- datos_M  # Utilizamos todo el conjunto de datos (sin filtrar por muestra)
smp_dataM <- pop_dataM
# Crear una nueva columna de dominio compuesto: 'Entidad_Ano'
datos_M$Entidad_Ano <- paste(datos_M$Entidad, datos_M$Ano, sep = "_")
# Verificar si la nueva columna fue creada correctamente
head(datos_M$Entidad_Ano)


# Asignamos la nueva columna 'Entidad_Ano' como dominio
pop_dataM$Entidad_Ano <- paste(pop_dataM$Entidad, pop_dataM$Ano, sep = "_")
smp_dataM$Entidad_Ano <- paste(smp_dataM$Entidad, smp_dataM$Ano, sep = "_")

# Ahora definimos los dominios para la población y muestra
modelM <- emdi::ebp(
  fixed = IngresoPromM  ~ Mahalanobis_Distance + ImssIssste + VABruto + MONTO_PREDIAL + PEA, 
  pop_data = pop_dataM,  # Todos los municipios de todos los años para la población
  smp_data = smp_dataM,  # Usamos los mismos datos para la muestra
  pop_domains = "Entidad_Ano",  # Usamos la nueva columna de dominio
  smp_domains = "Entidad_Ano",  # Usamos la misma columna de dominio para la muestra
  L = 50,  # Número de repeticiones para el método
  MSE = FALSE,  # No calcular el error cuadrático medio
  B = 50,  # Número de simulaciones
  na.rm = TRUE  # Eliminar valores faltantes
)


# Ver resultados del modelo
summary(modelM)
View(modelM)


# Obtener las estimaciones de ingreso promedio
prediccionesM <- modelM[["ind"]]
View(prediccionesM)

# Guardar los resultados en un archivo CSV
write.csv(prediccionesM, file = "predicciones_mujer.csv", row.names = FALSE)










#ultima ocpion para obtener ingresos municipales (si los saca a nivel municipal)
# Cargar las librerías necesarias
library(readxl)
library(dplyr)
library(emdi)

# Paso 1: Cargar el archivo Excel
datos <- read.xlsx("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/DatosOrigenAreasPequenas1.xlsx",1)
#datos <- na.omit(datos[, c("CVE_ENT", "CVE_MUN", "Ano")])

# Paso 2: Seleccionar las columnas relevantes
datos_ap <- datos %>%
  select(Ano, IngresoProm, Mahalanobis_Distance, ImssIssste, PIBmun, MONTO_PREDIAL, PEA, CVE_MUN, Entidad, Municipio, CVE_ENT)
# Crear una tabla con nombres de estado y municipio desde datos_ap
info_estados_municipios <- datos_ap %>%
  select(Ano, Entidad, Municipio, CVE_ENT, CVE_MUN) %>%
  distinct()


# Paso 3: Separar la columna `LLAVE` en estado y municipio
datos <- datos %>%
  mutate(
    Estado = substr(LLAVE, 1, 2),       # Extraer los dos primeros dígitos (estado)
    Municipio = substr(LLAVE, 3, 5)    # Extraer los últimos tres dígitos (municipio)
  )

# Paso 4: Seleccionar y limpiar las columnas relevantes
datos_ap <- datos %>%
  select(Ano, IngresoProm, Mahalanobis_Distance, ImssIssste, PIBmun, MONTO_PREDIAL, PEA, CVE_MUN, Entidad, Municipio, CVE_ENT)

# Convertir textos en la columna MONTO_PREDIAL a NA
datos_ap$MONTO_PREDIAL <- as.character(datos_ap$MONTO_PREDIAL)
datos_ap$MONTO_PREDIAL <- ifelse(
  grepl("[a-zA-Z]", datos_ap$MONTO_PREDIAL),
  NA,
  as.numeric(datos_ap$MONTO_PREDIAL)
)

# Eliminar valores faltantes
datos_ap <- na.omit(datos_ap)

# Crear dominios por Estado, Municipio y Año
datos_ap$Entidad_Ano <- paste(datos_ap$Estado, datos_ap$Municipio, datos_ap$Ano, sep = "_")


# Crear una columna Dominio_Municipal si no existe
datos$Dominio_Municipal <- paste(datos$CVE_ENT, datos$CVE_MUN, datos$Ano, sep = "_")


# Paso 5: Dividir en datos poblacionales y muestrales
pop_data <- datos_ap  # Usar todos los datos disponibles como población
smp_data <- pop_data  # También los usamos como muestra en este caso

# Paso 6: Modelar utilizando el método EBP
modelo_ebp <- emdi::ebp(
  fixed = IngresoProm ~ Mahalanobis_Distance + ImssIssste + PIBmun + MONTO_PREDIAL + PEA,
  pop_data = pop_data,
  smp_data = smp_data,
  pop_domains = "Entidad_Ano",
  smp_domains = "Entidad_Ano",
  L = 20,        # Número de repeticiones para bootstrap
  MSE = TRUE,   # No calcular el error cuadrático medio
  B = 20,        # Número de simulaciones
  na.rm = TRUE,  # Eliminar valores faltantes
  transformation = "log"
)

# Fusionar predicciones con la información de estado y municipio
predicciones_completas <- merge(
  predicciones,
  info_estados_municipios,
  by.x = "Domain",     # Columna en predicciones
  by.y = "Entidad_Ano", # Columna en info_estados_municipios
  all.x = TRUE
)
head(predicciones_completas)
summary(predicciones_completas)

# Paso 7: Obtener predicciones
predicciones <- modelo_ebp$ind

# Ver resultados
print(summary(modelo_ebp))
View(predicciones)

# Paso 8: Guardar las predicciones en un archivo CSV
write.csv(predicciones_completas, "predicciones_completas_con_nombres.csv", row.names = FALSE)
cat("El archivo 'predicciones_completas_con_nombres.csv' ha sido creado con éxito.\n")

residuals(modelo_ebp)
fitted(modelo_ebp)
modelo_ebp$ind
modelo_ebp$MSE
qqnorm(residuals(modelo_ebp), main = "QQ Plot - Ingreso")
qqline(residuals(modelo_ebp), col = "red")
hist(residuals(modelo_ebp), main = "Residuos Ingreso", col = "lightblue")



#######ESTE ES EL BUENO =)
#código completo para obtener el ingreso promedio por municipio
##el de la tesis
# 📦 Cargar librerías necesarias
library(readxl)
library(dplyr)
library(stringr)
library(emdi)


# 📂 1. Leer archivo original
datos <- read.csv("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/DatosOrigenAreasPequenas1.csv",
                  fileEncoding = "latin1")

# 🛠 2. Limpiar claves y construir dominio municipal
datos <- datos %>%
  mutate(
    CVE_ENT = str_pad(as.character(CVE_ENT), 2, pad = "0"),
    CVE_MUN = str_pad(as.character(CVE_MUN), 3, pad = "0"),
    Dominio_Municipal = paste(CVE_ENT, CVE_MUN, Ano, sep = "_")
  )

# 🔍 3. Filtrar y limpiar variables numéricas
variables_modelo <- c("IngresoProm", "Mahalanobis_Distance", "ImssIssste", "PIBmun", "MONTO_PREDIAL", "PEA")
datos <- datos %>%
  mutate(across(all_of(variables_modelo), ~ suppressWarnings(as.numeric(gsub(",", "", .)))))

# 🧹 4. Eliminar filas con NA en las variables del modelo
pop_data <- datos %>%
  select(Dominio_Municipal, all_of(variables_modelo), Ano, CVE_ENT, CVE_MUN, Entidad, Municipio) %>%
  na.omit()

smp_data <- pop_data  # En este caso usamos el mismo conjunto como muestra

# ⚙️ 5. Ejecutar modelo EBP
modelo_ebp <- emdi::ebp(
  fixed = IngresoProm ~ Mahalanobis_Distance + ImssIssste + PIBmun + MONTO_PREDIAL + PEA,
  pop_data = pop_data,
  smp_data = smp_data,
  pop_domains = "Dominio_Municipal",
  smp_domains = "Dominio_Municipal",
  L = 20,
  B = 20,
  MSE = TRUE,
  na.rm = TRUE,
  transformation = "box.cox"
)

summary(modelo_ebp)
install.packages("nortest")
library (nortest)
library(ggplot2)
ad.test(modelo_ebp$ind$Mean)
ggplot(predicciones, aes(x = Mean)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Histograma de Ingreso Estimado", x = "Ingreso Promedio", y = "Frecuencia")
qqnorm(predicciones$Mean,
       main = "QQ Plot de Ingreso Estimado")
qqline(predicciones$Mean, col = "red")


residuos_ingreso <- residuals(modelo_ebp)
View(residuos_ingreso)
# Histograma
hist(residuos_ingreso, breaks = 30, col = "darkorange",
     main = "Histograma de Residuos - Ingreso", xlab = "Residuos")

# QQ plot
qqnorm(residuos_ingreso, main = "QQ Plot de Residuos - Ingreso")
qqline(residuos_ingreso, col = "red")

#MSE- errores de las predicciones
modelo_ebp$MSE
modelo_ebp$ind$SE <- sqrt(modelo$ind$MSE)



# 📈 6. Obtener predicciones
predicciones <- modelo_ebp$ind


# 🧩 7. Separar el dominio en claves y año
predicciones <- predicciones %>%
  mutate(
    CVE_ENT = str_sub(Domain, 1, 2),
    CVE_MUN = str_sub(Domain, 4, 6),
    Ano = str_sub(Domain, 8, 11)
  )

# 🧾 8. Unir con nombres de entidad y municipio
nombres <- datos %>%
  select(CVE_ENT, CVE_MUN, Entidad, Municipio) %>%
  distinct()

predicciones_final <- predicciones %>%
  left_join(nombres, by = c("CVE_ENT", "CVE_MUN")) %>%
  select(CVE_ENT, Entidad, CVE_MUN, Municipio, Ano, Ingreso_Estimado = Mean)

# 💾 9. Guardar en CSV
write.csv(predicciones_final, "ingreso_promedio_municipal_con_nombres.csv", row.names = FALSE)

cat("\n✅ Archivo guardado: ingreso_promedio_municipal_con_nombres.csv\n")




##estimación de áreas pequenas para productividad
# 📚 Cargar librerías necesarias
library(emdi)
library(dplyr)
library(readxl)
library(stringr)

# 📁 Cargar los datos desde Excel
datos <- read.xlsx("/Users/dmares/Documents/Doctorado unam/Seminario 7mo sem/DatosOrigenAreasPequenas1.xlsx", 1)

# 🧹 Limpiar y preparar los datos
datos <- datos %>%
  mutate(
    CVE_ENT = str_pad(as.character(CVE_ENT), 2, pad = "0"),
    CVE_MUN = str_pad(as.character(CVE_MUN), 3, pad = "0"),
    Dominio_Municipal = paste(CVE_ENT, CVE_MUN, Ano, sep = "_")
  )

# 🧪 Asegúrate que MONTO_PREDIAL sea numérico
datos$MONTO_PREDIAL <- as.character(datos$MONTO_PREDIAL)
datos$MONTO_PREDIAL <- ifelse(grepl("[a-zA-Z]", datos$MONTO_PREDIAL), NA, as.numeric(datos$MONTO_PREDIAL))

# 🧾 Variables necesarias
variables_modelo <- c("Productividad", "IngresoProm", "Mahalanobis_Distance", "PIBmun", "MONTO_PREDIAL")
datos <- datos %>%
  mutate(across(all_of(variables_modelo), ~as.numeric(gsub(",", "", .))))

# 🧽 Eliminar NA
datos <- na.omit(datos[, c("Dominio_Municipal", variables_modelo, "Ano", "CVE_ENT", "Entidad", "CVE_MUN", "Municipio")])

# 👥 Dividir en población y muestra
pop_data <- datos
smp_data <- datos

# ⚙️ Modelo EBP
modelo_productividad <- emdi::ebp(
  fixed = Productividad ~ IngresoProm + Mahalanobis_Distance + PIBmun + MONTO_PREDIAL,
  pop_data = pop_data,
  smp_data = smp_data,
  pop_domains = "Dominio_Municipal",
  smp_domains = "Dominio_Municipal",
  L = 10,
  B = 10,
  MSE = TRUE,
  na.rm = TRUE,
  transformation = "log"
)

summary(modelo_productividad)
install.packages("nortest")
library (nortest)
library(ggplot2)
ad.test(modelo_productividad$ind$Mean)

ggplot(predicciones, aes(x = Mean)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Histograma de Productividad Estimada", x = "Productividad Promedio", y = "Frecuencia")
qqnorm(predicciones$Mean,
       main = "QQ Plot de Productividad Estimada")
qqline(predicciones$Mean, col = "red")


residuos_produc <- residuals(modelo_productividad)
# Histograma
hist(residuos_produc, breaks = 30, col = "darkorange",
     main = "Histograma de Residuos - Productividad", xlab = "Residuos")

# QQ plot
qqnorm(residuos_produc, main = "QQ Plot de Residuos - Productividad")
qqline(residuos_produc, col = "red")



hist(predicciones$MSE, breaks = 30, main = "Errores Estándar (MSE) de las Estimaciones",
     xlab = "MSE", col = "skyblue", border = "white")





info_claves_nombres <- datos %>%
  distinct(Dominio_Municipal, CVE_ENT, Entidad, CVE_MUN, Municipio)

# 📊 Obtener predicciones
predicciones <- modelo_productividad$ind
predicciones$Dominio_Municipal <- rownames(predicciones)

# 🔗 Unir con nombres de entidad y municipio
info_nombres <- datos %>%
  select(Dominio_Municipal, CVE_ENT, Entidad, CVE_MUN, Municipio) %>%
  distinct()

predicciones_final <- merge(predicciones, info_nombres, by = "Dominio_Municipal", all.x = TRUE)

# 🎯 Seleccionar columnas finales
predicciones_final <- predicciones_final %>%
  select(CVE_ENT, Entidad, CVE_MUN, Municipio, Year = Domain, Productividad_Estimada = Mean)

# 💾 Guardar archivo
write.csv(predicciones_final, "estimaciones_productividad_municipal.csv", row.names = FALSE)
cat("✅ Archivo 'estimaciones_productividad_municipal.csv' creado con éxito.\n")



#análisis estadístico para resultados de ingreso promedio laboral



