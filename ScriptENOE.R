########
##Base de datos ENOE
#########

# SCRIPT COMPLETO: INGRESO TOTAL REAL POR ESTADO 2014-2023
# Incluye: ingresos laborales + transferencias (jubilaciones, rentas, intereses, propinas)
# Metodología: Individuo → Hogar → Municipio → Estado (ponderado)
# CON JOIN COE2 + VIV (IDENTIFICACIÓN CORRECTA DE HOGARES)


cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("PROCESAMIENTO COMPLETO ENOE 2014-2023\n")
cat("CON FILTROS DE OUTLIERS - INGRESO TOTAL REAL (pesos 2018)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

# ============================================
# 1. CONFIGURACIÓN INICIAL
# ============================================

# Instalar/cargar paquetes
paquetes <- c("tidyverse", "survey", "readr", "janitor", "here")
for(p in paquetes){
  if(!require(p, character.only = TRUE)){
    install.packages(p)
    library(p, character.only = TRUE)
  }
}

# Ruta de tus archivos (¡AJUSTA ESTA RUTA!)
ruta_base <- "/Users/dmares/Downloads/DatosOriginalesENOE"

# ============================================
# 2. FUNCIONES AUXILIARES
# ============================================

#' Extraer año y trimestre del nombre del archivo
extraer_info_archivo <- function(nombre_archivo) {
  año <- as.numeric(str_extract(nombre_archivo, "(?<=enoe_)(20\\d{2})"))
  trimestre <- as.numeric(str_extract(nombre_archivo, "(?<=_)(\\d)(?=t\\.csv)"))
  return(list(año = año, trimestre = trimestre))
}

#' Cargar archivo CSV de ENOE de manera robusta
cargar_enoe <- function(ruta) {
  cat("  📂 Cargando:", basename(ruta), "\n")
  tryCatch({
    df <- read_csv(ruta, locale = locale(encoding = "ISO-8859-1"),
                   guess_max = 10000, show_col_types = FALSE)
    names(df) <- tolower(names(df))
    names(df) <- gsub("[^a-z0-9]", "", names(df))
    cat("  ✅ Cargado:", nrow(df), "filas\n")
    return(df)
  }, error = function(e) {
    cat("  ❌ Error:", e$message, "\n")
    return(NULL)
  })
}

#' Calcular mediana ponderada
mediana_ponderada <- function(x, w) {
  if(length(x) == 0 || all(is.na(x))) return(NA)
  orden <- order(x)
  x_ord <- x[orden]
  w_ord <- w[orden]
  cum_w <- cumsum(w_ord) / sum(w_ord)
  idx <- which(cum_w >= 0.5)[1]
  if(is.na(idx)) return(NA)
  return(x_ord[idx])
}

#' Calcular Gini ponderado CON FILTRO DE OUTLIERS
gini_ponderado_corregido <- function(x, w, truncar = TRUE) {
  if(length(x) < 10) return(NA)
  
  validos <- !is.na(x) & !is.na(w) & w > 0 & x > 0
  x <- x[validos]
  w <- w[validos]
  
  if(length(x) < 10) return(NA)
  
  # Truncar outliers si se solicita
  if(truncar) {
    p99 <- quantile(x, 0.99, na.rm = TRUE)
    x <- pmin(x, p99)
  }
  
  orden <- order(x)
  x_ord <- x[orden]
  w_ord <- w[orden]
  
  total_w <- sum(w_ord)
  total_xw <- sum(x_ord * w_ord)
  if(total_xw == 0) return(NA)
  
  cum_w <- cumsum(w_ord)
  numerador <- sum((2 * cum_w - w_ord) * x_ord * w_ord)
  denominador <- total_w * total_xw
  
  gini <- (numerador / denominador) - 1
  return(max(0, min(1, gini)))
}

# ============================================
# 3. INPC (Índice Nacional de Precios al Consumidor)
# ============================================

inpc <- data.frame(
  año = 2014:2023,
  inpc = c(85.333, 87.6545, 90.12791667, 95.573,
           100.2553333, 103.9006667, 107.43,
           113.5419167, 122.5075, 129.2796667)
) %>%
  mutate(factor_deflactor = 100 / inpc)

# ============================================
# 4. LÍMITES DE INGRESO RAZONABLES POR AÑO (en pesos corrientes)
# ============================================

limites_ingreso <- data.frame(
  año = 2014:2023,
  min_razonable = c(800, 850, 900, 950, 1000, 1050, 1100, 1150, 1200, 1300),
  max_razonable = c(150000, 155000, 160000, 165000, 170000, 175000, 180000, 185000, 190000, 195000)
)

# ============================================
# 5. LISTAR ARCHIVOS DISPONIBLES
# ============================================

cat("\n📂 Buscando archivos COE2 y VIV...\n")

# Archivos COE2
archivos_coe2 <- list.files(ruta_base, pattern = "coe2.*\\.csv$", 
                            full.names = TRUE, ignore.case = TRUE)
if(length(archivos_coe2) == 0) {
  archivos_coe2 <- list.files(pattern = "coe2.*\\.csv$", 
                              full.names = TRUE, ignore.case = TRUE)
}

# Archivos VIV
archivos_viv <- list.files(ruta_base, pattern = "viv.*\\.csv$", 
                           full.names = TRUE, ignore.case = TRUE)
if(length(archivos_viv) == 0) {
  archivos_viv <- list.files(pattern = "viv.*\\.csv$", 
                             full.names = TRUE, ignore.case = TRUE)
}

# Crear metadatos COE2
archivos_coe2_info <- map_dfr(archivos_coe2, function(ruta) {
  info <- extraer_info_archivo(basename(ruta))
  tibble(ruta = ruta, año = info$año, trimestre = info$trimestre)
}) %>% filter(!is.na(año), año >= 2014, año <= 2023)

# Crear metadatos VIV
archivos_viv_info <- map_dfr(archivos_viv, function(ruta) {
  info <- extraer_info_archivo(basename(ruta))
  tibble(ruta = ruta, año = info$año, trimestre = info$trimestre)
}) %>% filter(!is.na(año), año >= 2014, año <= 2023)

cat("\n📊 Archivos COE2 encontrados por año:\n")
print(archivos_coe2_info %>% count(año) %>% rename(coe2 = n))

cat("\n📊 Archivos VIV encontrados por año:\n")
print(archivos_viv_info %>% count(año) %>% rename(viv = n))

# ============================================
# 6. DICCIONARIO DE ESTADOS
# ============================================

diccionario_estados <- tibble(
  ent = 1:32,
  estado = c("Aguascalientes", "Baja California", "Baja California Sur", 
             "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
             "CDMX", "Durango", "Guanajuato", "Guerrero",
             "Hidalgo", "Jalisco", "Estado de México", "Michoacán", "Morelos",
             "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
             "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
             "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán",
             "Zacatecas"),
  region = c("Centro Norte", "Norte", "Norte", "Sur", "Norte", "Centro Occidente", 
             "Sur", "Norte", "Centro", "Norte", "Centro Occidente", "Sur",
             "Centro", "Centro Occidente", "Centro", "Centro Occidente", "Centro",
             "Centro Occidente", "Norte", "Sur", "Centro", "Centro",
             "Sur", "Centro", "Norte", "Norte", "Sur", "Norte", "Centro", "Sur",
             "Sur", "Centro Norte")
)

# ============================================
# 7. PROCESAR CADA AÑO CON JOIN COE2 + VIV
# ============================================

resultados_todos <- list()

for(año_actual in 2014:2023) {
  
  cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
  cat("PROCESANDO AÑO:", año_actual, "\n")
  cat(paste0(rep("=", 60), collapse = ""), "\n")
  
  # 7.1 Encontrar archivos del año (primer trimestre)
  archivo_coe2 <- archivos_coe2_info %>%
    filter(año == año_actual, trimestre == 1) %>%
    slice(1) %>% pull(ruta)
  
  archivo_viv <- archivos_viv_info %>%
    filter(año == año_actual, trimestre == 1) %>%
    slice(1) %>% pull(ruta)
  
  if(length(archivo_coe2) == 0 | length(archivo_viv) == 0) {
    cat("❌ Faltan archivos para", año_actual, "\n")
    next
  }
  
  # 7.2 Cargar datos
  coe2 <- cargar_enoe(archivo_coe2)
  viv <- cargar_enoe(archivo_viv)
  
  if(is.null(coe2) | is.null(viv)) next
  
  # 7.3 Variables para join
  vars_join <- c("cda", "ent", "con", "vsel", "nhog", "hmud")
  vars_join <- vars_join[vars_join %in% names(coe2) & vars_join %in% names(viv)]
  
  if(length(vars_join) < 3) {
    cat("❌ No hay suficientes variables para join. Buscando alternativas...\n")
    vars_comunes <- intersect(names(coe2), names(viv))
    vars_id <- vars_comunes[grepl("cd|ent|con|v_sel|n_hog|h_mud", vars_comunes)]
    if(length(vars_id) >= 3) {
      vars_join <- vars_id
      cat("✅ Usando variables alternativas:", paste(vars_join, collapse = ", "), "\n")
    } else {
      cat("❌ No se puede hacer join. Saltando año", año_actual, "\n")
      next
    }
  }
  
  # 7.4 Eliminar duplicados en VIV (un registro por hogar)
  viv_unico <- viv %>%
    distinct(across(all_of(vars_join)), .keep_all = TRUE)
  
  cat("📊 VIV después de eliminar duplicados:", nrow(viv_unico), "hogares únicos\n")
  
  # 7.5 Hacer join
  base_unida <- coe2 %>%
    inner_join(viv_unico, by = vars_join, suffix = c("", "_viv"))
  
  cat("✅ Join completado:", nrow(base_unida), "individuos en hogares identificados\n")
  
  if(nrow(base_unida) == 0) {
    cat("❌ No hay individuos después del join\n")
    next
  }
  
  # ============================================
  # 8. CONSTRUIR INGRESO TOTAL
  # ============================================
  
  # 8.1 Filtrar casos válidos (trabajo principal)
  base_ingreso <- base_unida %>%
    filter(!is.na(p6b1), !is.na(p6b2), p6b1 %in% 1:7, p6b2 > 0, p6b2 < 99999) %>%
    mutate(
      ingreso_principal = case_when(
        p6b1 == 1 ~ p6b2 * 30,
        p6b1 == 2 ~ p6b2 * 4.33,
        p6b1 == 3 ~ p6b2 * 2.1667,
        p6b1 == 4 ~ p6b2 * 2,
        p6b1 == 5 ~ p6b2,
        p6b1 == 6 ~ p6b2 / 12,
        p6b1 == 7 ~ p6b2,
        TRUE ~ NA_real_
      )
    )
  
  # 8.2 Otros trabajos (p9l1)
  if("p9l1" %in% names(base_ingreso)) {
    base_ingreso <- base_ingreso %>%
      mutate(ingreso_otros = ifelse(!is.na(p9l1) & p9l1 > 0 & p9l1 < 99999, p9l1, 0))
  } else {
    base_ingreso$ingreso_otros <- 0
    cat("   ℹ️ p9l1 no disponible, usando 0\n")
  }
  
  # 8.3 Jubilaciones (p10_1)
  if("p10_1" %in% names(base_ingreso)) {
    base_ingreso <- base_ingreso %>%
      mutate(ingreso_jubilacion = ifelse(!is.na(p10_1) & p10_1 > 0 & p10_1 < 99999, p10_1, 0))
  } else {
    base_ingreso$ingreso_jubilacion <- 0
  }
  
  # 8.4 Rentas (p10_2)
  if("p10_2" %in% names(base_ingreso)) {
    base_ingreso <- base_ingreso %>%
      mutate(ingreso_rentas = ifelse(!is.na(p10_2) & p10_2 > 0 & p10_2 < 99999, p10_2, 0))
  } else {
    base_ingreso$ingreso_rentas <- 0
  }
  
  # 8.5 Intereses (p10_3)
  if("p10_3" %in% names(base_ingreso)) {
    base_ingreso <- base_ingreso %>%
      mutate(ingreso_intereses = ifelse(!is.na(p10_3) & p10_3 > 0 & p10_3 < 99999, p10_3, 0))
  } else {
    base_ingreso$ingreso_intereses <- 0
  }
  
  # 8.6 Otros no laborales (p10_4)
  if("p10_4" %in% names(base_ingreso)) {
    base_ingreso <- base_ingreso %>%
      mutate(ingreso_otros_no_laborales = ifelse(!is.na(p10_4) & p10_4 > 0 & p10_4 < 99999, p10_4, 0))
  } else {
    base_ingreso$ingreso_otros_no_laborales <- 0
  }
  
  # 8.7 Calcular ingreso total
  base_ingreso <- base_ingreso %>%
    mutate(
      ingreso_total_mensual = ingreso_principal + ingreso_otros + 
        ingreso_jubilacion + ingreso_rentas +
        ingreso_intereses + ingreso_otros_no_laborales,
      
      id_hogar = paste(cda, ent, con, vsel, nhog, hmud, sep = "_")
    )
  
  # 8.8 VERIFICAR FACTORES DE EXPANSIÓN
  factores_disponibles <- grep("fac|FAC|factor|peso", names(base_ingreso), value = TRUE, ignore.case = TRUE)
  cat("🔍 Factores de expansión disponibles:", paste(factores_disponibles, collapse = ", "), "\n")
  
  # 8.9 ASIGNAR FACTOR DE EXPANSIÓN
  if("fac_tri" %in% names(base_ingreso)) {
    base_ingreso$fac <- base_ingreso$fac_tri
    cat("✅ Usando fac_tri como factor de expansión\n")
  } else if("fac" %in% names(base_ingreso)) {
    base_ingreso$fac <- base_ingreso$fac
    cat("✅ Usando fac como factor de expansión\n")
  } else if("factor" %in% names(base_ingreso)) {
    base_ingreso$fac <- base_ingreso$factor
    cat("✅ Usando factor como factor de expansión\n")
  } else if("fac_per" %in% names(base_ingreso)) {
    base_ingreso$fac <- base_ingreso$fac_per
    cat("✅ Usando fac_per como factor de expansión\n")
  } else {
    posibles_factores <- grep("fac", names(base_ingreso), value = TRUE, ignore.case = TRUE)
    if(length(posibles_factores) > 0) {
      base_ingreso$fac <- base_ingreso[[posibles_factores[1]]]
      cat("⚠️ Usando", posibles_factores[1], "como factor de expansión\n")
    } else {
      base_ingreso$fac <- 1
      cat("❌ NO SE ENCONTRÓ FACTOR. Usando 1 (SIN PONDERAR)\n")
    }
  }
  
  # 8.10 FILTRAR OUTLIERS (¡LA PARTE MÁS IMPORTANTE!)
  limite_año <- limites_ingreso %>% filter(año == año_actual)
  
  # Mostrar estadísticos antes del filtro
  cat("\n📊 Estadísticos ANTES del filtro de outliers:\n")
  print(summary(base_ingreso$ingreso_total_mensual))
  
  # Aplicar filtros
  base_ingreso <- base_ingreso %>%
    filter(
      ingreso_total_mensual >= limite_año$min_razonable,
      ingreso_total_mensual <= limite_año$max_razonable,
      !is.na(fac), fac > 0
    )
  
  cat("\n✅ Individuos después de filtro realista:", nrow(base_ingreso), "\n")
  cat("   Rango del factor:", min(base_ingreso$fac), "-", max(base_ingreso$fac), "\n")
  
  # ============================================
  # 9. A NIVEL HOGAR
  # ============================================
  
  hogares <- base_ingreso %>%
    group_by(id_hogar, ent) %>%
    summarise(
      fac_hogar = first(fac),
      ingreso_hogar = sum(ingreso_total_mensual, na.rm = TRUE),
      miembros_ocupados = n(),
      .groups = "drop"
    )
  
  cat("✅ Hogares únicos:", nrow(hogares), "\n")
  
  # ============================================
  # 10. A NIVEL ESTADO (sin municipio)
  # ============================================
  
  estados <- hogares %>%
    group_by(ent) %>%
    summarise(
      ingreso_estado = weighted.mean(ingreso_hogar, fac_hogar, na.rm = TRUE),
      poblacion_estado = sum(fac_hogar, na.rm = TRUE),
      hogares_muestra = n(),
      .groups = "drop"
    )
  
  # ============================================
  # 11. CALCULAR GINI POR ESTADO (USANDO INGRESO TRUNCADO)
  # ============================================
  
  # Truncar al percentil 99 para eliminar outliers extremos
  p99 <- quantile(base_ingreso$ingreso_total_mensual, 0.99, na.rm = TRUE)
  
  base_ingreso <- base_ingreso %>%
    mutate(ingreso_truncado = pmin(ingreso_total_mensual, p99))
  
  gini_estados <- base_ingreso %>%
    group_by(ent) %>%
    summarise(
      gini = gini_ponderado_corregido(ingreso_truncado, fac, truncar = FALSE),
      mediana = mediana_ponderada(ingreso_truncado, fac),
      n_obs = n(),
      .groups = "drop"
    )
  
  # También calcular Gini a nivel de HOGAR (más realista para comparar con INEGI)
  gini_hogares <- hogares %>%
    group_by(ent) %>%
    summarise(
      gini_hogar = gini_ponderado_corregido(ingreso_hogar, fac_hogar, truncar = TRUE),
      .groups = "drop"
    )
  
  # ============================================
  # 12. UNIR RESULTADOS
  # ============================================
  
  resultado_año <- estados %>%
    left_join(gini_estados, by = "ent") %>%
    left_join(gini_hogares, by = "ent") %>%
    left_join(diccionario_estados, by = "ent") %>%
    mutate(
      año = año_actual,
      ingreso_estado_mensual = ingreso_estado,
      ingreso_estado_anual = ingreso_estado * 12
    ) %>%
    select(año, ent, estado, region, 
           ingreso_estado_mensual, ingreso_estado_anual,
           mediana_estatal = mediana,
           gini_individuos = gini,
           gini_hogares = gini_hogar,
           poblacion_estado, hogares_muestra, n_obs)
  
  resultados_todos[[as.character(año_actual)]] <- resultado_año
  
  cat("✅ Año", año_actual, "procesado:", nrow(resultado_año), "estados\n")
  cat("   Hogares totales en muestra:", sum(resultado_año$hogares_muestra), "\n")
  cat("   Gini promedio (individuos):", mean(resultado_año$gini_individuos, na.rm = TRUE), "\n")
  cat("   Gini promedio (hogares):", mean(resultado_año$gini_hogares, na.rm = TRUE), "\n")
}

# ============================================
# 13. COMBINAR TODOS LOS AÑOS
# ============================================

cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("COMBINANDO RESULTADOS 2014-2023\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

if(length(resultados_todos) > 0) {
  resultados_completos <- bind_rows(resultados_todos) %>%
    arrange(año, ent)
  
  cat("✅ Total registros:", nrow(resultados_completos), "\n")
  cat("✅ Años procesados:", paste(unique(resultados_completos$año), collapse = ", "), "\n")
  
  # ============================================
  # 14. APLICAR INFLACIÓN (INGRESO REAL 2018)
  # ============================================
  
  resultados_reales <- resultados_completos %>%
    left_join(inpc, by = "año") %>%
    mutate(
      ingreso_mensual_real = ingreso_estado_mensual * factor_deflactor,
      ingreso_anual_real = ingreso_estado_anual * factor_deflactor
    ) %>%
    select(año, ent, estado, region,
           ingreso_mensual_nominal = ingreso_estado_mensual,
           ingreso_anual_nominal = ingreso_estado_anual,
           ingreso_mensual_real,
           ingreso_anual_real,
           mediana_estatal,
           gini_individuos,
           gini_hogares,
           poblacion_estado,
           hogares_muestra,
           n_obs,
           inpc, factor_deflactor)
  
  # ============================================
  # 15. GUARDAR RESULTADOS
  # ============================================
  
  # Archivo principal
  write_csv(resultados_reales, "ingresos_totales_reales_estados_2014_2023.csv")
  
  # Resumen nacional (usando GINI DE HOGARES para comparar con INEGI)
  resumen_nacional <- resultados_reales %>%
    group_by(año) %>%
    summarise(
      ingreso_nacional_prom = weighted.mean(ingreso_anual_real, poblacion_estado, na.rm = TRUE),
      gini_nacional_hogares = weighted.mean(gini_hogares, poblacion_estado, na.rm = TRUE),
      gini_nacional_individuos = weighted.mean(gini_individuos, poblacion_estado, na.rm = TRUE),
      poblacion_total = sum(poblacion_estado, na.rm = TRUE),
      hogares_totales = sum(hogares_muestra, na.rm = TRUE),
      .groups = "drop"
    )
  
  write_csv(resumen_nacional, "resumen_nacional_anual.csv")
  
  # ============================================
  # 16. MOSTRAR RESULTADOS
  # ============================================
  
  cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
  cat("RESULTADOS FINALES\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  
  cat("\n📊 PROMEDIO NACIONAL POR AÑO (ingreso anual real 2018):\n")
  print(resumen_nacional %>%
          mutate(ingreso = paste0("$", format(round(ingreso_nacional_prom), big.mark = ","))))
  
  cat("\n🏆 TOP 5 ESTADOS CON MAYOR INGRESO REAL (promedio 2014-2023):\n")
  resultados_reales %>%
    group_by(estado) %>%
    summarise(ingreso_prom = mean(ingreso_anual_real, na.rm = TRUE)) %>%
    arrange(desc(ingreso_prom)) %>%
    head(5) %>%
    mutate(ingreso = paste0("$", format(round(ingreso_prom), big.mark = ","))) %>%
    print()
  
  cat("\n📉 TOP 5 ESTADOS CON MENOR INGRESO REAL (promedio 2014-2023):\n")
  resultados_reales %>%
    group_by(estado) %>%
    summarise(ingreso_prom = mean(ingreso_anual_real, na.rm = TRUE)) %>%
    arrange(ingreso_prom) %>%
    head(5) %>%
    mutate(ingreso = paste0("$", format(round(ingreso_prom), big.mark = ","))) %>%
    print()
  
  cat("\n🔍 COEFICIENTE DE GINI NACIONAL (HOGARES - para comparar con INEGI):\n")
  print(resumen_nacional %>% select(año, gini_nacional_hogares))
  
  cat("\n🔍 COEFICIENTE DE GINI NACIONAL (INDIVIDUOS - referencia):\n")
  print(resumen_nacional %>% select(año, gini_nacional_individuos))
  
  cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
  cat("✅ PROCESO COMPLETADO EXITOSAMENTE\n")
  cat("📁 Archivos generados:\n")
  cat("   1. ingresos_totales_reales_estados_2014_2023.csv\n")
  cat("   2. resumen_nacional_anual.csv\n")
  cat(paste0(rep("=", 80), collapse = ""), "\n")
  
} else {
  cat("\n❌ No se procesó ningún año.\n")
}




# Cargar tu base
ingresos <- read_csv("ingresos_totales_reales_estados_2014_2023.csv")

# Ver que no haya saltos bruscos año con año (debería ser suave)
ingresos %>%
  group_by(estado) %>%
  arrange(año) %>%
  mutate(
    cambio_porcentual = (ingreso_anual_real / lag(ingreso_anual_real) - 1) * 100
  ) %>%
  filter(abs(cambio_porcentual) > 20) %>%  # Cambios >20% son sospechosos
  select(estado, año, ingreso_anual_real, cambio_porcentual)



# Crear variable dummy para años COVID
ingresos_mejorada <- ingresos %>%
  mutate(
    covid = ifelse(año == 2020, 1, 0),
    periodo = case_when(
      año <= 2019 ~ "Pre-COVID",
      año == 2020 ~ "COVID",
      año >= 2021 ~ "Post-COVID"
    )
  )

# Ver si el patrón persiste
ingresos_mejorada %>%
  group_by(periodo) %>%
  summarise(
    ingreso_prom = mean(ingreso_anual_real, na.rm = TRUE),
    cv = sd(ingreso_anual_real)/mean(ingreso_anual_real)
  )



# Limitar cambios porcentuales anuales
# CORRECCIÓN: Winsorizar cambios extremos
ingresos_suavizado <- ingresos %>%
  group_by(estado) %>%
  arrange(año) %>%
  mutate(
    # Calcular cambios porcentuales
    ingreso_lag = lag(ingreso_anual_real),
    cambio_pct = (ingreso_anual_real / ingreso_lag - 1) * 100,
    
    # Limitar cambios extremos (a ±30%)
    cambio_acotado = case_when(
      cambio_pct > 30 ~ 30,
      cambio_pct < -30 ~ -30,
      TRUE ~ cambio_pct
    ),
    
    # Reconstruir serie con cambios acotados
    ingreso_suavizado = ifelse(
      año == min(año),
      ingreso_anual_real,
      NA_real_
    )
  ) %>%
  # Llenar hacia adelante (versión corregida)
  mutate(
    ingreso_suavizado = accumulate(
      cambio_acotado[-1], 
      ~ .x * (1 + .y/100), 
      .init = ingreso_suavizado[1]
    )
  ) %>%
  ungroup()

# Verificar
ingresos_suavizado %>%
  filter(estado %in% c("Tabasco", "Morelos", "Hidalgo")) %>%
  select(estado, año, ingreso_anual_real, cambio_pct, ingreso_suavizado)




# Versión log (sí debe funcionar)
ingresos_modelos <- ingresos %>%
  mutate(
    log_ingreso = log(ingreso_anual_real),
    año_factor = as.factor(año),
    covid = ifelse(año == 2020, 1, 0),
    periodo = case_when(
      año <= 2019 ~ "Pre-COVID",
      año == 2020 ~ "COVID",
      año >= 2021 ~ "Post-COVID"
    )
  )

# Verificar que se creó
head(ingresos_modelos %>% select(estado, año, ingreso_anual_real, log_ingreso))

# Si no funciona, es porque hay valores cero o negativos
summary(ingresos$ingreso_anual_real)  # Verificar que todos > 0





#para modelos econometricos
base_modelos <- ingresos %>%
  mutate(
    log_ingreso = log(ingreso_anual_real),
    año_factor = as.factor(año),
    dummy_covid = ifelse(año == 2020, 1, 0),
    # Tendencias por periodo
    tendencia_pre = ifelse(año <= 2019, año - 2013, 0),
    tendencia_covid = ifelse(año == 2020, 1, 0),
    tendencia_post = ifelse(año >= 2021, año - 2020, 0)
  )




# ============================================
# POBREZA LABORAL POR ESTADO (2014-2023)
# ============================================

cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("CÁLCULO DE POBREZA LABORAL (ingreso < canasta básica)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

# 1. Cargar tu base
ingresos <- read_csv("/Users/dmares/ingresos_totales_reales_estados_2014_2023.csv")  # o el nombre que le hayas puesto

# 2. Líneas de pobreza laboral (Canasta Básica Alimentaria - CONEVAL)
lineas_pobreza <- data.frame(
  año = 2014:2023,
  # Valor mensual por persona (promedio nacional)
  canasta_basica = c(
    1066.88,  # 2014 (promedio urbano/rural)
    1122.63,  # 2015
    1216.38,  # 2016
    1327.90,  # 2017
    1394.51,  # 2018
    1465.05,  # 2019
    1555.43,  # 2020
    1624.75,  # 2021
    1775.07,  # 2022
    1913.79   # 2023
  )
) %>%
  mutate(
    canasta_basica_anual = canasta_basica * 12  # Versión anual
  )

# 3. Calcular pobreza laboral (usando distribución log-normal)
pobreza_laboral <- ingresos %>%
  left_join(lineas_pobreza, by = "año") %>%
  mutate(
    # Parámetros de distribución log-normal
    # A partir de la media y el GINI estimamos la forma
    # Fórmula: GINI = 2Φ(σ/√2) - 1  →  σ = √2 * Φ⁻¹((GINI+1)/2)
    sigma = sqrt(2) * qnorm((gini_hogares + 1) / 2),
    mu = log(ingreso_anual_real) - (sigma^2)/2,
    
    # Proporción bajo la línea de pobreza
    prob_pobreza = plnorm(canasta_basica_anual, meanlog = mu, sdlog = sigma),
    
    # Tasa de pobreza laboral (%)
    tasa_pobreza_laboral = prob_pobreza * 100,
    
    # Número de pobres (aproximado)
    pobres_laborales = prob_pobreza * poblacion_estado
  )

# 4. Ver resultados
cat("\n📊 TASA DE POBREZA LABORAL PROMEDIO POR AÑO:\n")
pobreza_laboral %>%
  group_by(año) %>%
  summarise(
    tasa_nacional = weighted.mean(tasa_pobreza_laboral, poblacion_estado, na.rm = TRUE)
  ) %>%
  mutate(tasa = paste0(round(tasa_nacional, 1), "%"))

# 5. Top estados
cat("\n🏆 ESTADOS CON MAYOR POBREZA LABORAL (promedio 2014-2023):\n")
pobreza_laboral %>%
  group_by(estado) %>%
  summarise(tasa_prom = mean(tasa_pobreza_laboral, na.rm = TRUE)) %>%
  arrange(desc(tasa_prom)) %>%
  head(5) %>%
  mutate(tasa = paste0(round(tasa_prom, 1), "%"))

cat("\n📉 ESTADOS CON MENOR POBREZA LABORAL (promedio 2014-2023):\n")
pobreza_laboral %>%
  group_by(estado) %>%
  summarise(tasa_prom = mean(tasa_pobreza_laboral, na.rm = TRUE)) %>%
  arrange(tasa_prom) %>%
  head(5) %>%
  mutate(tasa = paste0(round(tasa_prom, 1), "%"))

# 6. Evolución temporal
library(ggplot2)

pobreza_nacional <- pobreza_laboral %>%
  group_by(año) %>%
  summarise(
    tasa_nacional = weighted.mean(tasa_pobreza_laboral, poblacion_estado, na.rm = TRUE)
  )

ggplot(pobreza_nacional, aes(x = año, y = tasa_nacional)) +
  geom_line(color = "darkred", size = 1.5) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = paste0(round(tasa_nacional, 1), "%")),
            vjust = -1, size = 3.5) +
  labs(title = "Evolución de la Pobreza Laboral en México",
       subtitle = "Personas con ingreso laboral insuficiente para canasta básica",
       x = "Año", y = "Tasa de Pobreza Laboral (%)") +
  scale_x_continuous(breaks = 2014:2023) +
  theme_minimal()

# 7. Guardar
write_csv(pobreza_laboral, "pobreza_laboral_estados_2014_2023.csv")
write_csv(pobreza_nacional, "pobreza_laboral_nacional_2014_2023.csv")

cat("\n✅ Archivos guardados:\n")
cat("   - pobreza_laboral_estados_2014_2023.csv\n")
cat("   - pobreza_laboral_nacional_2014_2023.csv\n")







# ============================================
# POBREZA LABORAL POR ESTADO 2014-2023
# Línea URBANA de Canasta Básica Alimentaria (CONEVAL)
# ============================================

cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("CÁLCULO DE POBREZA LABORAL (LÍNEA URBANA CONEVAL)\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

# ============================================
# 1. CARGAR PAQUETES
# ============================================

library(tidyverse)
library(readr)
library(ggrepel)

# ============================================
# 2. CARGAR TU BASE DE INGRESOS
# ===========================================-

# Cargar la base correcta
# Cargar tu base correcta
ingresos <- read_csv("ingresos_totales_reales_estados_2014_2023.csv")

# ============================================
# 3. LÍNEAS DE POBREZA LABORAL (URBANA - CONEVAL)
# ============================================

# Fuente: CONEVAL - Canasta Básica Alimentaria URBANA
# https://www.coneval.org.mx/Medicion/Paginas/Lineas-de-bienestar-y-canasta-basica.aspx

lineas_pobreza <- data.frame(
  año = 2014:2023,
  # Valores URBANOS mensuales (pesos corrientes)
  canasta_urbana_mensual = c(
    1227.42,  # 2014
    1272.91,  # 2015
    1337.42,  # 2016
    1438.90,  # 2017
    1523.17,  # 2018
    1598.42,  # 2019
    1700.27,  # 2020
    1787.18,  # 2021
    1982.45,  # 2022
    2177.45   # 2023
  )
) %>%
  mutate(
    # Versión anual
    canasta_urbana_anual = canasta_urbana_mensual * 12,
    
    # Convertir a reales (2018) para consistencia con tu ingreso
    factor_deflactor = 100 / c(85.333, 87.6545, 90.1279, 95.573,
                               100.2553, 103.9007, 107.43,
                               113.5419, 122.5075, 129.2797),
    canasta_real_anual = canasta_urbana_anual * factor_deflactor
  )

cat("\n📊 Línea de pobreza URBANA (anual, miles de pesos):\n")
print(lineas_pobreza %>%
        mutate(linea_miles = round(canasta_urbana_anual/1000, 1)) %>%
        select(año, linea_miles))

# ============================================
# 4. CALCULAR POBREZA LABORAL
# ============================================

cat("\n⚙️ Calculando pobreza laboral por estado...\n")

pobreza_laboral <- ingresos %>%
  left_join(lineas_pobreza, by = "año") %>%
  mutate(
    # Parámetros de distribución log-normal
    sigma = sqrt(2) * qnorm((gini_hogares + 1) / 2),
    mu = log(ingreso_anual_real) - (sigma^2)/2,
    
    # Probabilidad de pobreza (usando línea REAL para consistencia)
    prob_pobreza = plnorm(canasta_real_anual, meanlog = mu, sdlog = sigma),
    
    # Tasa de pobreza laboral (%)
    tasa_pobreza = prob_pobreza * 100,
    
    # Número absoluto de personas en pobreza
    pobres_absolutos = prob_pobreza * poblacion_estado,
    
    # Clasificación por nivel
    nivel_pobreza = case_when(
      tasa_pobreza < 20 ~ "Baja",
      tasa_pobreza < 35 ~ "Media",
      tasa_pobreza < 50 ~ "Alta",
      TRUE ~ "Muy alta"
    )
  )

# ============================================
# 5. RESUMEN NACIONAL
# ============================================

pobreza_nacional <- pobreza_laboral %>%
  group_by(año) %>%
  summarise(
    poblacion_total = sum(poblacion_estado, na.rm = TRUE),
    pobres_totales = sum(pobres_absolutos, na.rm = TRUE),
    tasa_nacional = (pobres_totales / poblacion_total) * 100,
    .groups = "drop"
  ) %>%
  mutate(
    pobres_millones = round(pobres_totales / 1e6, 1),
    tasa_formato = paste0(round(tasa_nacional, 1), "%")
  )

cat("\n📈 POBREZA LABORAL NACIONAL:\n")
print(pobreza_nacional %>% select(año, tasa_formato, pobres_millones))

# ============================================
# 6. TOP ESTADOS
# ============================================

cat("\n🏆 ESTADOS CON MAYOR TASA DE POBREZA (promedio 2014-2023):\n")
pobreza_laboral %>%
  group_by(estado) %>%
  summarise(
    tasa_prom = mean(tasa_pobreza, na.rm = TRUE),
    pobres_prom = mean(pobres_absolutos, na.rm = TRUE) / 1e6
  ) %>%
  arrange(desc(tasa_prom)) %>%
  head(5) %>%
  mutate(
    tasa_fmt = paste0(round(tasa_prom, 1), "%"),
    pobres_fmt = paste0(round(pobres_prom, 1), "M")
  ) %>%
  select(estado, tasa_fmt, pobres_fmt)

cat("\n🏆 ESTADOS CON MÁS POBRES ABSOLUTOS (promedio 2014-2023):\n")
pobreza_laboral %>%
  group_by(estado) %>%
  summarise(
    pobres_prom = mean(pobres_absolutos, na.rm = TRUE) / 1e6,
    tasa_prom = mean(tasa_pobreza, na.rm = TRUE)
  ) %>%
  arrange(desc(pobres_prom)) %>%
  head(5) %>%
  mutate(
    pobres_fmt = paste0(round(pobres_prom, 1), "M"),
    tasa_fmt = paste0(round(tasa_prom, 1), "%")
  ) %>%
  select(estado, pobres_fmt, tasa_fmt)

cat("\n📉 ESTADOS CON MENOR TASA DE POBREZA (promedio 2014-2023):\n")
pobreza_laboral %>%
  group_by(estado) %>%
  summarise(tasa_prom = mean(tasa_pobreza, na.rm = TRUE)) %>%
  arrange(tasa_prom) %>%
  head(5) %>%
  mutate(tasa_fmt = paste0(round(tasa_prom, 1), "%")) %>%
  select(estado, tasa_fmt)

# ============================================
# 7. GRÁFICOS
# ============================================

library(ggplot2)

# Gráfico 1: Evolución nacional (tasa)
g1 <- ggplot(pobreza_nacional, aes(x = año, y = tasa_nacional)) +
  geom_line(color = "darkred", size = 1.5) +
  geom_point(color = "red", size = 3) +
  geom_text(aes(label = paste0(round(tasa_nacional, 1), "%")),
            vjust = -1, size = 3.5) +
  labs(title = "Evolución de la Pobreza Laboral en México",
       subtitle = "Línea de pobreza: Canasta Básica Alimentaria URBANA",
       x = "Año", y = "Tasa de Pobreza Laboral (%)",
       caption = "Fuente: ENOE - Cálculos propios") +
  scale_x_continuous(breaks = 2014:2023) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

print(g1)

# Gráfico 2: Personas en pobreza (millones)
g2 <- ggplot(pobreza_nacional, aes(x = año, y = pobres_millones)) +
  geom_col(fill = "darkred", alpha = 0.7) +
  geom_text(aes(label = paste0(pobres_millones, "M")),
            vjust = -0.5, size = 3.5) +
  labs(title = "Personas en Pobreza Laboral en México",
       x = "Año", y = "Millones de personas") +
  scale_x_continuous(breaks = 2014:2023) +
  theme_minimal()

print(g2)

# Gráfico 3: Relación ingreso-pobreza (2023)
pobreza_2023 <- pobreza_laboral %>% filter(año == 2023)

g3 <- ggplot(pobreza_2023, aes(x = ingreso_anual_real/1000, 
                               y = tasa_pobreza, 
                               size = poblacion_estado/1e6,
                               label = estado)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_text_repel(size = 3, max.overlaps = 15) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linetype = "dashed") +
  labs(title = "Relación Ingreso - Pobreza Laboral (2023)",
       x = "Ingreso Anual Real (miles de pesos 2018)",
       y = "Tasa de Pobreza (%)",
       size = "Población (M)") +
  scale_x_continuous(labels = scales::dollar_format(prefix = "$")) +
  theme_minimal()

print(g3)

# ============================================
# 8. GUARDAR RESULTADOS
# ============================================

# Base completa por estado-año
write_csv(pobreza_laboral, "pobreza_laboral_urbana_estados.csv")

# Resumen nacional
write_csv(pobreza_nacional, "pobreza_laboral_urbana_nacional.csv")

# Base para modelos (con logs y variables adicionales)
base_modelos <- pobreza_laboral %>%
  mutate(
    log_pobres = log(pobres_absolutos),
    log_tasa = log(tasa_pobreza + 0.1),  # +0.1 para evitar log(0)
    log_ingreso = log(ingreso_anual_real),
    año_factor = as.factor(año),
    covid = ifelse(año == 2020, 1, 0),
    periodo = case_when(
      año <= 2019 ~ "Pre-COVID",
      año == 2020 ~ "COVID",
      año >= 2021 ~ "Post-COVID"
    )
  )

write_csv(base_modelos, "base_pobreza_modelos.csv")

cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("✅ PROCESO COMPLETADO\n")
cat("📁 Archivos guardados:\n")
cat("   1. pobreza_laboral_urbana_estados.csv\n")
cat("   2. pobreza_laboral_urbana_nacional.csv\n")
cat("   3. base_pobreza_modelos.csv\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

# ============================================
# 9. VALIDACIÓN RÁPIDA
# ============================================

cat("\n🔍 VALIDACIÓN:\n")
cat("   CONEVAL reporta pobreza laboral 2020: ~38-40%\n")
cat("   Tu estimación 2020:", 
    round(pobreza_nacional$tasa_nacional[pobreza_nacional$año == 2020], 1), "%\n")

cor_ingreso <- cor(pobreza_laboral$ingreso_anual_real, 
                   pobreza_laboral$tasa_pobreza, 
                   use = "complete.obs")
cat("   Correlación ingreso-pobreza: r =", round(cor_ingreso, 3), "\n")
cat("   (Negativa y fuerte es lo esperado)\n")






# Revisemos los valores que estás usando
ingresos %>% 
  filter(año == 2023) %>% 
  select(estado, ingreso_anual_real) %>% 
  head()

# Deberías ver algo como:
# ingreso_anual_real ≈ 200,000 - 500,000 pesos

# Ahora revisemos la línea de pobreza que estás usando
lineas_pobreza %>% filter(año == 2023)
# canasta_real_anual ≈ ? (debería ser ~30,000 - 40,000 pesos)

# Si la línea es mucho más baja que los ingresos, todas las tasas serán cercanas a 0



# ============================================
# SOLUCIÓN FINAL: POBREZA CON INGRESO PER CÁPITA
# ============================================

# 1. Usar el factor correcto de personas por hogar
# Según INEGI (ENIGH 2022): promedio nacional = 3.4 personas por hogar
personas_por_hogar <- 3.4

# 2. Crear ingreso per cápita
ingresos_pc <- ingresos %>%
  mutate(
    # Ingreso per cápita anual
    ingreso_pc_anual = ingreso_anual_real / personas_por_hogar,
    # Ingreso per cápita mensual (para verificar que sea realista)
    ingreso_pc_mensual = ingreso_pc_anual / 12
  )

# 3. Verificar que ahora los ingresos son realistas
cat("\n📊 INGRESO PER CÁPITA MENSUAL (2023):\n")
ingresos_pc %>%
  filter(año == 2023) %>%
  select(estado, ingreso_pc_mensual) %>%
  mutate(
    ingreso_fmt = paste0("$", format(round(ingreso_pc_mensual), big.mark = ","))
  ) %>%
  arrange(desc(ingreso_pc_mensual)) %>%
  head(10) %>%
  print()

# 4. Líneas de pobreza (CORRECTAS)
lineas_pobreza <- data.frame(
  año = 2014:2023,
  canasta_mensual = c(
    1227.42, 1272.91, 1337.42, 1438.90, 1523.17,
    1598.42, 1700.27, 1787.18, 1982.45, 2177.45
  )
) %>%
  mutate(
    canasta_anual = canasta_mensual * 12,
    # Ajustar por inflación a pesos 2018
    factor_deflactor = 100 / c(85.333, 87.6545, 90.1279, 95.573,
                               100.2553, 103.9007, 107.43,
                               113.5419, 122.5075, 129.2797),
    linea_pobreza_real = canasta_anual * factor_deflactor
  )

# 5. Calcular pobreza con ingreso PER CÁPITA
pobreza_final <- ingresos_pc %>%
  left_join(lineas_pobreza, by = "año") %>%
  mutate(
    # Parámetros de distribución log-normal
    sigma = sqrt(2) * qnorm((gini_hogares + 1) / 2),
    mu = log(ingreso_pc_anual) - (sigma^2)/2,
    
    # Probabilidad de pobreza (ingreso per cápita < línea)
    prob_pobreza = plnorm(linea_pobreza_real, meanlog = mu, sdlog = sigma),
    
    # Tasa de pobreza (%)
    tasa_pobreza = prob_pobreza * 100,
    
    # Número de personas en pobreza
    pobres_absolutos = prob_pobreza * poblacion_estado
  )

# 6. Resultados nacionales (¡AHORA SÍ!)
pobreza_nacional <- pobreza_final %>%
  group_by(año) %>%
  summarise(
    tasa_nacional = weighted.mean(tasa_pobreza, poblacion_estado, na.rm = TRUE),
    pobres_totales = sum(pobres_absolutos, na.rm = TRUE),
    ingreso_pc_nacional = weighted.mean(ingreso_pc_mensual, poblacion_estado, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pobres_millones = pobres_totales / 1e6,
    tasa_formato = paste0(round(tasa_nacional, 1), "%"),
    ingreso_fmt = paste0("$", format(round(ingreso_pc_nacional), big.mark = ","))
  )

cat("\n", paste0(rep("=", 80), collapse = ""), "\n")
cat("📈 POBREZA LABORAL NACIONAL FINAL (ingreso per cápita):\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

print(pobreza_nacional %>%
        select(año, tasa_formato, pobres_millones, ingreso_fmt))

# 7. Comparar con lo esperado
cat("\n✅ COMPARACIÓN CON CONEVAL (pobreza laboral):\n")
cat("   CONEVAL 2020: ~38-40%\n")
cat("   Tu estimación 2020:", 
    pobreza_nacional$tasa_formato[pobreza_nacional$año == 2020], "\n")

# 8. Resultados por estado
cat("\n🏆 ESTADOS CON MAYOR POBREZA (promedio 2014-2023):\n")
pobreza_final %>%
  group_by(estado) %>%
  summarise(
    tasa_prom = mean(tasa_pobreza, na.rm = TRUE),
    ingreso_pc_prom = mean(ingreso_pc_mensual, na.rm = TRUE)
  ) %>%
  arrange(desc(tasa_prom)) %>%
  head(5) %>%
  mutate(
    tasa_fmt = paste0(round(tasa_prom, 1), "%"),
    ingreso_fmt = paste0("$", format(round(ingreso_pc_prom), big.mark = ","))
  ) %>%
  select(estado, tasa_fmt, ingreso_fmt) %>%
  print()

cat("\n📉 ESTADOS CON MENOR POBREZA (promedio 2014-2023):\n")
pobreza_final %>%
  group_by(estado) %>%
  summarise(tasa_prom = mean(tasa_pobreza, na.rm = TRUE)) %>%
  arrange(tasa_prom) %>%
  head(5) %>%
  mutate(tasa_fmt = paste0(round(tasa_prom, 1), "%")) %>%
  select(estado, tasa_fmt) %>%
  print()

# 9. Guardar resultados
write_csv(pobreza_final, "pobreza_laboral_final.csv")
write_csv(pobreza_nacional, "pobreza_nacional_final.csv")

cat("\n✅ Archivos guardados:\n")
cat("   - pobreza_laboral_final.csv\n")
cat("   - pobreza_nacional_final.csv\n")










# ============================================
# VALIDACIÓN ESTADÍSTICA DE LA POBREZA LABORAL
# Comparación con CONEVAL (línea urbana)
# ============================================
library(tidyverse)

# Cargar archivo (ajusta ruta si es necesario)
pobreza_wide <- read_csv("pobreza_laboral_final.csv")

# Renombrar primera columna y eliminar fila de total general
pobreza_long <- pobreza_wide %>%
  rename(estado = `Etiquetas de fila`) %>%
  filter(estado != "Total general") %>%
  pivot_longer(
    cols = -estado,
    names_to = "año",
    values_to = "tasa_pobreza"
  ) %>%
  mutate(
    año = as.numeric(año),
    tasa_pobreza = as.numeric(tasa_pobreza)
  )

# Verificar
head(pobreza_long)

# Guardar versión limpia
write_csv(pobreza_long, "pobreza_laboral_limpia.csv")


library(readxl)
library(tidyverse)

# Ver las hojas del archivo
excel_sheets("/Users/dmares/Downloads/pm_ip_2024.xlsx")

# Cargar las primeras 20 filas para inspeccionar
raw_head <- read_excel("/Users/dmares/Downloads/pm_ip_2024.xlsx", sheet = "Cuadro 2", n_max = 20)
print(raw_head)


coneval_raw <- read_excel("/Users/dmares/Downloads/pm_ip_2024.xlsx", 
                          sheet = "Cuadro 2", 
                          skip = 8,        # Ajusta si es necesario
                          col_names = FALSE)

head(coneval_raw, 10)

coneval_clean <- coneval_raw %>%
  select(1, 10:14) %>%
  rename(
    estado = 1,
    pobreza_2016 = 2,
    pobreza_2018 = 3,
    pobreza_2020 = 4,
    pobreza_2022 = 5,
    pobreza_2024 = 6
  ) %>%
  # Eliminar filas que no sean estados (como notas al pie)
  filter(!is.na(estado), 
         !str_detect(estado, "Nota|Fuente|Estados Unidos Mexicanos"),
         estado != "") %>%
  # Convertir a numérico (algunos pueden tener comas o caracteres)
  mutate(across(starts_with("pobreza"), ~ as.numeric(.)))

# Verificar
head(coneval_clean, 10)

coneval_long <- coneval_clean %>%
  pivot_longer(
    cols = starts_with("pobreza"),
    names_to = "año",
    values_to = "pobreza_coneval",
    names_prefix = "pobreza_"
  ) %>%
  mutate(año = as.numeric(año))

head(coneval_long)

comparacion <- pobreza_long %>%
  filter(año %in% c(2016, 2018, 2020, 2022)) %>%
  left_join(coneval_long, by = c("estado", "año")) %>%
  filter(!is.na(pobreza_coneval))  # Eliminar estados sin dato (si alguno falta)

# Verificar
head(comparacion)

# Correlación de Pearson
cor_pearson <- cor.test(comparacion$tasa_pobreza, 
                        comparacion$pobreza_coneval, 
                        method = "pearson")

# Correlación de Spearman
cor_spearman <- cor.test(comparacion$tasa_pobreza, 
                         comparacion$pobreza_coneval, 
                         method = "spearman")

# MAE (Error absoluto medio)
mae <- mean(abs(comparacion$tasa_pobreza - comparacion$pobreza_coneval), na.rm = TRUE)

# Mostrar resultados
cat("\n", paste0(rep("=", 60), collapse = ""), "\n")
cat("VALIDACIÓN POBREZA LABORAL vs POBREZA MULTIDIMENSIONAL\n")
cat(paste0(rep("=", 60), collapse = ""), "\n")
cat("Correlación Pearson: r =", round(cor_pearson$estimate, 3), 
    "(p =", format.pval(cor_pearson$p.value, digits = 3), ")\n")
cat("Correlación Spearman: ρ =", round(cor_spearman$estimate, 3), "\n")
cat("MAE (puntos porcentuales):", round(mae, 2), "\n")

library(ggplot2)

ggplot(comparacion, aes(x = pobreza_coneval, y = tasa_pobreza, color = as.factor(año))) +
  geom_point(alpha = 0.6, size = 2) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue", alpha = 0.5) +
  labs(title = "Validación: Pobreza Laboral (ENOE) vs Pobreza Multidimensional (CONEVAL)",
       subtitle = paste("r =", round(cor_pearson$estimate, 3),
                        "| ρ =", round(cor_spearman$estimate, 3),
                        "| MAE =", round(mae, 1), "p.p."),
       x = "Pobreza Multidimensional (%)",
       y = "Pobreza Laboral ENOE (%)",
       color = "Año") +
  theme_minimal()







library(tidyverse)

# 1. Cargar el archivo de Gini (formato ancho)
gini_wide <- read_csv("/Users/dmares/ingresos_totales_reales_estados_2014_2023.csv")

# Ver estructura
glimpse(gini_wide)
# Debe tener una columna 'Etiquetas de fila' y columnas numéricas para cada año.

# 2. Transformar a formato largo
gini_long <- gini_wide %>%
  rename(estado = `Etiquetas de fila`) %>%
  filter(estado != "Total general") %>%  # eliminar fila de total
  pivot_longer(
    cols = -estado,
    names_to = "año",
    values_to = "gini"
  ) %>%
  mutate(
    año = as.numeric(año),
    gini = as.numeric(gini)
  )

# 3. Cargar la base de población (puede ser la de ingresos)
poblacion <- read_csv("ingresos_totales_reales_estados_2014_2023.csv") %>%
  select(estado, año, poblacion_estado)

# 4. Unir Gini con población
gini_con_pob <- gini_long %>%
  left_join(poblacion, by = c("estado", "año"))

# 5. Calcular Gini nacional ponderado por población
gini_nacional <- gini_con_pob %>%
  group_by(año) %>%
  summarise(
    gini_nacional = weighted.mean(gini, poblacion_estado, na.rm = TRUE),
    .groups = "drop"
  )

# 6. Mostrar resultados
print(gini_nacional)

# 7. Si quieres comparar con ENIGH (solo años 2014,2016,2018,2020,2022)
enigh_gini <- data.frame(
  año = c(2014, 2016, 2018, 2020, 2022),
  gini_enigh = c(0.440, 0.449, 0.426, 0.415, 0.402)
)

comparacion_gini <- gini_nacional %>%
  filter(año %in% c(2014,2016,2018,2020,2022)) %>%
  left_join(enigh_gini, by = "año") %>%
  mutate(diferencia = gini_nacional - gini_enigh)

print(comparacion_gini)

# 8. Gráfico de comparación
ggplot(comparacion_gini, aes(x = año)) +
  geom_line(aes(y = gini_nacional, color = "ENOE")) +
  geom_point(aes(y = gini_nacional, color = "ENOE")) +
  geom_line(aes(y = gini_enigh, color = "ENIGH")) +
  geom_point(aes(y = gini_enigh, color = "ENIGH")) +
  labs(title = "Comparación Gini Nacional: ENOE vs ENIGH",
       x = "Año", y = "Coeficiente de Gini",
       color = "Fuente") +
  theme_minimal()






# Tus Gini ENOE (hogares) para años coincidentes
enoe_gini <- c(0.497, 0.484, 0.476, 0.486, 0.486)

# Gini ENIGH oficial
enigh_gini <- c(0.440, 0.449, 0.426, 0.415, 0.402)

# Pearson
cor_pearson <- cor(enoe_gini, enigh_gini, method = "pearson")
# Spearman
cor_spearman <- cor(enoe_gini, enigh_gini, method = "spearman")

cat("Pearson:", round(cor_pearson, 3), "\n")
cat("Spearman:", round(cor_spearman, 3), "\n")




# Cargar librerías

# 1. Tus Gini ENOE desde el archivo (ajusta nombres si es necesario)
# Si ya tienes 'gini_wide' cargado:
gini_enoe <- gini_wide %>%
  rename(estado = `Etiquetas de fila`) %>%
  filter(estado != "Total general") %>%
  select(estado, gini_2016 = `2016`, gini_2018 = `2018`)

# 2. Datos de Wikipedia (Gini ENIGH 2016 y 2018)
gini_wiki <- data.frame(
  estado = c("Aguascalientes", "Baja California", "Baja California Sur", "Campeche", 
             "Chiapas", "Chihuahua", "Ciudad de México", "Coahuila", "Colima", 
             "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco", 
             "México", "Michoacán", "Morelos", "Nayarit", "Nuevo León", "Oaxaca", 
             "Puebla", "Querétaro", "Quintana Roo", "San Luis Potosí", "Sinaloa", 
             "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán", 
             "Zacatecas"),
  gini_wiki_2016 = c(0.416, 0.430, 0.439, 0.467, 0.508, 0.473, 0.507, 0.417, 0.423,
                     0.415, 0.576, 0.471, 0.430, 0.422, 0.414, 0.424, 0.437, 0.472,
                     0.578, 0.493, 0.439, 0.480, 0.435, 0.450, 0.428, 0.498, 0.459,
                     0.474, 0.378, 0.489, 0.452, 0.491),
  gini_wiki_2018 = c(0.432, 0.402, 0.432, 0.472, 0.487, 0.443, 0.532, 0.414, 0.423,
                     0.419, 0.416, 0.482, 0.423, 0.430, 0.401, 0.424, 0.429, 0.437,
                     0.435, 0.496, 0.407, 0.437, 0.414, 0.464, 0.446, 0.439, 0.447,
                     0.472, 0.373, 0.453, 0.456, 0.419)
)

# 3. Unir bases
comparacion <- gini_enoe %>%
  left_join(gini_wiki, by = "estado")

# 4. Correlaciones para 2016
cor_pearson_2016 <- cor(comparacion$gini_2016, comparacion$gini_wiki_2016, use = "complete.obs")
cor_spearman_2016 <- cor(comparacion$gini_2016, comparacion$gini_wiki_2016, method = "spearman", use = "complete.obs")

# 5. Correlaciones para 2018
cor_pearson_2018 <- cor(comparacion$gini_2018, comparacion$gini_wiki_2018, use = "complete.obs")
cor_spearman_2018 <- cor(comparacion$gini_2018, comparacion$gini_wiki_2018, method = "spearman", use = "complete.obs")

# 6. Mostrar resultados
cat("===== VALIDACIÓN GINI (ENOE vs WIKIPEDIA) =====\n")
cat("2016:\n")
cat("  Pearson:", round(cor_pearson_2016, 3), "\n")
cat("  Spearman:", round(cor_spearman_2016, 3), "\n\n")
cat("2018:\n")
cat("  Pearson:", round(cor_pearson_2018, 3), "\n")
cat("  Spearman:", round(cor_spearman_2018, 3), "\n")

# 7. Gráfico opcional para 2016
ggplot(comparacion, aes(x = gini_wiki_2016, y = gini_2016, label = estado)) +
  geom_point() +
  geom_text_repel(size = 3) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Comparación Gini ENIGH (Wikipedia) vs ENOE (2016)",
       x = "Gini ENIGH 2016", y = "Gini ENOE 2016") +
  theme_minimal()
