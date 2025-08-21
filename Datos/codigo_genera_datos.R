# ================================================================
# GENERADOR DE DATASET ECOLOGISTICS - PREDICCIÓN DE RETRASOS
# ================================================================
# Genera dataset sintético pero realista para la actividad de ML

library(dplyr)
library(lubridate)

# Fijar semilla para reproducibilidad
set.seed(42)

# Parámetros del dataset
n_envios <- 50000  # 50,000 envíos históricos
proporcion_retrasos <- 0.15  # 15% de retrasos según el enunciado

print("Generando dataset EcoLogistics...")
print(paste("Total de envíos:", n_envios))
print(paste("Proporción esperada de retrasos:", proporcion_retrasos))


# 1. Variables básicas -------------------------------------------------------


# Información del envío
peso <- round(rlnorm(n_envios, meanlog = 3, sdlog = 1), 1)  # kg, distribución log-normal
peso <- pmax(peso, 0.5)  # Mínimo 0.5 kg
peso <- pmin(peso, 5000)  # Máximo 5000 kg

volumen <- round(rlnorm(n_envios, meanlog = 1.5, sdlog = 0.8), 2)  # m³
volumen <- pmax(volumen, 0.01)  # Mínimo 0.01 m³
volumen <- pmin(volumen, 100)   # Máximo 100 m³

# Valor declarado (correlacionado con peso y volumen)
valor_base <- peso * runif(n_envios, 50, 500) + volumen * runif(n_envios, 10000, 50000)
valor_declarado <- round(pmax(valor_base + rnorm(n_envios, 0, 10000), 5000))

# Tipo de producto (influye en urgencia y manejo)
tipos_producto <- c("electronica", "ropa", "alimentos", "muebles", "documentos", "otros")
prob_tipos <- c(0.15, 0.25, 0.20, 0.15, 0.10, 0.15)
tipo_producto <- sample(tipos_producto, n_envios, replace = TRUE, prob = prob_tipos)



# 2. Informacion Geografica -----------------------------------------------

# Ciudades principales de Chile
ciudades <- c("Santiago", "Valparaíso", "Concepción", "La Serena", "Antofagasta", 
              "Temuco", "Valdivia", "Puerto Montt", "Iquique", "Copiapó", 
              "Talca", "Chillán", "Los Ángeles", "Osorno", "Calama")

origen <- sample(ciudades, n_envios, replace = TRUE, 
                 prob = c(0.4, 0.1, 0.1, 0.05, 0.05, 0.06, 0.04, 0.05, 0.03, 0.03, 0.04, 0.03, 0.03, 0.02, 0.02))

destino <- sample(ciudades, n_envios, replace = TRUE,
                  prob = c(0.35, 0.12, 0.12, 0.06, 0.06, 0.07, 0.05, 0.06, 0.04, 0.04, 0.05, 0.04, 0.04, 0.03, 0.03))

# Distancia basada en origen y destino (simplificado)
distancia_base <- ifelse(origen == destino, 
                         runif(n_envios, 5, 50),  # Misma ciudad
                         runif(n_envios, 100, 1500))  # Ciudades diferentes

#  Ajustar distancia según combinaciones específicas
distancia <- distancia_base

# Ajustar para Santiago-Valparaíso
mask_stgo_valpo <- (origen == "Santiago" & destino == "Valparaíso") | (origen == "Valparaíso" & destino == "Santiago")
if(sum(mask_stgo_valpo) > 0) {
  distancia[mask_stgo_valpo] <- runif(sum(mask_stgo_valpo), 120, 140)
}

# Ajustar para Santiago-Concepción
mask_stgo_conce <- (origen == "Santiago" & destino == "Concepción") | (origen == "Concepción" & destino == "Santiago")
if(sum(mask_stgo_conce) > 0) {
  distancia[mask_stgo_conce] <- runif(sum(mask_stgo_conce), 500, 520)
}
distancia <- round(distancia, 1)


# Zona (urbana/rural basada en ciudades)
zona_urbana_ciudades <- c("Santiago", "Valparaíso", "Concepción", "Antofagasta", "La Serena")
zona <- ifelse(destino %in% zona_urbana_ciudades & origen %in% zona_urbana_ciudades, 
               "urbana", 
               sample(c("urbana", "rural"), n_envios, replace = TRUE, prob = c(0.7, 0.3)))



# 3. Informacion Temporal -------------------------------------------------

# Generar fechas del último año
fecha_inicio <- as.Date("2023-01-01")
fecha_fin <- as.Date("2023-12-31")
fecha_envio <- sample(seq(fecha_inicio, fecha_fin, by = "day"), n_envios, replace = TRUE)

# Día de la semana
dia_semana <- weekdays(fecha_envio, abbreviate = FALSE)
dia_semana <- tolower(dia_semana)
dia_semana <- case_when(
  dia_semana == "monday" ~ "lunes",
  dia_semana == "tuesday" ~ "martes", 
  dia_semana == "wednesday" ~ "miercoles",
  dia_semana == "thursday" ~ "jueves",
  dia_semana == "friday" ~ "viernes",
  dia_semana == "saturday" ~ "sabado",
  dia_semana == "sunday" ~ "domingo"
)

# Mes
mes <- month(fecha_envio, label = TRUE, abbr = FALSE)
mes <- tolower(as.character(mes))

# Hora de salida (afecta tráfico)
hora_salida <- sample(6:22, n_envios, replace = TRUE, 
                      prob = c(0.05, 0.08, 0.12, 0.15, 0.12, 0.08, 0.08, 0.08, 0.08, 0.06, 0.04, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01))



# 4. Informacion del Conductor o Vehiculo ---------------------------------

# Experiencia del conductor (años)
experiencia_conductor <- round(rexp(n_envios, rate = 0.15))  # Distribución exponencial
experiencia_conductor <- pmin(experiencia_conductor, 25)  # Máximo 25 años

# Tipo de vehículo
tipos_vehiculo <- c("camion_pequeno", "camion_mediano", "camion_grande", "furgon")
prob_vehiculos <- c(0.35, 0.30, 0.20, 0.15)
tipo_vehiculo <- sample(tipos_vehiculo, n_envios, replace = TRUE, prob = prob_vehiculos)

# Antigüedad del vehículo (correlacionada con tipo)
antiguedad_base <- case_when(
  tipo_vehiculo == "furgon" ~ rpois(n_envios, 3),
  tipo_vehiculo == "camion_pequeno" ~ rpois(n_envios, 5),
  tipo_vehiculo == "camion_mediano" ~ rpois(n_envios, 7),
  tipo_vehiculo == "camion_grande" ~ rpois(n_envios, 10)
)
antiguedad_vehiculo <- pmin(antiguedad_base, 20)  # Máximo 20 años

# Capacidad de carga según tipo de vehículo
capacidad_carga <- numeric(n_envios)

# Asignar capacidad por tipo de vehículo
capacidad_carga[tipo_vehiculo == "furgon"] <- runif(sum(tipo_vehiculo == "furgon"), 800, 1200)
capacidad_carga[tipo_vehiculo == "camion_pequeno"] <- runif(sum(tipo_vehiculo == "camion_pequeno"), 2000, 3500)
capacidad_carga[tipo_vehiculo == "camion_mediano"] <- runif(sum(tipo_vehiculo == "camion_mediano"), 5000, 8000)
capacidad_carga[tipo_vehiculo == "camion_grande"] <- runif(sum(tipo_vehiculo == "camion_grande"), 10000, 15000)
capacidad_carga <- round(capacidad_carga)



# 5. Condiciones externas -------------------------------------------------

# Clima (estacional)
prob_clima_verano <- c(0.6, 0.25, 0.10, 0.05)  # despejado, nublado, lluvia, viento
prob_clima_invierno <- c(0.3, 0.35, 0.30, 0.05)

clima <- character(n_envios)
for(i in 1:n_envios) {
  if(mes[i] %in% c("diciembre", "enero", "febrero")) {
    clima[i] <- sample(c("despejado", "nublado", "lluvia", "viento"), 1, prob = prob_clima_verano)
  } else if(mes[i] %in% c("junio", "julio", "agosto")) {
    clima[i] <- sample(c("despejado", "nublado", "lluvia", "viento"), 1, prob = prob_clima_invierno)
  } else {
    clima[i] <- sample(c("despejado", "nublado", "lluvia", "viento"), 1, prob = c(0.45, 0.30, 0.20, 0.05))
  }
}

# Tráfico estimado (1-10, donde 10 es mucho tráfico)
trafico_base <- case_when(
  zona == "urbana" ~ sample(4:9, n_envios, replace = TRUE, prob = c(0.1, 0.15, 0.20, 0.25, 0.20, 0.10)),
  TRUE ~ sample(1:5, n_envios, replace = TRUE, prob = c(0.3, 0.3, 0.2, 0.15, 0.05))
)

# Ajustar tráfico por día de semana y hora
factor_dia <- case_when(
  dia_semana %in% c("lunes", "martes", "miercoles", "jueves", "viernes") ~ 1.2,
  dia_semana == "sabado" ~ 0.9,
  dia_semana == "domingo" ~ 0.7
)

factor_hora <- case_when(
  hora_salida %in% 7:9 ~ 1.4,   # Hora pico mañana
  hora_salida %in% 17:19 ~ 1.3, # Hora pico tarde
  hora_salida %in% 12:14 ~ 1.1, # Almuerzo
  TRUE ~ 1.0
)

trafico_estimado <- round(pmin(trafico_base * factor_dia * factor_hora, 10))

# Es feriado (aproximado)
feriados_chile <- as.Date(c("2023-01-01", "2023-04-07", "2023-04-08", "2023-05-01", 
                            "2023-05-21", "2023-06-26", "2023-07-16", "2023-08-15", 
                            "2023-09-18", "2023-09-19", "2023-10-09", "2023-11-01", 
                            "2023-12-08", "2023-12-25"))
es_feriado <- fecha_envio %in% feriados_chile



# 6. Tiempo estimado y Variable obj ---------------------------------------

# Tiempo estimado base (función de distancia)
tiempo_estimado_base <- distancia / 80 + runif(n_envios, 0.5, 2)  # Velocidad promedio 80 km/h + tiempo adicional

# Ajustar tiempo estimado por factores
tiempo_estimado <- tiempo_estimado_base * 
  case_when(
    tipo_vehiculo == "camion_grande" ~ 1.3,
    tipo_vehiculo == "camion_mediano" ~ 1.2,
    tipo_vehiculo == "camion_pequeno" ~ 1.1,
    TRUE ~ 1.0
  ) *
  case_when(
    clima == "lluvia" ~ 1.4,
    clima == "viento" ~ 1.2,
    clima == "nublado" ~ 1.1,
    TRUE ~ 1.0
  ) *
  (1 + (trafico_estimado - 1) * 0.1) *  # Cada punto de tráfico suma 10%
  ifelse(zona == "rural", 1.2, 1.0) *    # Rural es 20% más lento
  ifelse(es_feriado, 0.8, 1.0)           # Feriados menos tráfico

tiempo_estimado <- round(tiempo_estimado, 1)


# 7. Generar Variable Objetivo --------------------------------------------

# Probabilidad de retraso basada en múltiples factores
prob_retraso_base <- plogis(
  -2.5 +  # Intercepto para ~15% de retrasos base
    ifelse(peso > quantile(peso, 0.8), 0.8, 0) +  # Peso alto
    ifelse(distancia > 800, 0.6, 0) +  # Distancia larga
    ifelse(clima == "lluvia", 1.2, ifelse(clima == "viento", 0.6, 0)) +  # Mal clima
    ifelse(trafico_estimado > 7, 0.8, 0) +  # Mucho tráfico
    ifelse(antiguedad_vehiculo > 10, 0.5, 0) +  # Vehículo viejo
    ifelse(experiencia_conductor < 2, 0.7, ifelse(experiencia_conductor > 10, -0.3, 0)) +  # Experiencia
    ifelse(dia_semana %in% c("viernes", "lunes"), 0.4, 0) +  # Días problemáticos
    ifelse(hora_salida > 18, 0.5, 0) +  # Salida tarde
    ifelse(tipo_producto == "alimentos", 0.6, 0) +  # Productos perecederos
    ifelse(zona == "rural", 0.4, 0) +  # Zona rural
    ifelse(mes %in% c("diciembre", "enero"), 0.5, 0) +  # Temporada alta
    rnorm(n_envios, 0, 0.3)  # Ruido aleatorio
)

# Generar variable objetivo
retraso <- rbinom(n_envios, 1, prob_retraso_base)
retraso <- ifelse(retraso == 1, "Si", "No")

# Verificar proporción final de retrasos
prop_retrasos_final <- mean(retraso == "Si")
print(paste("Proporción final de retrasos:", round(prop_retrasos_final, 3)))




# 8. Missing Values -------------------------------------------------------

# Introducir NAs de forma realista
# Experiencia del conductor (5% missing - conductores temporales)
experiencia_conductor[sample(n_envios, n_envios * 0.05)] <- NA

# Clima (3% missing - sensores fallan)
clima[sample(n_envios, n_envios * 0.03)] <- NA

# Tráfico estimado (8% missing - sistema de estimación no disponible)
trafico_estimado[sample(n_envios, n_envios * 0.08)] <- NA

# Valor declarado (2% missing - no declarado)
valor_declarado[sample(n_envios, n_envios * 0.02)] <- NA



# 9. DF Final -------------------------------------------------------------

# Crear dataset final
envios_ecologistics <- data.frame(
  # ID único
  id_envio = paste0("ENV", sprintf("%05d", 1:n_envios)),
  
  # Información del envío
  peso = peso,
  volumen = volumen,
  valor_declarado = valor_declarado,
  tipo_producto = tipo_producto,
  
  # Información geográfica
  origen = origen,
  destino = destino,
  distancia = distancia,
  zona = zona,
  
  # Información temporal
  fecha_envio = fecha_envio,
  dia_semana = dia_semana,
  mes = mes,
  hora_salida = hora_salida,
  es_feriado = es_feriado,
  
  # Información conductor y vehículo
  experiencia_conductor = experiencia_conductor,
  tipo_vehiculo = tipo_vehiculo,
  antiguedad_vehiculo = antiguedad_vehiculo,
  capacidad_carga = capacidad_carga,
  
  # Condiciones externas
  clima = clima,
  trafico_estimado = trafico_estimado,
  
  # Variables calculadas
  tiempo_estimado = tiempo_estimado,
  
  # Variable objetivo
  retraso = retraso,
  
  stringsAsFactors = FALSE
)


# 10. Verificaciones  -----------------------------------------------------

print("=== RESUMEN DEL DATASET GENERADO ===")
print(paste("Dimensiones:", nrow(envios_ecologistics), "filas x", ncol(envios_ecologistics), "columnas"))
print(paste("Proporción de retrasos:", round(mean(envios_ecologistics$retraso == "Si"), 3)))
print(paste("Valores faltantes totales:", sum(is.na(envios_ecologistics))))

print("\n=== RESUMEN POR VARIABLE ===")
print(summary(envios_ecologistics))

print("\n=== DISTRIBUCIÓN DE VARIABLE OBJETIVO ===")
print(table(envios_ecologistics$retraso))
print(prop.table(table(envios_ecologistics$retraso)))

print("\n=== MISSING VALUES POR VARIABLE ===")
missing_summary <- sapply(envios_ecologistics, function(x) sum(is.na(x)))
print(missing_summary[missing_summary > 0])


# 11. Guardar Archivo -----------------------------------------------------

# Guardar el archivo CSV
write.csv(envios_ecologistics, "Clase ML/Actividad/envios_ecologistics.csv", row.names = FALSE)


# Guardar también un archivo de muestra pequeño para pruebas rápidas
muestra_pequena <- envios_ecologistics[sample(nrow(envios_ecologistics), 1000), ]
write.csv(muestra_pequena, "envios_ecologistics_muestra.csv", row.names = FALSE)



# Opcional: crear archivo para "envíos de mañana" (usado en el código malo)
envios_manana <- envios_ecologistics[sample(nrow(envios_ecologistics), 200), ]
envios_manana$retraso <- NULL  # Remover variable objetivo para simular predicción
write.csv(envios_manana, "envios_manana.csv", row.names = FALSE)
