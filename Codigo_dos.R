# =====================================================
# Segunda Solucion - EcoLogistics Predicción Retrasos
# =====================================================

# Análisis de ML para predecir retrasos en entregas


# Librerias clave ---------------------------------------------------------
library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(tidyr)  # Para pivot_longer (reemplaza gather)
library(VIM)  # Para análisis de missing values
library(pROC)  # Para curvas ROC

# Fijar semilla para reproducibilidad
set.seed(123)


# Analisis Exploratorio ---------------------------------------------------

# Cargar datos
datos <- read.csv("Datos/envios_ecologistics.csv", stringsAsFactors = FALSE)

# Información básica del dataset
dim(datos)
names(datos)

# Explorar variable objetivo
table(datos$retraso)
prop.table(table(datos$retraso))

cat("Proporción de retrasos:", mean(datos$retraso == "Si"))

# INSIGHT DE NEGOCIO: >15% de retrasos confirma el problema reportado por el CEO


# Calidad de datos --------------------------------------------------------

# Revisar missing values por variable
missing_summary <- datos %>%
  summarise_all(~sum(is.na(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "missing_count") %>%
  mutate(missing_pct = round(missing_count / nrow(datos) * 100, 2)) %>%
  arrange(desc(missing_count))

print(missing_summary)

# Visualizar patrones de missing values
VIM::aggr(datos, col = c('navyblue','red'), numbers = TRUE, sortVars = TRUE)

# Análisis de outliers en variables numéricas
numeric_vars <- sapply(datos, is.numeric)
par(mfrow = c(2, 3))
for(var in names(datos)[numeric_vars][1:6]) {
  boxplot(datos[[var]], main = paste("Boxplot:", var))
}
par(mfrow = c(1, 1))


# Pre-procesamiento de datos ----------------------------------------------
# Estrategia de imputación basada en contexto de negocio
datos_clean <- datos

# Imputar missing values de forma inteligente
# Ej: experiencia_conductor faltante -> imputar con mediana por tipo_vehiculo
datos_clean <- datos_clean %>%
  group_by(tipo_vehiculo) %>%
  mutate(
    experiencia_conductor = ifelse(
      is.na(experiencia_conductor),
      median(experiencia_conductor, na.rm = TRUE),
      experiencia_conductor
    )
  ) %>%
  ungroup() %>%
  mutate(
    # Clima faltante -> imputar "despejado" (más conservador)
    clima = ifelse(is.na(clima), "despejado", clima),
    
    # Tráfico faltante -> imputar con mediana general
    trafico_estimado = ifelse(
      is.na(trafico_estimado),
      median(trafico_estimado, na.rm = TRUE),
      trafico_estimado
    ),
    
    # Valor declarado faltante -> imputar con mediana general
    valor_declarado = ifelse(
      is.na(valor_declarado),
      median(valor_declarado, na.rm = TRUE),
      valor_declarado
    )
  )

# Verificar que no queden NAs
print(colSums(is.na(datos_clean)))

# Si aún quedan NAs, eliminar esas filas como último recurso
if(any(is.na(datos_clean))) {
  filas_antes <- nrow(datos_clean)
  datos_clean <- na.omit(datos_clean)
  print(paste("Filas eliminadas por NAs restantes:", filas_antes - nrow(datos_clean)))
}

# Feature Engineering con conocimiento del dominio
datos_clean <- datos_clean %>%
  mutate(
    # Densidad de carga (peso/volumen) - importante para eficiencia
    densidad_carga = ifelse(volumen > 0, peso / volumen, 0),
    
    # Es fin de semana (más tráfico, menos personal)
    es_fin_semana = dia_semana %in% c("sabado", "domingo"),
    
    # Es temporada alta (diciembre-febrero)
    es_temporada_alta = mes %in% c("diciembre", "enero", "febrero"),
    
    # Distancia larga (>500km requiere más planificación)
    distancia_larga = distancia > 500,
    
    # Conductor experimentado (>5 años)
    conductor_experimentado = experiencia_conductor > 5,
    
    # Categorizar valor del envío
    categoria_valor = case_when(
      valor_declarado < 50000 ~ "bajo",
      valor_declarado < 200000 ~ "medio",
      TRUE ~ "alto"
    )
  )

# Convertir variables categóricas apropiadamente
categorical_vars <- c("zona", "tipo_producto", "tipo_vehiculo", "clima", "categoria_valor")
datos_clean[categorical_vars] <- lapply(datos_clean[categorical_vars], as.factor)

# Convertir variable objetivo a factor
datos_clean$retraso <- as.factor(datos_clean$retraso)


# Split de los datos ---------------------------------------------------

# OPCIÓN: Usar muestra más pequeña si hay problemas de memoria
# Activando muestra pequeña para resolver problema de memoria:
set.seed(123)
sample_size <- min(10000, nrow(datos_clean))  # Máximo 10k filas
sample_indices <- sample(nrow(datos_clean), sample_size)
datos_clean <- datos_clean[sample_indices, ]
cat("Usando muestra de", nrow(datos_clean), "filas para optimizar memoria\n")

# División estratificada para mantener proporción de retrasos
trainIndex <- createDataPartition(
  datos_clean$retraso, 
  p = 0.7,                    # 70% entrenamiento
  list = FALSE,
  times = 1
)

datos_train <- datos_clean[trainIndex, ]
datos_temp <- datos_clean[-trainIndex, ]

# Dividir el 30% restante en validation (15%) y test (15%)
validationIndex <- createDataPartition(
  datos_temp$retraso,
  p = 0.5,                    # 50% del 30% = 15% del total
  list = FALSE,
  times = 1
)

datos_validation <- datos_temp[validationIndex, ]
datos_test <- datos_temp[-validationIndex, ]

# Verificar distribución en cada conjunto
cat("Distribución en Train:", prop.table(table(datos_train$retraso)))
cat("Distribución en Validation:", prop.table(table(datos_validation$retraso)))
cat("Distribución en Test:", prop.table(table(datos_test$retraso)))


# Training y Seleccion de Modelos -----------------------------------------

# Configurar validación cruzada (optimizada para memoria)
ctrl <- trainControl(
  method = "cv",
  number = 3,                 # Reducido de 5 a 3 para memoria
  classProbs = TRUE,          # Necesario para ROC
  summaryFunction = twoClassSummary,
  sampling = "down"           # Cambio de SMOTE a downsampling (menos memoria)
)

# Grid de hiperparámetros para Random Forest (reducido para memoria)
rf_grid <- expand.grid(
  mtry = c(3, 5)  # Reducido para memoria
)

# Entrenar Random Forest con CV (ultra-optimizado para memoria)
rf_model <- train(
  retraso ~ .,
  data = datos_train,
  method = "rf",
  tuneGrid = rf_grid,
  trControl = ctrl,
  metric = "ROC",             # Usar AUC como métrica
  ntree = 50,                 # Reducido aún más para memoria
  importance = TRUE,
  nodesize = 20,              # Nodos aún más grandes
  maxnodes = 100              # Límite máximo de nodos por árbol
)

# Grid para SVM
svm_grid <- expand.grid(
  C = c(0.1, 1, 10),
  sigma = c(0.01, 0.1, 1)
)

# Entrenar SVM con CV
svm_model <- train(
  retraso ~ .,
  data = datos_train,
  method = "svmRadial",
  tuneGrid = svm_grid,
  trControl = ctrl,
  metric = "ROC",
  preProcess = c("center", "scale")  # Importante para SVM
)

# Comparar modelos en validation set
models_list <- list(
  RandomForest = rf_model,
  SVM = svm_model
)

# Evaluar en conjunto de validación
validation_results <- data.frame(
  Model = character(),
  AUC = numeric(),
  Accuracy = numeric(),
  Sensitivity = numeric(),
  Specificity = numeric(),
  stringsAsFactors = FALSE
)

for(i in names(models_list)) {
  # Predicciones
  pred_prob <- predict(models_list[[i]], datos_validation, type = "prob")[,"Si"]
  pred_class <- predict(models_list[[i]], datos_validation)
  
  # Métricas
  roc_obj <- roc(datos_validation$retraso, pred_prob)
  cm <- confusionMatrix(pred_class, datos_validation$retraso, positive = "Si")
  
  validation_results <- rbind(validation_results, data.frame(
    Model = i,
    AUC = as.numeric(auc(roc_obj)),
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = cm$byClass["Sensitivity"],
    Specificity = cm$byClass["Specificity"]
  ))
}

print(validation_results)



# Selección de modelo -----------------------------------------------------
best_model_name <- validation_results$Model[which.max(validation_results$AUC)]
best_model <- models_list[[best_model_name]]

cat("Mejor modelo seleccionado:", best_model_name)
cat("AUC en validation:", max(validation_results$AUC))


# Evaluación final (Test Set) ---------------------------------------------
# Predicciones finales
final_pred_prob <- predict(best_model, datos_test, type = "prob")[,"Si"]
final_pred_class <- predict(best_model, datos_test)

# Matriz de confusión final
final_cm <- confusionMatrix(final_pred_class, datos_test$retraso, positive = "Si")
print(final_cm)

# Curva ROC final
final_roc <- roc(datos_test$retraso, final_pred_prob)
plot(final_roc, main = paste("Curva ROC Final -", best_model_name, 
                             "\nAUC =", round(auc(final_roc), 3)))



# Interpretabilidad -------------------------------------------------------

if(best_model_name == "RandomForest") {
  var_imp <- varImp(best_model)
  plot(var_imp, main = "Importancia de Variables - Random Forest")
  
  # Top 10 variables más importantes
  importance_scores <- var_imp$importance
  top_vars <- head(importance_scores[order(importance_scores$Overall, decreasing = TRUE), , drop = FALSE], 10)
  print("Top 10 variables más importantes:")
  print(top_vars)
}



# Consideraciones: --------------------------------------------------------
# Análisis de umbrales para optimizar según costo de negocio
thresholds <- seq(0.1, 0.9, by = 0.1)
threshold_analysis <- data.frame(
  Threshold = thresholds,
  TP = numeric(length(thresholds)),
  FP = numeric(length(thresholds)),
  TN = numeric(length(thresholds)),
  FN = numeric(length(thresholds)),
  Precision = numeric(length(thresholds)),
  Recall = numeric(thresholds),
  Business_Cost = numeric(length(thresholds))
)

# Costos de negocio estimados (en miles de pesos)
cost_FN <- 150  # Costo de no predecir retraso (cliente insatisfecho, compensación)
cost_FP <- 50   # Costo de falsa alarma (recursos de contingencia innecesarios)

for(i in 1:length(thresholds)) {
  thresh <- thresholds[i]
  pred_thresh <- ifelse(final_pred_prob > thresh, "Si", "No")
  pred_thresh <- factor(pred_thresh, levels = c("No", "Si"))
  
  cm_thresh <- confusionMatrix(pred_thresh, datos_test$retraso, positive = "Si")
  
  threshold_analysis$TP[i] <- cm_thresh$table[2,2]
  threshold_analysis$FP[i] <- cm_thresh$table[2,1]
  threshold_analysis$TN[i] <- cm_thresh$table[1,1]
  threshold_analysis$FN[i] <- cm_thresh$table[1,2]
  threshold_analysis$Precision[i] <- cm_thresh$byClass["Precision"]
  threshold_analysis$Recall[i] <- cm_thresh$byClass["Sensitivity"]
  
  # Costo total de negocio
  threshold_analysis$Business_Cost[i] <- 
    threshold_analysis$FN[i] * cost_FN + threshold_analysis$FP[i] * cost_FP
}

# Umbral óptimo según costo de negocio
optimal_threshold <- thresholds[which.min(threshold_analysis$Business_Cost)]
cat("Umbral óptimo según costo de negocio:", optimal_threshold)
cat("Costo mínimo estimado:", min(threshold_analysis$Business_Cost), "miles de pesos\n")



# Guardar los resultados --------------------------------------------------
# Guardar modelo final
saveRDS(best_model, "Datos/modelo_retrasos_final.rds")

# Guardar métricas y análisis
write.csv(validation_results, "Datos/comparacion_modelos.csv", row.names = FALSE)
write.csv(threshold_analysis, "Datos/analisis_umbrales.csv", row.names = FALSE)
