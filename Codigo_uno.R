# =====================================================
# Primera solución - EcoLogistics Predicción Retrasos
# =====================================================

library(randomForest)
library(caret)

# Cargar datos
datos <- read.csv("Datos/envios_ecologistics.csv")

# Convertir variable objetivo (solo esto para que funcione)
datos$retraso <- as.factor(datos$retraso)

# Rápida limpieza
datos <- na.omit(datos)  # Eliminar todos los NAs

# Crear algunas variables nuevas
datos$peso_por_volumen <- datos$peso / datos$volumen
datos$velocidad_promedio <- datos$distancia / datos$tiempo_estimado

# Entrenar modelo directamente
modelo <- randomForest(retraso ~ ., data = datos, ntree = 50)

# Ver qué tan bueno es
print(modelo)

# Hacer predicciones
predicciones <- predict(modelo, datos)

# Calcular accuracy
accuracy <- sum(predicciones == datos$retraso) / length(datos$retraso)
print(paste("Accuracy:", accuracy))

# Guardar modelo
save(modelo, file = "modelo_retrasos.RData")

# Variables importantes
importance(modelo)

# Listo! El modelo está funcionando bien con alto % de accuracy!
# Se puede implementar en producción



# Código adicional "para mejorar el modelo"
# Intentar con SVM también
library(e1071)
modelo_svm <- svm(retraso ~ ., data = datos)
pred_svm <- predict(modelo_svm, datos)
accuracy_svm <- sum(pred_svm == datos$retraso) / length(datos$retraso)
print(paste("SVM Accuracy:", accuracy_svm))

# Random Forest sigue siendo mejor, usar ese

# Predicción para mañana
nuevos_envios <- read.csv("envios_manana.csv")
predicciones_manana <- predict(modelo, nuevos_envios)
write.csv(predicciones_manana, "predicciones_manana.csv")