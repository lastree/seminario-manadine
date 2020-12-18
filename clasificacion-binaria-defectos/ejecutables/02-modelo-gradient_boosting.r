#install.packages('xgboost')
#install.packages('caret')
#install.packages('e1071')
#install.packages('pROC')
#install.packages('Ckmeans.1d.dp')
#install.packages('ggplot2')
#install.packages('DiagrammeR')

library(xgboost)
library(caret)
library(e1071)
library(pROC)
library(Ckmeans.1d.dp)
library(ggplot2)
library(DiagrammeR)

# Lectura del fichero .csv obtenido
data <- read.csv(???, sep = ???, header = ???)
head(data)

# Primera partición: Obtención del conjunto de test

# Pista: la librería caret tiene funciones maravillosas para partir conjuntos de datos preservando la distribución de las clases y para hacer downsampling ^^
df_test <- ?
df_train <- ?
df_valid <- ?

 Tamaño final de los conjuntos.
cat('Tamaño del conjunto de entrenamiento:', nrow(df_train), '\n')
cat('Tamaño del conjunto de validación:', nrow(df_valid), '\n')
cat('Tamaño del conjunto de test:', nrow(df_test))

# Separamos de los conjuntos la variable objeto de estudio
# Convertir en matrices, ya que la función para entrenar el modelo XGB no permite utilizar dataframes
X_train <- as.matrix(df_train[, which(names(df_train) != ???)])
y_train <- df_train[, ???]

X_valid <- as.matrix(df_valid[, which(names(df_valid) != ???)])
y_valid <- df_valid[, ???]

X_test <- as.matrix(df_test[, which(names(df_test) != ???)])
y_test <- df_test[, ???]

params <- list(objetive = 'binary:logistic',
               nthread = 4,
               max_depth = 6,
               eta = 0.3,
               eval_metric = 'auc')

xgb_model <- xgboost(data = ???, label = ???, params = ???, nrounds = 30, verbose = 1, seed = 0)

# Importancia de cada variable en el modelo
feature_importance <- xgb.importance(feature_names = ???, model = ???)
feature_importance

# Gráfico con la importancia
options(repr.plot.width = 6, repr.plot.height = 3.5)
xgb.ggplot.importance(importance_matrix = feature_importance, rel_to_first = TRUE)

# Ajuste/Rendimiento sobre datos de entrenamiento.
pred_train <- predict(xgb_model, ???)

# Rendimiento sobre datos de validación
pred_valid <- predict(xgb_model, ???)

# Creamos un dataframe con la probabilidad de ser o no defectuosa la pieza
# y añadimos el cálculo de la predicción final

# Entrenamiento
predictions_train <- data.frame('probability' = pred_train, 
                                'prediction' = ???)

# Validación
predictions_valid <- data.frame('probability' = pred_valid, 
                                'prediction' = ???)

# Cálculo de AUCs

# Entrenamiento
roc_train <- roc(???, predictions_train$prediction)
auc_train <- round(auc(roc_train), 4)

# Validación
roc_valid <- roc(???, predictions_valid$prediction)
auc_valid <- round(auc(roc_valid), 4)

cat(paste('Área bajo la curva (AUC) en entrenamiento:', auc_train, '\n'))
cat(paste('Área bajo la curva (AUC) en validación:', auc_valid))

# Bucle for para obtener las AUCs del modelo según nº de variables
auc_features <- data.frame('num_variables' = numeric(), 'auc_train' = numeric(), 'auc_valid' = numeric())

for (i in 1:???){
    cat('Entrenamiento del modelo con ', i, ' variables.\n')
    cat('================================================')
    
    # Reducción de conjuntos
    cols <- c(feature_importance[1:i, '???'])$???
    X_train_red <- as.matrix(X_train[, cols])
    X_valid_red <- as.matrix(X_valid[, cols])
    
    # Ajuste del modelo y obtención de predicciones
    model_red <- xgboost(data = X_train_red, label = y_train, params = params, nrounds = 30,
                         verbose = 0, seed = 0)
    
    pred_train <- predict(model_red, X_train_red)
    pred_train <- ifelse(pred_train > 0.5, 1, 0)
    
    pred_valid <- predict(model_red, X_valid_red)
    pred_valid <- ifelse(pred_valid > 0.5, 1, 0)
    
    # AUCs
    roc_train <- roc(y_train, pred_train)
    auc_train <- round(auc(roc_train), 4)
    
    roc_valid <- roc(y_valid, pred_valid)
    auc_valid <- round(auc(roc_valid), 4)
    
    # Añadimos la información al dataframe
    auc_features[i, 'num_variables'] <- i
    auc_features[i, 'auc_train'] <- auc_train
    auc_features[i, 'auc_valid'] <- auc_valid
}

# Dataframe que contiene la información del proceso iterativo
auc_features

# Gráfico que permite visualizar el AUC de los conjuntos según el nº de variables utilizadas
options(repr.plot.width = 6, repr.plot.height = 3.5)

p <- ggplot(data = ???)
p <- p + geom_line(aes(x = ???, y = ???, colour = 'Entrenamiento'))
p <- p + geom_line(aes(x = ???, y = ???, colour = 'Validación'))
p <- p + theme_bw()
p <- p + theme(plot.title = element_text(size = 12, hjust = 0.5, face = "bold", color= "grey20"))
p <- p + labs(title = 'Gradient Boosting AUC según nº de variables')
p <- p + scale_x_continuous('Número de variables', breaks = seq(1, nrow(auc_features), 1))
p <- p + scale_y_continuous('AUC')
p <- p + scale_color_manual('', breaks = c('Entrenamiento', 'Validación'), 
                             values = c('Entrenamiento' = 'steelblue', 'Validación' = 'darkorange'))
p

# Selección de las variables a utilizar
cols <- c(feature_importance[1:???, '???'])$???

# Reducción de los conjuntos
X_train_red <- X_train[, ???]
X_valid_red <- X_valid[, ???]
X_test_red <- X_test[, ???]

# Nuevo modelo con la reducción de variables
xgb_model_red <- xgboost(data = ???, label = ???, params = ???, nrounds = 30,
                         verbose = 0, seed = 0)

# Rendimiento del modelo en conjuntos de entrenamiento y validación (AUCs)

# Entrenamiento
pred_train <- predict(xgb_model_red, X_train_red)
pred_train <- ifelse(pred_train > 0.5, 1, 0)

roc_train <- roc(y_train, pred_train)
auc_train <- round(auc(roc_train), 4)

# Validación
pred_valid <- predict(xgb_model_red, X_valid_red)
pred_valid <- ifelse(pred_valid > 0.5, 1, 0)

roc_valid <- roc(y_valid, pred_valid)
auc_valid <- round(auc(roc_valid), 4)

# Resultados
cat('Área bajo la curva (AUC) en entrenamiento:', auc_train, '\n')
cat('Área bajo la curva (AUC) en validación:', auc_valid)

# Cambio en los valores de los hiperparámetros
best_params <- list(objetive = 'binary:logistic',
                    nthread = 4,
                    max_depth = ???,
                    eta = ???,
                    eval_metric = 'auc')

# Entrenamiento del modelo con los nuevos valores
best_xgb_model <- xgboost(data = X_train_red, label = y_train, params = best_params, nrounds = ???,
                          verbose = 0, seed = 0)

# Rendimiento del nuevo modelo en los conjuntos de entrenamiento y validación
# Entrenamiento
new_pred_train <- predict(best_xgb_model, X_train_red)
new_pred_train <- ifelse(new_pred_train > 0.5, 1, 0)

new_roc_train <- roc(y_train, new_pred_train)
new_auc_train <- round(auc(new_roc_train), 4)

# Validación
new_pred_valid <- predict(best_xgb_model, X_valid_red)
new_pred_valid <- ifelse(new_pred_valid > 0.5, 1, 0)

new_roc_valid <- roc(y_valid, new_pred_valid)
new_auc_valid <- round(auc(new_roc_valid), 4)

# Resultados
cat('Nuevo área bajo la curva (AUC) en entrenamiento:', new_auc_train, '\n')
cat('Nuevo área bajo la curva (AUC) en validación:', new_auc_valid)

# Predicciones en conjunto de test
pred_test <- predict(best_xgb_model, ???)
pred_test <- ???

# Área bajo la curva
roc_test <- roc(???, pred_test)
auc_test <- round(auc(roc_test), 4)

cat('Área bajo la curva (AUC) en test:', auc_test)

# Matriz de confusión
cm_test <- ???

cat('Matriz de confusión para datos de test:\n\n')
print(cm_test)

# Otra manera de obtener la matriz de confusión, junto con otras métricas
cm_test2 <- confusionMatrix(as.factor(???), as.factor(???), mode = 'everything',
                            positive = '1')
cm_test2

# IMPORTANTE!!: Tiempo de ejecución muy largo. Evitar ejecutar
xgb.plot.tree(feature_names = best_xgb_model$feature_names, model = best_xgb_model, trees = 0)

# Guardado del modelo ya entrenado
xgb.save(???, ???)
