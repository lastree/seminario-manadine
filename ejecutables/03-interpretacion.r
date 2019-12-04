
#install.packages("SHAPforxgboost")
#install.packages("Ckmeans.1d.dp")  -> caprichín para gráficos
#install.packages("patchwork")  -> caprichín para gráficos 

library(xgboost)   # la necesitamos para cargar el modelo entrenado
library(SHAPforxgboost)
library(ggplot2)
library(patchwork)

modelo <- ?????("modelo_entrenado")
data <- read.csv("data/dataPrepared.csv")
X <-  ?????? 
y <- ?????

## Cáculo de la importancia de xgboost
options(repr.plot.width=8, repr.plot.height=3.5)
importance.matrix <- ?????????
p1 <- xgb.ggplot.importance(?????, rel_to_first = TRUE)
p2 <- xgb.ggplot.importance(?????, measure = 'Frequency', rel_to_first = TRUE)
???????

# Obtener los SHAP values x
shap.values <- ????(????, ????)
?????(shap.values)
# Ranking de variables según mean |SHAP|
print(?????)

shap.data <- ?????
shap.sum <- ?????(shap.data)
y_pred <- predict(modelo, ??????)

???????

options(repr.plot.width=8, repr.plot.height=4.5)
shap.plot.summary.wrap1(?????, ?????)

# Preparamos los datos en el formato que necesita
shap.long <- shap.prep(??????, ??????)
shap.plot.summary(??????, x_bound  = ????, dilute = ????)