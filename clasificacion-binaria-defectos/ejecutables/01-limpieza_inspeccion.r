
#install.packages(?)

library(?)
library(?)

???? # Imprime directorio de trabajo
load('data/data.rdata') # Carga un fichero .rdata
????  # Muestra los objetos cargados en memoria 

# Tipo de objeto
cat("El fichero es un", ??????,"de", ??????, "filas y",??????, "columnas.")

??? # muestra las primeras filas

# Ausencias
?????(???)

options(repr.plot.width=8, repr.plot.height=2.5)  # cambiar el tamaño de los gráficos

for (i in 1:19){
    p <- ggplot(????)
    p <- p + geom_????(?????)   # histograma
    p <- p + geom_????(?????)   # aprox. densidad
    p <- p + scale_?????_continuous(name = ???????)   # para que aparezca el nombre de la variable en el eje 
    p <- p + ???????  # formato "limpio"
    print(p)  # incluir para que lo imprima por pantalla ^^
}

# Outliers
options(repr.plot.width=8, repr.plot.height=2.5)

for (i in ??????){
    p <- ggplot(??????)
    p <- p + geom_?????(?????)
    p <- p + scale_???????_continuous(??????)  # para que aparezca el nombre en el eje 
    p <- p + ???????  # formato "limpio"
    print(p)  # incluir para que lo imprima por pantalla ^^
}

# Si sospechas de que alguna variable toma casi siempre los mismos valores, asegúrate aquí:
?????(????????)

# Eliminamos ausencias
data <- ????? 
cat("El fichero es un", ??????,"de", ??????, "filas y",??????, "columnas.")

# Eliminamos variables con baja varianza (quimico9)
sd.vector <- ?????  # calcula la sd para cada variable
data <- ??????   # elimina aquellas que tengan sd<0.005

# Guarda tus resultados en csv
write.csv(??????)