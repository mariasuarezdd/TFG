# Comando para limpiar los datos del entorno
rm(list=ls())
# Lectura de los datos del fichero datos.txt
datos=read.table(file = "datos.txt", header=T, sep="", dec=",")
attach(datos)
# Se carga la librería randomForest
library(randomForest)
# Definición de los modelos con variables numéricas. Se incorpora la variable importance como verdadera ya que se pretende evaluar importancia de las distintas variables
arbol.rf<-randomForest(I1~X1+X2+X3, ntree = 500, maxnodes = 1000, importance = TRUE)

# Se generan los resultados y las gráficas de importancia
print(arbol.rf)
importance(arbol.rf)
varImpPlot(arbol.rf)

# Mismo procedimiento para los tres casos restantes
arbol.rf2<-randomForest(I2~X1+X2+X3, ntree = 500, maxnodes = 1000, importance = TRUE)
print(arbol.rf2)
importance(arbol.rf2)
varImpPlot(arbol.rf2)
arbol.rf3<-randomForest(I3~X1+X2+X3, ntree = 500, maxnodes = 1000, importance = TRUE)
print(arbol.rf3)
importance(arbol.rf3)
varImpPlot(arbol.rf3)
arbol.rf4<-randomForest(I4~X1+X2+X3, ntree = 500, maxnodes = 1000, importance = TRUE)
print(arbol.rf4)
importance(arbol.rf4)
varImpPlot(arbol.rf4)

