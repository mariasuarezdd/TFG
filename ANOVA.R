# Comando para limpiar los datos del entorno
rm(list=ls())
# Lectura de los datos del fichero datos.txt
datos=read.table(file = "datos.txt", header=T, sep="", dec=",")
attach(datos)

# Se declaran las variables numéricas X1, X2 y X3 del fichero de datos como factores
x1=factor(X1)
x2=factor(X2)
x3=factor(X3)

# Se definen los modelos mediante la función aov y se ejecutan los análisis llamando a la función anova
mod<-aov(I1~x1*x2*x3)
anova(mod)
mod2<-aov(I2~x1*x2*x3)
anova(mod2)
mod3<-aov(I3~x1*x2*x3)
anova(mod3)
mod4<-aov(I4~x1*x2*x3)
anova(mod4)