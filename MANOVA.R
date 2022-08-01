rm(list=ls())

# Lectura de los datos originales
datos=read.table(file = "datos.txt", header=T, sep="", dec=",")
names(datos)
attach(datos)
# Se define la nueva matriz de datos
datos_nuevos<-matrix(nrow=81,ncol=7)

# Mediante la función runif se generan dos valores adicionales I12 e I13 para I1
I12=runif(27)
I13=runif(27)
for (i in 1:27){
  I12[i]=I1[i]+rnorm(n=1,  mean=0,  sd=0.1)
  I13[i]=I1[i]+rnorm(n=1,  mean=0,  sd=0.1)
}

# Mediante la función runif se generan dos valores adicionales I22 e I23 para I2
I22=runif(27)
I23=runif(27)
for (i in 1:27){
  I22[i]=I2[i]+rnorm(n=1,  mean=0,  sd=0.1)
  I23[i]=I2[i]+rnorm(n=1,  mean=0,  sd=0.1)
}

# Mediante la función runif se generan dos valores adicionales I32 e I33 para I3
I32=runif(27)
I33=runif(27)
for (i in 1:27){
  I32[i]=I3[i]+rnorm(n=1,  mean=0,  sd=0.1)
  I33[i]=I3[i]+rnorm(n=1,  mean=0,  sd=0.1)
}

# Mediante la función runif se generan dos valores adicionales I42 e I43 para I4
I42=runif(27)
I43=runif(27, 11, 15)
for (i in 1:27){
  I42[i]=I4[i]+rnorm(n=1,  mean=0,  sd=0.1)
  I43[i]=I4[i]+rnorm(n=1,  mean=0,  sd=0.1)
}

# Se generan columnas intercalando los datos originales Ii con los generados Ii2 y Ii3
columnaI1<-numeric(length=81)
for (i in 1:27){
  columnaI1[3*i-2]<-c(I12[i])
  columnaI1[3*i-1]<-c(I1[i])
  columnaI1[3*i]<-c(I13[i])
}

columnaI2<-numeric(length=81)
for (i in 1:27){
  columnaI2[3*i-2]<-c(I22[i])
  columnaI2[3*i-1]<-c(I2[i])
  columnaI2[3*i]<-c(I23[i])
}

columnaI3<-numeric(length=81)
for (i in 1:27){
  columnaI3[3*i-2]<-c(I32[i])
  columnaI3[3*i-1]<-c(I3[i])
  columnaI3[3*i]<-c(I33[i])
}

columnaI4<-numeric(length=81)
for (i in 1:27){
  columnaI4[3*i-2]<-c(I42[i])
  columnaI4[3*i-1]<-c(I4[i])
  columnaI4[3*i]<-c(I43[i])
}

# Se importan los nuevos factores (filas originales triplicadas)
datos=read.table(file = "XN.txt", header=T, sep="", dec=",")
attach(datos)
cX1<-c(X1N)
cX2<-c(X2N)
cX3<-c(X3N)

# Se escribe la matriz datos_nuevos con los vectores almacenados para cada factor y respuesta
datosnuevos<-cbind(cX1,cX2,cX3, columnaI1, columnaI2, columnaI3, columnaI4)

# Se exportan los datos al fichero datos_manova.txt
write.table(datos_nuevos,"datos_manova.txt", sep="", dec=",")

# Análisis multivariante de la varianza considerando I1 e I2
x1=factor(cX1)
x2=factor(cX2)
x3=factor(cX3)
mod1<-manova(cbind(columnaI1,columnaI2)~x1*x2*x3)
mod11=summary(mod1)
mod11$SS

# Análisis multivariante de la varianza considerando I3 e I4
mod2<-manova(cbind(columnaI3,columnaI4)~x1*x2*x3)
mod22=summary(mod2)
mod22$SS