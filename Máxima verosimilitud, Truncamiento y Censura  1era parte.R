# UNIVERSIDAD DE COSTA RICA
# ESCUELA DE ECONOMIA

# MICROECONOMETRIA
#Tarea 2
#Estudiantes: B75115 y B76137
#Primera Parte
install.packages(c("readxl","dplyr","ggplot2", "plotly","tidyr","scales","aod")) #Para varios paquetes, ENTRE COMILLAS

library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(aod) #Paquete que contiene funci�n para prueba de Wald
#PRIMERA PARTE
#1
Cobb_Douglas<- read_excel("C:/Users/David Mora Salazar/Documents/ECONOM�A UNIVERSIDAD DE COSTA RICA/Microeconometr�a/Tarea 2/Cobb Douglas.xlsx" ,sheet = "Hoja4")
Cobb_Douglas<-as.matrix(Cobb_Douglas[,2:4]) # Convertimos a matriz y quitamos la primera columna, que tiene un contador, 
Insumo_1<-Cobb_Douglas[,1]  # Insumo_1 est� en la primera columna
Insumo_2<-Cobb_Douglas[,2]  # Insumo_2 en la segunda
Producto<-Cobb_Douglas[,3]     # Producto en la tercera 
#2
mv_fprod<-function(alfa,Z1,Z2,Q) {
  n<-length(Z1)  #Número de observaciones
  e<-Q-alfa[1]*(Z1^alfa[2])*(Z2^alfa[3])   # Forma funcional de los residuos
  logl<- -0.5*n*log(2*pi)-0.5*n*log(alfa[4])-((t(e)%*%e)/(2*alfa[4])) # Función de verosimilitud normal evaluada en residuos
  return(-logl)  # Función da el negativo de la verosimilitud. Necesario para función de optimización "optim"
}
#Optimizaci�n
resultado<-optim(c(1,0.5,0.5,0.01),mv_fprod,method="BFGS",hessian=T,Z1=Insumo_1,Z2=Insumo_2,Q=Producto)

resultado   #Muestra todos los resultados del hessiano
#Valor de la m�xima verosimilitud
resultado$value  # Valor resultante para la función de verosimilitud
paste("La maxima verosimilitud es:", resultado$value)

#3
resultado$par   # Valores estimados para los parámetros mediante máxima verosimilitud

cuadro <- matrix(resultado$par,ncol=4,byrow=FALSE)
colnames(cuadro) <- c("alfa[1]","alfa[2]","alfa[3]","alfa[4] o varianza")
cuadro <- as.table(cuadro)
cuadro

#4
R<-cbind(0,1,1,0)  # Vector con coeficientes de la restricción:  debe tener tamaño del vector de coeficientes
V<-solve(resultado$hessian)  # Matriz de información:  inversa del Hessiano.  Como usamos -logl, no hace falta poner -H^-1
prueba_wald<-wald.test(b = resultado$par,Sigma =V,L=R,H0=1)  # H0 es que Rb = 1
prueba_wald$result

#5
mv_fprod_restr<-function(alfa,Z1,Z2,Q) {
  
  n<-length(Z1)  #Número de observaciones
  e<-Q-alfa[1]*(Z1^alfa[2])*(Z2^(1-alfa[2]))   # Se impone restricción de que exponentes sumen 1
  logl<- -0.5*n*log(2*pi)-0.5*n*log(alfa[4])-((t(e)%*%e)/(2*alfa[4])) # Función de verosimilitud normal evaluada en residuos
  return(-logl)  # Función da el negativo de la verosimilitud
}
#Para optimizar se utiliza la funcion optim
resultado_restr<-optim(c(1,0.5,0.5,0.01),mv_fprod_restr,method="BFGS",hessian=T,Z1=Insumo_1,Z2=Insumo_2,Q=Producto)


#La razon de verosimilitud es: -2*(ln(restringido)- ln(no restringido))
LRestad<- 2*(resultado$value - resultado_restr$value)*-1
pvalueLR<-1-pchisq(LRestad,1)  # Pvalue de Ji2 con 1 gl que se usa para comparar la raz�n de verosimilitud.

LRestad
pvalueLR

valores_rv <- c(LRestad,pvalueLR)
valores_rv
#Se ordenan los datos en una tabla
cuadro2 <- matrix(valores_rv,ncol=2,byrow=FALSE)
colnames(cuadro2) <- c("Razon de verosimilitud","Pvalue")
rownames(cuadro2) <- c("")
cuadro2 <- as.table(cuadro2)
cuadro2




