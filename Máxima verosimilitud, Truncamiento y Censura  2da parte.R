# UNIVERSIDAD DE COSTA RICA
# ESCUELA DE ECONOMIA

# MICROECONOMETRIA
#Tarea 2
#Estudiantes: B75115 y B76137
#Segunda Parte
install.packages(c("readxl","dplyr","ggplot2", "plotly","tidyr","scales","aod","wooldridge")) #Para varios paquetes, ENTRE COMILLAS
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)
library(aod)
library(wooldridge)
library(haven)
library(npsf)
library(censReg)
#Cargue la base
data("mroz")
?mroz
Mroz <- load("C:/Users/David Mora Salazar/Documents/ECONOMÍA UNIVERSIDAD DE COSTA RICA/Microeconometría/Tarea 2/mroz.RData")
#1: Regresión de MCO
Modelo<- lm(formula=hours~educ+exper+expersq+age+kidslt6+kidsge6,data=data)
summary(Modelo)
#3.a Histograma de frecuencias
ggplot(data, aes(x=hours)) + geom_histogram( fill="#69b3a2", color="#e9ecef")+
  ggtitle("Histograma de frecuencia de hours")  +
  theme(
    plot.title = element_text(size=15)
  )
#3.b Histograma de densidad
ggplot(data=data,aes(hours))+geom_histogram(aes(y = ..density..),bins=3)+stat_function(fun = dnorm, args = list(mean = mean(data$hours), sd = sd(data$hours)))

#4.a Histograma de frecuencias
data_trunc<- data %>% filter(`hours`>0)
ggplot(data_trunc, aes(x=hours)) + geom_histogram( fill="#69b3a2", color="#e9ecef") + 
  scale_x_continuous(breaks = seq(1, 5000, by = 1000))+ ggtitle("Histograma de frecuencia de hours truncado")  +
  theme(
    plot.title = element_text(size=15)
  )
#4.b Histograma de densidad
ggplot(data=data_trunc,aes(hours))+geom_histogram(aes(y = ..density..),bins=3)+stat_function(fun = dnorm, args = list(mean = mean(data_trunc$hours), sd = sd(data_trunc$hours)))

#5
Modelo_trunc<-npsf::truncreg(formula=hours~educ+exper+expersq+age+kidslt6+kidsge6,data=data,ll=0, marg.eff=TRUE)
#Efectos marginales: #promedio
summary(Modelo_trunc$marg.effects)
#Efectos marginales: sobre la media
#Primero se saca la media de las variables:
mean_educ <- mean(data_trunc$educ)
mean_exper <- mean(data_trunc$exper)
mean_expersq <- mean(data_trunc$expersq)
mean_age <- mean(data_trunc$age)
mean_kidslt6 <- mean(data_trunc$kidslt6)
mean_kidsge6 <- mean(data_trunc$kidsge6)
#Multiplicamos la media de la variable por el coeficiente:
medias <- list("mean_c" = 1, "mean_educ" = mean_educ, "mean_exper" = mean_exper, "mean_expersq" = mean_expersq, "mean_age" = mean_age, "mean_kidslt6" = mean_kidslt6, "mean_kidsge6" = mean_kidsge6)
coeficientes <- list("c" = 2122.06922,"coef_educ" = -29.65101, "coef_exper" = 72.60832, "coef_expersq" = -0.94513, "coef_age" = -27.39191, "coef_kidslt6" = -484.85903, "coef_kidsge6" = -102.59511)
mediasxcoeficientes <- list(unlist(medias)*unlist(coeficientes))
suma_mediasxcoeficientes <- 1115.281
sigma <- 850.77423
#Se saca el alpha:
alpha <- (-1115.281)/850.77423
#Se saca phi minúscula y phi de alpha de la tabla normal:
phi_min_alpha <- dnorm(alpha, mean = 0, sd=1)
phi_alpha  <- pnorm(alpha, mean = 0, sd=1)
#Se saca Lambda:
Lambda <- phi_min_alpha/(1-phi_alpha)
#Se saca delta:
delta <- Lambda*(Lambda-alpha)
uno_menos_delta <- 1-delta
#Se sacan los efectos marginales sobre la media
eff_marg_media <- lapply(coeficientes, "*",uno_menos_delta)