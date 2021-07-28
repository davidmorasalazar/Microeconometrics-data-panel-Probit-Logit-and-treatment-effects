install.packages("HSAUR")

library(HSAUR)
names(plasma)
dim(plasma)
(plasma)
library(margins)



#PRIMERA PARTE
#1. Regresión Logit

logit_plasma <- glm(ESR ~fibrinogen + globulin,
                   family=binomial(link='logit'),data=plasma)

logit_plasma
summary(logit_plasma)

#Los resultados muestran una relación positiva de odd ratio 
#del ESR ante cambios en las proteínas.
#Vemos que en este resultado solo el fibrinogen se considera significativa a 5% de 
#significancia.
#La interpretación de esta regresión solo se basa en la dirección del efecto,
#pero no en su magnitud.



#2. Efecto marginal promedio
EMprom_plasma<-margins(logit_plasma)
summary(EMprom_plasma)
EMprom_plasma

#En este caso se mide si el nivel del ESR es mayor a 20. Este resultado muestra 
#que ante un aumento de una unidad de fibrinogen, la probailidad que el nivel de 
#ESR sea mayor a 20 aumenta en promedio 20.99 puntos porcentuales.

#3. Efecto marginal en las medias

medias<-colMeans(plasma[, c("globulin", "fibrinogen")])
(medias)
(plasma(globulin))
plasma[, c("globulin", "fibrinogen")]
EM_medias_logit<-margins(logit_plasma, at=list(fibrinogen=medias["fibrinogen"],
                                              globulin=medias["globulin"]))

summary(EM_medias_logit)
EM_medias_logit
# Los resultados muestran que un aumento en una unidad de fibrinogen aumenta la 
#probabilidad de ESR mayor a 20 en 21.46 puntos porcentuales para un individuo 
#promedio.

#4. tabla de clasificación
plogit <- predict(logit_plasma, type = "response")
plogit

table(plasma$ESR, plogit>0.5)

#con la tabla obtenemos que la exactitud es 0.785
(26+2)/32
#sensibilidad = 0.867
26/30
#La especificidad =1
2/2



#5. Regresión probit
plasma_probit <- glm(ESR ~globulin + fibrinogen,
                    family=binomial(link='probit'),data=plasma)

summary(plasma_probit)
#Los resultados son similares a los de la regresión logit,
#se continua observando al fibrinogen como variable significativa para la variable
#dependiente, Igualmente se observa las direcciones positivas de los efectos, pero
#no la magnitud de sus coeficientes.

#6. EM Promedio
EMprom_probit_plasma<-margins(plasma_probit)
EMprom_probit_plasma
summary(EMprom_probit_plasma)

#Se tiene que un aumento en una unidad de fibrinogen aumenta en promedio la
#probabilidad de ESR>20 en 21.17 puntos porcentuales.


#7. EM en la media
EM_medias_probit_plasma<-margins(plasma_probit, at=list(globulin=medias["globulin"],
                                                        fibrinogen=medias["fibrinogen"]))

EM_medias_probit_plasma

summary(EM_medias_probit_plasma)
#Se muestra que un aumento de una uniad de fibronogen hace que la probabilidad
# de obtener ESR>20 aumento en 23.3 puntos porcentuales para el individuo promedio.


#8. Resultados
#Según los resultados obtenidos hay evidencia para decir que existe relación positiva
#entre la probabilidad de lectura de un ESR>20 y el nivel de Fibrinogen. 
#Sin embargo, no se encuentra evidencia de que el coeficiente para globulin sea 
#diferente de cero, por lo que no hay relación con una lectura de ESR>20. Sin embargo
#en la literatura existe indicaciones que si hay relación de las proteinas
#y el aumento de ESR por lo que se podría cuestionar las pocas observaciones en esta
#base de datos (29 obs) y también que aquellas que muestras ESR>20 con pocas (6/29 obs)





