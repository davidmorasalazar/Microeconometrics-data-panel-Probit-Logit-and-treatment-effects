#TERCERA PARTE
library(survival)
library(mclogit)
library(margins)
library(mlogit)

#1. Características
#La base de datos deben de contener variables que muestren la caracteristica
#de elección del individuo para realizar un logit o probit condicional.
#Esto para obtener la utiidad de escogencia de la variable dependiente dado a sus
#caractericas, omitiendo la relevancia de alternativas irrelevantes.
#el modelo probit condicional a diferencia de un modelo multinomial, es 
#estimado usando un único vector de parámetros beta. Estos betas, a diferencia
#de un modelo logit si se interpretan.

cereales <- read.csv("C:/Users/Man14/Desktop/Economía/Microeconometría/Tareas/3/cereales.csv")

#2. Regresión Logit condicional

cereal_clogit<-clogit(eleccion~grasa+azucar+vitaminas+proteina+calorias+
                        tazas+fibra+carbo+
                        strata(individuos),data = cereales)
cereal_clogit

#Los resultados para las características escogidas no se muestran significativas
#para explicar la eleccion del individuo sobre que cereal consumir. De todas formas,
#si interpretamos los coeficiente diríamos por ejemplo que el nivel de vitaminas tiene efecto positivo
#sobre la eleción de un cereal con una utilidad marginal de 0.07 mientras que la grasa 
#y el azucar tienen el efecto contrario, con utilidad marginal negativa en magnitud de -0.3 y
#-0.06 respectivamente.

#3. Probabilidades predichas

prob_clogit<-predict(cereal_clogit,type="expected")
prob_clogit 

#En nuestra base de datos tenemos un total de 12 individuos que deciden que cereal
#tomar. Vemos que el primer individuo tiene un 0.38 de probabilidad de seleccionar
#cheerios, un 0.20 de elegir Froot Loops, 0.35 de Rice Krispies y un 0.066 de Special K.
#Para el segundo individuo, escoge Cheerios con 0.54 de probabilidad, toma Froot Loops
#con 0.065, Rice Krispies con 0.29 y con probabilidad de 0.098 elije Special K.
#Para un tercer individuo, este elije Cheerios con 0.051 de probabilidad, Froot Loops con
#0.56, Rice Krispies con 0.15 y con 0.22 de probailidad selecciona Special K.




#4. Efectos marginales promedios
cereales_mclogit<-mclogit(cbind(eleccion,individuos)~grasa+azucar+vitaminas+proteina+calorias+
                            tazas+fibra+carbo,data=cereales)
#removing specialk

mpc_mclogit<-margins(cereales_mclogit)

mpc_mclogit
summary(mpc_mclogit)

#Se continua obteniendo resultados no significativos, por lo que ninguna 
#caracteristica cambia la probabilidad de escoger entre un cereal u otro.
#Si así lo fuese, se diría que el azucar y la grasa en promedio disminuyen 
#la probabilidad de elegir un cereal en 0.74 y 3.6 puntos porcentuales respectivamente, 
#mientras que las vitaminas tendrían efecto contrario de 0.93 pp.




#5. Efecto marginales en las medias
medias<-colMeans(cereales[, c("grasa", "azucar", "vitaminas", "carbo", "fibra",
                              "proteina", "tazas", "calorias")])
(medias)
EM_medias_clogit<-margins(cereales_mclogit,
                          at=list(grasa=medias["grasa"],
                                  azucar=medias["azucar"],
                                  carbo=medias["carbo"],
                                  fibra=medias["fibra"],
                                  proteina=medias["proteina"],
                                  tazas=medias["tazas"],
                                  vitaminas=medias["vitaminas"]))

summary(EM_medias_clogit)
EM_medias_clogit

#No se obtienen resultados significativso sobre este modelo por lo que no
#se puede concluir sobre las caracteristicas en la probabilidad de elección
#de un individuo promedio.Si así fuera se tendría que un aumento de una unidad 
#en grasa y azucar tendría un aumento de probabilidad de elegir un cereal en 
#3.62 y en 0.74 puntos porcentuales respectivamente para un individuo promedio.
#Mientras que un aumento en una unidad en vitaminas aumentaria la probabilidad
#de escogencia en 0.93 puntos porcentuales para el individuo promedio.


#6. Prueba de Hausmann
#Se toma el supuesto que existe independencia en razón de probabilidad de las variables}
#por lo que estas no se ven afectadas si se considera una opción alternativa.
#Si las probabilidades cambian con un aumento de alternativas entonces no existe
#independencia de alternativas irrelevantes. La prueba de Hausman muestra
#si existe o no esta condición. Es importante utilizarla la prueba en este
#y modelos de logit condional para asegurarse que no cuenta con este sesgo y que
# y que la incorporacion de más opciones no afectaria de manera significativa
#la elección de un individuo.


#7. Prueba de Hausman




B<-as.vector(cereales_mclogit$coefficients)
B 
VB<-as.matrix(cereales_mclogit$covmat)
VB 


cereales1<-cereales[which(cereales$identif != 1),]
cereal_mclogitrestr<-mclogit(cbind(eleccion,individuos)~grasa+azucar+vitaminas+proteina+calorias+
                               tazas+fibra+carbo,data=cereales1)
b<-as.vector(cereal_mclogitrestr$coefficients)
Vb<-as.matrix(cereal_mclogitrestr$covmat)


B1<-B
B1
VB1<-VB
VB1

H<-t(b-B1)%*%solve(Vb-VB1)%*%(b-B1)
H




pvalueH<-1-pchisq(H,8)
pvalueH

#El H calculado resulta mayor que pchisq con k=8 por lo que hay evidencia
#para rechazar la hipotesis nula de que haya independencia de alternativas
#irrelevantes. Por tanto, se recomienda utilizar un modelo logit anidado para este caso
