# UNIVERSIDAD DE COSTA RICA
# ESCUELA DE ECONOMIA

# MICROECONOMETRIA
#Tarea 1
#Estudiante: B75115
install.packages(c("readxl","dplyr","ggplot2", "plotly","tidyr","scales")) #Para varios paquetes, ENTRE COMILLAS

library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(scales)

#PRIMERA PARTE
#1
aprobados<- read_excel("C:/Users/David Mora Salazar/Documents/ECONOMÍA UNIVERSIDAD DE COSTA RICA/Microeconometría/Tarea 1/aprobados.xlsx" ,sheet = "BASE DE DATOS")
rendimientos<- read_excel("C:/Users/David Mora Salazar/Documents/ECONOMÍA UNIVERSIDAD DE COSTA RICA/Microeconometría/Tarea 1/rendimiento.xlsx" ,sheet = "BASE DE DATOS")

#2
aprobados_fil<- aprobados %>% filter(`CURSO LECTIVO`==2019)
aprobados_fil<- aprobados_fil[!is.na(aprobados_fil$CODIGO),] 
rendimientos_fil<- rendimientos %>% filter(`AÑO CURSADO`==2019)
rendimientos_fil<- rendimientos_fil[!is.na(rendimientos_fil$CODIGO),] 

#3
aprobados_fil<-aprobados_fil%>%select(`CURSO LECTIVO`,NOMBRE,PROVINCIA,ZONA,MFT,MFH,MFM,APT,APH,APM,MF1T,AP1T)
rendimientos_fil<-rendimientos_fil%>%select(`AÑO CURSADO`,NOMBRE,PROVINCIA,ZONA,APROBT,APROBH,APROBM,REPROT,REPROH,REPROBM)

#4
aprobados_fil<- aprobados_fil %>% rename(`TOTAL MATRICULA FINAL`=MFT,`HOMBRES MATRICULA FINAL`=MFH,`MUJERES MATRICULA FINAL`=MFM,`TOTAL APROBADOS MATRICULA FINAL`=APT,`TOTAL HOMBRES APROBADOS MATRICULA FINAL`=APH,`TOTAL MUJERES APROBADOS MATRICULA FINAL`=APM,`TOTAL 7 AÑO MATRICULA FINAL`=MF1T,`TOTAL 7 AÑO APROBADO`=AP1T)
rendimientos_fil<- rendimientos_fil %>% rename(`TOTAL APROBADOS`=APROBT,`TOTAL APROBADOS HOMBRES`=APROBH,`TOTAL APROBADOS MUJERES`=APROBM,`TOTAL REPROBADOS`=REPROT,`TOTAL REPROBADOS HOMBRES`=REPROH,`TOTAL REPROBADOS MUJERES`=REPROBM)

#Para este paso, es importante notar que contrario a lo que define la nomenclatura de la base de datos "rendimientos_fil
#, para el analista la variable ZONA es igual a 1 cuando el colegio se encuentra en una zona urbana e igual a 2 cuando está en una zona rural,
#por tanto, contrario a la nomenclatura, el analista procederá a efectuar el cambio"

aprobados_fil$ZONA[aprobados_fil$ZONA==1]<-"zona urbana"
aprobados_fil$ZONA[aprobados_fil$ZONA==2]<-"zona rural"
rendimientos_fil$ZONA[rendimientos_fil$ZONA==1]<-"zona urbana"
rendimientos_fil$ZONA[rendimientos_fil$ZONA==2]<-"zona rural"


#SEGUNDA PARTE
#1  

#El valor máximo del Total Matricula Final es:
max(aprobados_fil$`TOTAL MATRICULA FINAL`) 
#La posición en que se encuentra ese máximo es:
which.max(aprobados_fil$`TOTAL MATRICULA FINAL`)

#2
# Colegio con el máximo del Total Matricula Final:
aprobados_fil[which.max(aprobados_fil$`TOTAL MATRICULA FINAL`),]%>% select( NOMBRE, `TOTAL MATRICULA FINAL`)
# Colegio con el mínimo del Total Matricula Final:
aprobados_fil[which.min(aprobados_fil$`TOTAL MATRICULA FINAL`),]%>% select( NOMBRE, `TOTAL MATRICULA FINAL`)


#3
#Promedio, por provincia, de los hombres de la matricula final:
aprobados_fil %>% group_by(PROVINCIA) %>% summarize(promedio=mean(`HOMBRES MATRICULA FINAL`))
#Promedio, por provincia, de las mujeres de la matricula final:
aprobados_fil %>% group_by(PROVINCIA) %>% summarize(promedio=mean(`MUJERES MATRICULA FINAL`))

#4
aprobados_fil <- aprobados_fil %>% mutate( DIFERENCIA = `HOMBRES MATRICULA FINAL` - `MUJERES MATRICULA FINAL` )
sum(aprobados_fil$DIFERENCIA) 
#Lo anterior indica que existen más mujeres matriculadas que hombres matriculados en la total.

#5
aprobados_fil <- aprobados_fil %>% mutate( "PORCENTAJE DE APROBACION" = (`TOTAL APROBADOS MATRICULA FINAL` / `TOTAL MATRICULA FINAL`)*100 )
grafico<-aprobados_fil %>% group_by(PROVINCIA,ZONA) %>% summarize(`PA_PROVINCIA`=mean(`PORCENTAJE DE APROBACION`))
#Lo siguiente es el dibujo del gráfico.
ggplot(grafico, aes(x=PROVINCIA, y=PA_PROVINCIA, fill=ZONA)) + 
  geom_bar(stat = "identity")+facet_grid(ZONA~ .)+
  ggtitle("Porcentaje de aprobación según Zona y Provincia")+
  ylab("Porcentaje de Aprobación")

#TERCERA PARTE

#1
#Máximo del total aprobado
max(rendimientos_fil$`TOTAL APROBADOS`) 
#Máximo del total reprobado
max(rendimientos_fil$`TOTAL REPROBADOS`) 


rendimientos_fil[which.max(rendimientos_fil$`TOTAL APROBADOS`),]%>% select( NOMBRE, `TOTAL APROBADOS`)
rendimientos_fil[which.max(rendimientos_fil$`TOTAL REPROBADOS`),]%>% select( NOMBRE, `TOTAL REPROBADOS`)

#2
#Mínimo del total aprobado
min(rendimientos_fil$`TOTAL APROBADOS`)
#Mínimo del total reprobado
min(rendimientos_fil$`TOTAL REPROBADOS`) 

rendimientos_fil[which.min(rendimientos_fil$`TOTAL APROBADOS`),]%>% select( NOMBRE, `TOTAL APROBADOS`)
rendimientos_fil[which.min(rendimientos_fil$`TOTAL REPROBADOS`),]%>% select( NOMBRE, `TOTAL REPROBADOS`)

#3
#Promedio, por zona, de hombres aprobados
rendimientos_fil %>% group_by(ZONA) %>% summarize(promedio=mean(`TOTAL APROBADOS HOMBRES`))
#Promedio, por zona, de mujeres aprobadas
rendimientos_fil %>% group_by(ZONA) %>% summarize(promedio=mean(`TOTAL APROBADOS MUJERES`))


#4
#Promedio de mujeres reprobados, por zona
rendimientos_fil %>% group_by(ZONA) %>% summarize(promedio=mean(`TOTAL REPROBADOS MUJERES`))
#Promedio de hombres reprobados, por zona
rendimientos_fil %>% group_by(ZONA) %>% summarize(promedio=mean(`TOTAL REPROBADOS HOMBRES`))


#5
ggplot(rendimientos_fil, aes(`TOTAL APROBADOS`, `TOTAL REPROBADOS`))+
  geom_point()
