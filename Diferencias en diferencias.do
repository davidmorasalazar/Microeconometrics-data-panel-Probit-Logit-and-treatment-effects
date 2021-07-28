/* 1. ¿Qué es el método de diferencias en diferencias? ¿Es correcto utilizarlo en esta situación? Explique ampliamente (8 puntos)
La herramienta para evaluar el impacto de un programa o tratamiento sobre un resultado Y de un grupo de individuos tiende a ser el estimador de diferencias en diferencias. Este método modela el impacto de un tratamiento sobre un grupo al que se le otorga el tratamiento, contra uno de control que no lo tiene, observando así los resultados a traves del dos periodos.

Se suponen dos grupos, indexados según su estatus de tratamiento, donde T=0 quiere decir que no recibió el tratamiento y T=1 recibió el tratamiento.​
El grupo que no recibió el tratamiento (T = 0 ) se denomina como grupo de control y grupo que recibió el tratamiento (T = 1) se le llama grupo de tratamiento.​
Existe un componente temporal en el estudio, ya que se observan los individuos en dos periodos de tiempo. Donde t = 0 es el periodo del tiempo antes del tratamiento y t = 1 es el periodo después del tratamiento.  

Para usar este método con estos datos se tendría que tomar el supuesto que los
paises E,F, G son comparables como grupo con los paises A,B,C,D ya que se compara estos 
primeros (tratados) con los segundos de control, este método solo se podría utilizar si existe un entorno experimental controlado.*/ 

ssc install diff
/*2. Calcule el estimador de diferencias en diferencias “a pie”. (2 puntos) */
tabulate treated time, summarize(y) nostandard nofreq noobs

/*3. Calcule el estimador de diferencias en diferencias sin covariables. Utilice las variables y, treated y time. ¿Qué se concluye con esta estimación? ¿Es significativo el tratamiento? ¿Cuál es el coeficiente que se relaciona con el estimador? (6 puntos) */
diff y, t(treated) p(time)

/*El estimador de diferencias en las diferencias muestra una reducción en la variable de interés debido a la política para los países tratados con respecto a los no tratados que no recibieron la política.*/

/* Con el estimador se concluye que la diferencia entre tratados y no tratados
 es negativa*/
 
 /* El efecto del tratamiento si es significativo con un 10% de significancia*/
 
 /* Coeficiente relacionado al estimador es de -2.54 miles de millones */
 
 
 /*4. Calcule el estimador de diferencias en diferencias con las siguientes covariables: x1, x2 y x3. Además, utilice las variables y, treated y time. ¿Qué se concluye con esta estimación? ¿Es significativo el tratamiento? ¿Cuál es el coeficiente que se relaciona con el estimador? (6 puntos)*/
 
 diff y, t(treated) p(time) cov(x1 x2 x3) report

/*El estimador de diferencias en las diferencias muestra una reducción en la variable de interés debido a la política para los países tratados con respecto a los no tratados que no recibieron la política.*/
/* La conclusión llega a ser similar que en el cálculo del estimador pasado. 
Existe una diferencia negativa entre grupos tratados y de control. Es importante considerar que el estimador de diferencias en diferencias que incluye las covariables es más certero.*/

/*El efecto del tratamiento vuelve a ser significativo a un nivel de signifacia del 10% */

/* El coeficiente relacionado al estimador es de -2.6 miles de millones, cercano al resultado
anterior*/


/* 5. ¿Existe una diferencia significativa entre las medias de los dos grupos? Utilice una prueba t de diferencia de medias en primer período, suponga variancias iguales (5 puntos) */

 diff y, t(treated) p(time) cov(x1 x2 x3) test
 
 /* No hay diferencia significativa en la media de grupos con respecto al 
 resultado y, ni a los predictores x1 y x2. Sin embargo, se muestra que el predictor
 x3 si es significativo a un nivel de significancia de 5%, en este si
 se muestra que sus medias por grupo si son significativamente distintas. */
 
