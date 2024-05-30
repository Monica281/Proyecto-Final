# Prueba Dickey-Fuller

# Si confirmamos que nuestra base de datos es estacionaria, lo que implicaría un  
# comportamiento determinista, podríamos realizar predicciones utilizando el mismo modelo. 
# Para verificar la estacionalidad, comúnmente se utiliza la prueba Dickey-Fuller Aumentada, 
# que nos permite determinar si una serie temporal es estacionaria o no estacionaria.

# Interpretación:
# Hipótesis nula: La serie temporal es no estacionaria.
# Hipótesis alternativa:  La serie temporal  es estacionaria.
# Si el valor p resultante del test es menor que un nivel de significancia (usualmente 0.05),
# se rechaza la hipótesis nula y se concluye que la serie temporal es estacionaria.

install.packages("tseries")
library(tseries)
datos4anos <- c(7, 9, 2, 10, 12, 10, 7, 8, 14, 8, 11, 14,8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16, 13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17, 13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)
adf.test(datos4anos)

# Observamos que, dado que el valor p es 0.03368, se cumple la hipótesis alternativa,
# lo que sugiere que nuestro modelo es estacionario y, por lo tanto, determinista.
# Esto nos permite realizar predicciones con mayor confianza utilizando este modelo.