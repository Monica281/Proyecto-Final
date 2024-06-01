################################  PROYECTO FINAL  ################################ 
################################    Inventarios   ################################ 


# Función que utiliza la distribución triangular
triangular_media_enteros <- function(n, a, b, media) {
  # Se calcula el parámetro adicional c que determina la forma de la distribución. 
  # Este valor se utiliza más adelante en el cálculo de los valores aleatorios.
  c <- 2 * media - a - b
  # Se generan n números aleatorios distribuidos uniformemente en el intervalo [0, 1].
  u <- runif(n)
  # Se asignan valores aleatorios según la distribución triangular
  # Si u <= (media-a)/(b-a), se calcula el valor utilizando la fórmula a+sqrt(u*(b-a)*(media-a)) 
  # Este caso corresponde a valores en la parte izquierda de la media.
  # Si u > (media-a)/(b-a), se calcula el valor utilizando la fórmula b-sqrt((1-u)*(b-a)*(b-media)).
  # Este caso corresponde a valores en la parte derecha de la media.
  lower <- ifelse(u <= (media - a)/(b - a), a + sqrt(u * (b - a) * (media - a)), 
                  b - sqrt((1 - u) * (b - a) * (b - media)))
  # Se redondean los valores 
  return(round(lower))
}

# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2023)

# Número de meses con demanda regular
n <- 9
# Demanda mínima
a <- 6
# Demanda máxima
b <- 16
# Demanda promedio
media <- 12 
# Se aplica la función
datos2023 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2023)
# En promedio, en marzo se requirieron 3 rollos, en septiembre  16 y en diciembre 18
datos2023V <- c(13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)


# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2022)

# Número de meses con demanda regular
n <- 9
# Demanda mínima
a <- 6
# Demanda máxima
b <- 15
# Demanda promedio
media <- 12 
# Se aplica la función
datos2022 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2022)
# En promedio, en marzo se requirieron 3 rollos, en septiembre  16 y en diciembre 17 
datos2022V <- c(13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17)


# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2021)

# Número de meses con demanda regular
n <- 9
# Demanda mínima
a <- 5
# Demanda máxima
b <- 13
# Demanda promedio
media <- 10 
# Se aplica la función
datos2021 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2021)
# En promedio, en marzo se requirieron 2 rollos, en septiembre  14 y en diciembre 16 
datos2021V <- c(8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16)


# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2020)

# Número de meses con demanda regular
n <- 9
# Demanda mínima
a <- 4
# Demanda máxima
b <- 13
# Demanda promedio
media <- 9 
# Se aplica la función
datos2020 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2020)
# En promedio, en marzo se requirieron 2 rollos, en septiembre  14 y en diciembre 14 
datos2020V <- c(7, 9, 2, 10, 12, 10, 7, 8, 14, 8, 11, 14)

# De tal forma que el acumulado de nuestras demandas promedio mensuales de los
# últimos 4 años quedaría de la siguiente manera:
datos4anos <- c(7, 9, 2, 10, 12, 10, 7, 8, 14, 8, 11, 14,8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16, 13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17, 13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)
fechas <- seq(as.Date("2020-01-01"), by = "month", length.out = length(datos4anos))
datos <- data.frame(Fecha = fechas, Demanda = datos4anos)
colnames(datos) <- c("Fecha", "Demanda_Promedio_Mensual")
View(datos)

###################################################################################

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

# install.packages("tseries")
library(tseries)
datos4anos <- c(7, 9, 2, 10, 12, 10, 7, 8, 14, 8, 11, 14,8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16, 13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17, 13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)
adf.test(datos4anos)

# Observamos que, dado que el valor p es 0.03368, se cumple la hipótesis alternativa,
# lo que sugiere que nuestro modelo es estacionario y, por lo tanto, determinista.
# Esto nos permite realizar predicciones con mayor confianza utilizando este modelo.

###################################################################################

# Visualicemos un gráfico de línea que represente la demanda promedio mensual 
# de los últimos cuatro años, con superposiciones entre cada año para observar 
# cómo ha aumentado el negocio del señor respecto a sus demandas durante este periodo. 
# Esta visualización nos permitirá apreciar de manera más efectiva la tendencia de 
# crecimiento a lo largo del tiempo.

datos2020V <- c(7, 9, 2, 10, 12, 10, 7, 8, 14, 8, 11, 14)
datos2021V <- c(8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16)
datos2022V <- c(13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17)
datos2023V <- c(13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)

meses <- seq(1, 12)

par(mfrow = c(1, 1))
plot(meses, datos2020V, type = "l", col = "blue", lwd = 2, xlim = c(1, 12), ylim = c(1,18), xlab = "Meses", ylab = "Valor", main = "Datos de los años 2020-2023")
lines(meses, datos2021V, col = "red", lwd = 2)
lines(meses, datos2022V, col = "green", lwd = 2)
lines(meses, datos2023V, col = "purple", lwd = 2)

legend("bottomright", legend = c("2020", "2021", "2022", "2023"), col = c("blue", "red", "green", "purple"), lwd = 2, cex = 0.6)

###################################################################################

# Coeficiente de variabilidad

# Primero calculamos la media de los datos
datos4anos <- c(7, 9, 2, 10, 12, 10, 7, 8, 14, 8, 11, 14,8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16, 13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17, 13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)
suma_datos<- sum(datos4anos)
N_datos <- length(datos4anos)
estimacion_dem <- suma_datos/N_datos
estimacion_dem

# Ahora calculamos la estimación de la varianza 
suma_cuad_datos <- sum(datos4anos^2)
estimacion_var <- (suma_cuad_datos/N_datos)-(estimacion_dem^2)
estimacion_var

# Por último calculamos el coeficiente de variabilidad
VC <- (estimacion_var/ (estimacion_dem^2))
VC

# Como el coeficiente de variabilidad es menor que 20% podemos aplicar un modelo EOQ.

###################################################################################

# Costos 

# 1. Rollos de PVC

# Multiplicamos la demanda de los 4 años por 1300, que es lo que cuesta cada rollo, 
# depués dividimos entre 4 para obtener el costo anual
costo4anos <- datos4anos*1300
costoaunal <- (sum(costo4anos))/4
# Visualicemos un data frame del costo promedio mensual durante los últimos 4 años 
fechas <- seq(as.Date("2020-01-01"), by = "month", length.out = length(costo4anos))
costo4anosfra <- data.frame(Fecha = fechas, Costo_mensual = costo4anos)
colnames(costo4anosfra) <- c("Fecha", "Costo_mensual")
View(costo4anosfra)

# 2. Moldes metálicos

# Molde ideal de huecos pequeños
moldes_totales <- 16
tiempo_vida_medio <- 2  # Años
tiempo_simulacion <- 4  # Años
# Generar datos de desgaste de moldes utilizando distribución exponencial

#rexp(n, rate) genera n valores aleatorios que siguen una distribución exponencial con una tasa (rate) específica.
#En este caso, n es igual al número total de moldes y la tasa se calcula como la inversa del tiempo medio 
#de vida (1 / tiempo_vida_medio). La distribución exponencial es apropiada para modelar el tiempo entre eventos sucesivos en un proceso 
#de Poisson, lo que se ajusta bien al desgaste gradual de los moldes con el tiempo.
desgaste_moldes <- rexp(moldes_totales, rate = 1 / tiempo_vida_medio)
# Contar cuántos moldes necesita comprar en un 1 año
#desgaste_moldes < tiempo_simulacion compara cada valor de desgaste de molde generado con el período de tiempo de 
#simulación. Devuelve un vector lógico donde TRUE indica que el molde necesita ser reemplazado dentro del período 
#de tiempo especificado.
moldes_comprar1 <- sum(desgaste_moldes < tiempo_simulacion)
#14 moldes con huecos pequeños en 4 años
#sum(desgaste_moldes < tiempo_simulacion) suma los valores TRUE en el vector lógico, lo que representa el número de moldes que se necesitan
#comprar dentro del período de tiempo especificado.
moldes_chi_anual <- ceiling(moldes_comprar1/4)

# Molde ideal de huecos medianos 
moldes_totales <- 6
tiempo_vida_medio <- 3.5  # Años
tiempo_simulacion <- 4  # Años
# Generar datos de desgaste de moldes utilizando distribución exponencial
desgaste_moldes <- rexp(moldes_totales, rate = 1 / tiempo_vida_medio)
# Contar cuántos moldes necesita comprar en un año
moldes_comprar2 <- sum(desgaste_moldes < tiempo_simulacion)
# 4 moldes con huecos medianos en 4 años
moldes_med_anual <- ceiling(moldes_comprar2/4)
# 1 molde con huecos medianos en un año

# Molde ideal de huecos grandes 
moldes_totales <- 4
tiempo_vida_medio <- 3.5  # Años
tiempo_simulacion <- 4  # Años
# Generar datos de desgaste de moldes utilizando distribución exponencial
desgaste_moldes <- rexp(moldes_totales, rate = 1 / tiempo_vida_medio)
# Contar cuántos moldes necesita comprar en un 1 año
moldes_comprar3 <- sum(desgaste_moldes < tiempo_simulacion)
# 3 moldes con huecos grandes en 4 años
moldes_grd_anual <- ceiling(moldes_comprar3/4)


# El molde con huecos grandes y medianos aproximadamente cuestan 1100 pesos; 
# mientras que el molde con huecos pequeños cuesta 1200 pesos, de tal forma que 
# el gasto relacionado a los moldes al cabo de 4 años es: 
gasto_molde_4anos <- (moldes_comprar1*1200)+((moldes_comprar2+moldes_comprar3)*1100)
gasto_molde_4anos
# $24500 pesos

###################################################################################

# MODELO EOQ PROBABILÍSTICO

# Calcular la probabilidad de obtener cada valor único
probabilidades <- prop.table(table(datos4anos))
print(probabilidades)

# Calcular la probabilidad acumulada
prob_acumulada <- cumsum(probabilidades)
probabilidades_acum_df <- as.data.frame(prob_acumulada)
probabilidades_acum_df



##################################  PROYECTO FINAL  ################################ 
################################ Teoría de decisiones ################################ 

calcular_dia <- function(dias_despues) {
  # Definir la fecha de inicio
  fecha_inicio <- as.Date("2024-11-02")
  # Sumar el número de días especificado
  nueva_fecha <- fecha_inicio + dias_despues
  # Convertir la nueva fecha al formato deseado
  nueva_fecha_formato <- format(nueva_fecha, "%d/%m")
  # Devolver la nueva fecha en formato "dd/mm"
  return(nueva_fecha_formato)
}

# Ejemplo de uso:
dias_despues <- 34
dia_resultante <- calcular_dia(dias_despues)
print(dia_resultante)
#1 de enero 
#2 de febrero
#9 de marzo
#12 de abril
#16 de mayo
#19 de junio
#23 de julio
#26 de agosto  
#29 de septiembre
#2 de noviembre
#12 de diciembr


obtener_valores_noveno_decimo <- function(vector) {
  valor_noveno <- vector[9]  # posición 9
  valor_decimo <- vector[10]  # posición 10
  return(c(valor_noveno, valor_decimo))
}
resultado <- obtener_valores_noveno_decimo(datos2023V)
print("Noveno y décimo valor:")
print(resultado)

#valores de seprtiembre y octubre posibles
sept <-c(16,10,14,14) 
###
probabilidades <- prop.table(table(sept))

probabilidades_vector <- as.vector(probabilidades)

print(probabilidades)
###
oct <-c(11,11,11,8)

probabilidades <- prop.table(table(oct))

probabilidades_vector <- as.vector(probabilidades)

print(probabilidades)

añoscuatros <-c(11,11,11,8,16,10,14,14)
mean(añoscuatros)

###

###10y8 = (0.25)(0.25)=0.0625   --- 18
###10y11  = (0.25)(0.75)=0.1875 --- 21

###14y8  = (0.50)(0.25)=0.125  ---- 22
###14y11  = (0.50)(0.75)=0.375   --- 25

###16y8 = (0.25)(0.25)=0.0625 --- 24
###16y11  = (0.25)(0.75)=0.1875 --- 27

###verificamos que las probabillidades dan 1


mean(18,21,22,24,25)
