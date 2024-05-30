# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2023)

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
# Número de meses con demanda regular
n <- 9
# Demanda mínima
a <- 6
# Demanda máxima
b <- 16
# Demanda promedio
media <- 12 

datos2023 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2023)
# En promedio, en marzo se requirieron 3 rollos, en septiembre  16 y en diciembre 18
datos2023V <- c(13, 14, 3, 10, 8, 13, 12, 12, 16, 11, 15, 18)


# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2022)

triangular_media_enteros <- function(n, a, b, media) {
  c <- 2 * media - a - b
  u <- runif(n)
  lower <- ifelse(u <= (media - a)/(b - a), a + sqrt(u * (b - a) * (media - a)), 
                  b - sqrt((1 - u) * (b - a) * (b - media)))
  return(round(lower))
}

n <- 9
a <- 6
b <- 15
media <- 12 

datos2022 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2022)
# En promedio, en marzo se requirieron 3 rollos, en septiembre  16 y en diciembre 17 
datos2022V <- c(13, 13, 3, 10, 11, 13, 13, 16, 10, 11, 11, 17)


# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2021)

triangular_media_enteros <- function(n, a, b, media) {
  c <- 2 * media - a - b
  u <- runif(n)
  lower <- ifelse(u <= (media - a)/(b - a), a + sqrt(u * (b - a) * (media - a)), 
                  b - sqrt((1 - u) * (b - a) * (b - media)))
  return(round(lower))
}

n <- 9
a <- 5
b <- 13
media <- 10 

datos2021 <- triangular_media_enteros(n, a, b, media)
# Se puede verificar que la suma de dichos datos es muy aproximada a la media
summary(datos2021)
# En promedio, en marzo se requirieron 2 rollos, en septiembre  14 y en diciembre 16 
datos2021V <- c(8, 9, 2, 12, 12, 10, 8, 9, 14, 11, 11, 16)


# Cálculo de la demanda promedio mensual de rollos de cada mes (año 2020)

triangular_media_enteros <- function(n, a, b, media) {
  c <- 2 * media - a - b
  u <- runif(n)
  lower <- ifelse(u <= (media - a)/(b - a), a + sqrt(u * (b - a) * (media - a)), 
                  b - sqrt((1 - u) * (b - a) * (b - media)))
  return(round(lower))
}

n <- 9
a <- 4
b <- 13
media <- 9 

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
datos