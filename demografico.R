#  cargar la librería tidyverse y ggplot2 para graficos
library(ggplot2)
library(tidyverse)
library(dplyr)

# Crear la base de datos con 1000 personas
datos <- tibble(
  edad = sample(18:80, 1000, replace = TRUE),
  genero = sample(c("Hombre", "Mujer"), 1000, replace = TRUE),
  estudios = sample(c("Primaria", "Secundaria", "Universitario"), 1000, replace = TRUE),
  ocupacion = sample(c("Empleado", "Desempleado", "Autónomo"), 1000, replace = TRUE),
  ingresos = sample(c(500:2500), 1000, replace = TRUE),
  salud = sample(c("si", "no"), 1000, replace = TRUE),
  
)

# Visualizar la distribución de la edad de la población
hist(datos$edad)

#¿Cuál es la edad media de las personas en la base de datos?

mean(datos$edad)
#¿Cuál es la distribución de género en la base de datos?
  
table(datos$genero)
#¿Hay alguna relación entre el nivel de estudios y la ocupación de las personas?
  
xtabs(~estudios+ocupacion, datos)

#¿Los ingresos promedio varían según el género o la ocupación de las personas?
 

library(dplyr)

datos %>%
  group_by(genero, ocupacion) %>%
  summarize(mean_ingresos = mean(ingresos))
#¿Hay alguna relación entre la salud y el ingreso de las personas?
 
library(ggplot2)

ggplot(datos, aes(x = salud, y = ingresos)) +
  geom_boxplot()

ggplot(datos, aes(x = edad, y = ingresos)) +
  geom_point()


ggplot(datos, aes(x = estudios)) +
  geom_bar(aes(fill = estudios))

resultados_regresion <- lm(ingresos ~ genero + edad + estudios, data = datos)

plot(resultados_regresion)

#Ver si hay alguna diferencia significativa en los ingresos promedio entre hombres y mujeres:
library(tidyverse)

t.test(ingresos ~ genero, data = datos)


datos_filtrados <- datos %>%
  filter(estudios %in% c("Primaria", "Universitario"))


mean(datos$salud == "si")

chisq.test(datos$genero, datos$ocupacion)

lm(ingresos ~ genero + edad + estudios, data = datos)
resultados_regresion <- lm(ingresos ~ genero + edad + estudios, data = datos)
plot(resultados_regresion)

ggplot(datos, aes(x = genero, y = ingresos, color = estudios)) +
  geom_point(position = position_jitter()) +
  geom_smooth(formula = y ~ x, method = "lm") +
  labs(title = "Regression of ingresos on genero, edad, and estudios",
       x = "Genero",
       y = "Ingresos")

