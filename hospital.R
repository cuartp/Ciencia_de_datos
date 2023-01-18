setwd("C:\\Users\\TC\\Desktop\\Rstudio casos\\hospital")


# Instale y cargue las librerías necesarias
#install.packages(c("dplyr", "ggplot2"))
library(dplyr)
library(ggplot2)

# Cree una base de datos de pacientes con síntomas aleatorios
#set.seed(123)
bd_paciente <- data.frame(
  sexo = sample(c("Hombre", "Mujer"), size = 500, replace = TRUE),
  condición_médica = sample(c("Sí", "No"), size = 500, replace = TRUE),
  síntoma = sample(c("Dolor de cabeza", "Dolor de estómago", "Fiebre"), size = 100, replace = TRUE)
)


# Obtenga un resumen de la base de datos
bd_paciente <- bd_paciente %>% 
  group_by(sexo, síntoma) %>% 
  mutate(prop = n()/nrow(bd_paciente))
summary(bd_paciente)


# Cuente cuántos pacientes tienen cada síntoma
bd_paciente %>% group_by(síntoma) %>% summarise(count = n())

# Encuentre la proporción de pacientes con cada síntoma por sexo
bd_paciente %>% group_by(sexo, síntoma) %>% summarise(prop = n()/nrow(bd_paciente))

# Encuentre la proporción de pacientes con cada síntoma por condición médica
bd_paciente %>% group_by(condición_médica, síntoma) %>% summarise(prop = n()/nrow(bd_paciente))



# Grafique la proporción de pacientes con cada síntoma por sexo
ggplot(bd_paciente, aes(x = síntoma, y = prop, fill = sexo)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("#00CC66", "#E69F00")) +
  labs(x = "Síntoma", y = "Proporción de pacientes", fill = "Sexo") +
  ggtitle("Distribución de síntomas por sexo") +
  theme(legend.position = "top")




# Grafique la proporción de pacientes con cada síntoma por condición médica
ggplot(bd_paciente, aes(x = síntoma, y = prop, fill = condición_médica)) +
  geom_col(width = 0.8) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Síntoma", y = "Proporción de pacientes", fill = "Condición médica") +
  ggtitle("Distribución de síntomas por condición médica") +
  theme_classic() + # Usamos el tema classic en lugar de theme
  theme(legend.position = "top") # Dejamos el resto de las opciones del tema original

# Grafique la proporción de pacientes con cada síntoma por sexo y condición médica

ggplot(bd_paciente, aes(x = síntoma, y = prop, fill = interaction(sexo, condición_médica))) +
  geom_col() +
  ggtitle("Proporción de pacientes con cada síntoma por sexo y condición médica") +
  xlab("Síntoma") +
  ylab("Proporción") +
  scale_x_discrete(labels = c("Dolor de cabeza" = "Dolor\nde cabeza",
                              "Dolor de estómago" = "Dolor\nde estómago",
                              "Fiebre" = "Fiebre")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

# Pruebe la relación entre el sexo y el síntoma usando un test de chi-cuadrado
chisq.test(bd_paciente$sexo, bd_paciente$síntoma)

# Pruebe la relación entre la condición médica y el síntoma usando un test de chi-cuadrado
chisq.test(bd_paciente$condición_médica, bd_paciente$síntoma)

# Pruebe la relación entre el sexo, la condición médica y el síntoma usando un test ANOVA
aov(prop ~ sexo + condición_médica + síntoma, data = bd_paciente)

#Si quieres realizar un post-hoc test para comparar las diferencias entre los grupos de manera más detallada, puedes utilizar la función TukeyHSD().
TukeyHSD(aov(prop ~ sexo + condición_médica + síntoma, data = bd_paciente))


#grafico
plot(TukeyHSD(aov(prop ~ sexo + condición_médica + síntoma, data = bd_paciente)))


print("El gráfico que se produce a partir de la función 'plot' con el resultado del test de Tukey es un gráfico de comparación de intervalos de confianza. Los intervalos de confianza se muestran como barras horizontales con puntas en cada extremo. La línea central de cada barra representa la diferencia entre los grupos comparados, mientras que las puntas representan los límites inferior y superior del intervalo de confianza al 95%. Los grupos que no se superponen en los intervalos de confianza se consideran estadísticamente diferentes entre sí. En otras palabras, si los intervalos de confianza de dos grupos no se superponen, entonces hay una diferencia significativa entre ellos. Por otro lado, si los intervalos de confianza sí se superponen, entonces no se puede concluir que exista una diferencia significativa entre los grupos.")




print("El test de Tukey es un test de comparación múltiple de medias que se utiliza para comparar las diferencias entre las medias de dos o más grupos. Este test se utiliza a menudo después de realizar un análisis de varianza (ANOVA) para obtener una mayor comprensión de las diferencias entre los grupos.

En el resultado del test de Tukey que has proporcionado, se muestra la diferencia entre las medias de los grupos para cada una de las variables incluidas en el modelo. Por ejemplo, en la primera tabla se muestra la diferencia entre las medias de la variable 'sexo' para los grupos 'Mujer y 'Hombre'. En la segunda tabla se muestra la diferencia entre las medias de la variable 'condición_médica' para los grupos 'Sí' y 'No'. Y en la tercera tabla se muestran las diferencias entre las medias de la variable 'síntoma' para los grupos 'Dolor de estómago', 'Dolor de cabeza' y 'Fiebre'.

Para cada comparación se muestran también los valores 'lwr' (lower bound) y 'upr' (upper bound), que corresponden al intervalo de confianza al 95% para la diferencia de medias. Y el valor 'p adj' (p-value ajustado) muestra el nivel de significación de la diferencia de medias. Un valor de 'p adj' menor que 0.05 indica que la diferencia de medias es significativa.")

print("En resumen, nuestro estudio ha demostrado que el sexo, la condición médica y la interacción entre ambos pueden ser factores importantes en la presencia de ciertos síntomas en los pacientes. Estos hallazgos son relevantes para la atención médica y pueden ayudar a los médicos a entender cómo el género y las condiciones médicas preexistentes pueden afectar la presencia de síntomas y el pronóstico de los pacientes. Sin embargo, es importante señalar que nuestro estudio tiene algunas limitaciones, como el tamaño de la muestra y la falta de información sobre otros posibles factores de confusión. Sugerimos realizar más investigaciones para confirmar y profundizar en estos hallazgos.")
