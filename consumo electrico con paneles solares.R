setwd("C:\\Users\\TC\\Desktop\\Rstudio casos\\represa")

# Crear un data frame vacío
df <- data.frame(df)


# Agregar una columna con la población total
df$poblacion <- 50000

# Agregar una columna con el consumo eléctrico promedio por habitante
df$consumo_electrico <- 200

# Agregar una columna con el consumo eléctrico total de la población
df$consumo_total <- df$poblacion * df$consumo_electrico

# Agregar una columna con el porcentaje de la población que tendrá paneles solares
df$porcentaje_paneles_solares <- 0.25

# Agregar una columna con el número de habitantes que tendrán paneles solares
df$habitantes_con_paneles_solares <- df$poblacion * df$porcentaje_paneles_solares

# Agregar una columna con el consumo eléctrico promedio de los habitantes con paneles solares
df$consumo_electrico_con_paneles_solares <- 150

# Agregar una columna con el consumo eléctrico total de los habitantes con paneles solares
df$consumo_total_con_paneles_solares <- df$habitantes_con_paneles_solares * df$consumo_electrico_con_paneles_solares

# Agregar una columna con la diferencia en el consumo eléctrico total con y sin paneles solares
df$diferencia_consumo <- df$consumo_total - df$consumo_total_con_paneles_solares

consumo_total_sin_paneles_solares <- sum(df$consumo_total)
consumo_promedio_con_paneles_solares <- mean(df$consumo_electrico_con_paneles_solares)

barplot(c(df$consumo_total, df$consumo_total_con_paneles_solares),
        names.arg=c("Sin paneles solares", "Con paneles solares"),
        main="Diferencia en el consumo eléctrico total")

# Crear el gráfico de barras
barplot(c(df$consumo_total, df$consumo_total_con_paneles_solares),
        names.arg=c("S/P solares", "C/P solares"),
        las=0,
        main="Diferencia en el consumo eléctrico total",
        col=c("lightblue", "pink"),
        cex.names=1.5)
text(x=1:2, y=c(df$consumo_total, df$consumo_total_con_paneles_solares),
     labels=c(df$consumo_total, df$consumo_total_con_paneles_solares),
     pos=3, cex=0.8)


library(ggplot2)
# Crear un data frame con los datos
df2 <- data.frame(tipo=c("Sin paneles solares", "Con paneles solares"),
                  consumo=c(df$consumo_total, df$consumo_total_con_paneles_solares))

# Crear el gráfico de barras
ggplot(data=df2, aes(x=tipo, y=consumo, fill=tipo)) +
  geom_bar(stat="identity") +
  labs(title="Diferencia en el consumo eléctrico total", x="", y="Consumo eléctrico (kWh)") +
  scale_fill_manual(values=c("lightblue", "pink")) +
  theme(legend.position="none")


"Este código crea un data frame vacío y agrega varias columnas con datos hipotéticos sobre una población y su consumo eléctrico. La población tiene un tamaño de 50.000 habitantes y un consumo eléctrico promedio de 200 kWh por habitante. Un 25% de la población tiene paneles solares y tiene un consumo eléctrico promedio de 150 kWh por habitante.

Luego, se calcula el consumo eléctrico total de la población sin paneles solares y con paneles solares y se calcula la diferencia entre ambos. También se calcula el consumo total sin paneles solares y el consumo eléctrico promedio con paneles solares.

Por último, se crean dos gráficos de barras para comparar el consumo eléctrico total sin paneles solares y con paneles solares. Uno de ellos se crea con la función barplot() de base R y el otro se crea con la librería ggplot2.

En conclusión, este código calcula y compara el consumo eléctrico total de una población hipotética con y sin paneles solares. Se crean dos gráficos de barras para facilitar la comparación visual de los datos"

"Un licenciado en antropología puede beneficiar a una empresa eléctrica de varias maneras. En primer lugar, un antropólogo puede ayudar a la empresa a entender mejor las necesidades y preferencias de sus clientes. Esto puede incluir investigar cómo los clientes usan la electricidad en sus hogares y cómo están dispuestos a pagar por ella. Además, un antropólogo puede realizar estudios de campo para conocer las necesidades y preferencias de los clientes de manera más detallada y proporcionar informes detallados y recomendaciones basados ​​en los hallazgos.

En segundo lugar, un antropólogo puede ayudar a la empresa a desarrollar estrategias de comunicación efectivas para promocionar su uso de energías renovables y reducir su impacto ambiental. Por ejemplo, pueden realizar investigaciones para entender qué mensajes resonan con los clientes y cómo se pueden comunicar de manera más efectiva.

En tercer lugar, un antropólogo puede ayudar a la empresa a desarrollar programas de responsabilidad social y medioambiental más efectivos. Esto puede incluir investigar cómo la empresa puede contribuir a la comunidad y cómo puede promover la sostenibilidad a largo plazo.

En resumen, un licenciado en antropología puede aportar una perspectiva única y valiosa a una empresa eléctrica al ayudarla a entender mejor a sus clientes y a desarrollar estrategias más efectivas para promocionar su uso de energías renovables y reducir su impacto ambiental."
