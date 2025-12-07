
# Fuente: INE - España ------ https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=02002.px&L=0
# Comunidad Valenciana  2.113.571 varones de origen español en 2019, y 378.550 varones de nacionalidad extranjera.
# Por tanto, la población masculina total era de 2.492.121 personas.
# Como se trata de agresiones de varones a mujeres en general, nos interesa aplicar modelos exclusivamente a la población masculina total en Andalucía en el año 2019.
# Se trata de recabar cifras de incidencia de violadores en población masculina española y extranjera en esta comunidad autónoma.
# Factores: sexo masculino, origen de los agresores.
# Calculus: ratio / tasa por cada 100.000 habitantes según origen, o sobre la población total.

hom_esp <- 2113571
hom_ext <- 378550
hom_total <- 2492121

# Porcentaje de españoles y extranjeros varones en la población masculina de toda la Comunidad Valenciana.
prct_esp <- (hom_esp / hom_total) *100
prct_esp
prct_ext <- (hom_ext / hom_total) *100
prct_ext

# Según estadísticas, en 2019 hubo un total de 170 agresiones sexuales, perpetradas por españoles (113), y extranjeros (55)
agr_esp <- 90
agr_ext <- 65

# La tasa (ratio) de violaciones perpetradas por españoles en base a cada 100.000 habitantes de la población masculina total de Andalucía
esp_total <- (agr_esp / hom_total)*100000
esp_total

#Porcentaje de violaciones perpetradas por españoles basadas en tasa por cada 100.000 habitantes (sólo varones)
esp_total/100

# La tasa (ratio) de violaciones perpetradas por extranjeros en base a cada 100.000 habitantes de la población masculina total de Andalucía
ext_total <- (agr_ext / hom_total)*100000

#Porcentaje de violaciones perpetradas por extranjeros basadas en tasa por cada 100.000 habitantes (sólo varones)
ext_total/100

# Ahora calculemos la tasa y el porcentaje de violadores por cada segmento de población masculina. ¿Cuál es el porcentaje de violadores españoles entre la población de origen español de Andalucía, y el porcentaje de extranjeros entre su segmento de población -número total de extranjeros varones residentes en Andalucía en 2019-?

esp_esp <- (agr_esp / hom_esp)*100000
ext_ext <- (agr_ext / hom_ext)*100000
esp_esp
ext_ext

# Gráfico de proporciones: agresiones sexuales con penetración de varones españoles y extranjeros en 2019 en la Comunidad Valenciana.
distr_plot <- as.vector()
unique(esp_esp, ext_ext) 

distr_plot  <- distr_plot [distr_plot  != 0]
distr_plot  <- factor(distr_plot)

qplot(as.dataframe(esp_esp, ext_ext), data="hom_total")+
  ggtitle("Agresiones Sexuales CLV")

#Distribution of Movie Ratings
hom_total %>% filter(esp_esp, ext_ext) %>% summarize(n = n()) %>%
  ggplot(aes(n)) + geom_histogram(fill = "cadetblue3", color = "grey20", bins = 10) +
  scale_x_log10() +
  ggtitle("Agresiones Sexuales CLV")


# Convert dose and cyl columns from numeric to factor variables
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
mtcars$cyl <- as.factor(mtcars$cyl)
head(ToothGrowth)
     
     # Color by qsec values
sp2<-ggplot(mtcars, aes(x=wt, y=mpg, color=qsec)) + geom_point()
sp2
# Change the low and high colors
# Sequential color scheme
sp2+scale_color_gradient(low="blue", high="red")
# Diverging color scheme
mid<-mean(mtcars$qsec)
sp2+scale_color_gradient2(midpoint=mid, low="blue", mid="green",
                          high="red", space ="Lab" )
     

#Porcentaje de violadores españoles por cada 100.000 varones de origen español que viven en Andalucía en 2019.
esp_esp / 100

#Porcentaje de violadores extranjeros por cada 100.000 varones de origen extranjero que viven en Andalucía en 2019.
ext_ext/100