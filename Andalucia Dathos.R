
# Fuente: INE - España ------ https://www.ine.es/jaxi/Tabla.htm?path=/t20/e245/p08/l0/&file=02002.px&L=0
# Andalucía 3.816.350 varones de origen español en 2019, y 354.255 varones de nacionalidad extranjera.
# Por tanto, la población masculina total era de 4.170.605 personas.
# Como se trata de agresiones de varones a mujeres en general, nos interesa aplicar modelos exclusivamente a la población masculina total en Andalucía en el año 2019.
# Se trata de recabar cifras de incidencia de violadores en población masculina española y extranjera en esta comunidad autónoma.
# Factores: sexo masculino, origen de los agresores.
# Calculus: ratio / tasa por cada 100.000 habitantes según origen, o sobre la población total.

hom_esp <- 3816350
hom_ext <- 354255
hom_total <- 4170605

# Porcentaje de españoles y extranjeros varones en la población masculina de toda Andalucía
prct_esp <- (hom_esp / hom_total) *100
prct_esp
prct_ext <- (hom_ext / hom_total) *100
prct_ext

# Según estadísticas, en 2019 hubo un total de 170 agresiones sexuales, perpetradas por españoles (113), y extranjeros (55)
agr_esp <- 113
agr_ext <- 55

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

#Porcentaje de violadores españoles por cada 100.000 varones de origen español que viven en Andalucía en 2019.
esp_esp / 100

#Porcentaje de violadores extranjeros por cada 100.000 varones de origen extranjero que viven en Andalucía en 2019.
ext_ext/100





