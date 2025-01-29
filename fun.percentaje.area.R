severity <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/obs/severity.ce.pi.ERA5-LAND.obs_1986_2005.rds")
mask <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/obs/mask.pi.era5.land.rds")

years <- 1986:2005

fun.cat.percentage.area <- function(severity, mask, years) {
    num.gb.land <- sum(mask$Data == 1, na.rm = TRUE)  # Total gridboxes de tierra
    
    # Inicializar matrices para cada categoría
    num_years <- length(years)
    c1 <- numeric(num_years)
    c2 <- numeric(num_years)
    c3 <- numeric(num_years)
    c4 <- numeric(num_years)
    c5 <- numeric(num_years)
    
    # Bucle para calcular los porcentajes por año
    for (i in seq_along(years)) {
        s.y <- subsetGrid(severity, years = years[i])  # Subconjunto por año
        s.y.c <- climatology(s.y)  # Climatología del subconjunto
        categories <- table(factor(s.y.c$Data, levels = 1:5))  # Contar categorías
        
        # Calcular porcentaje y asignar a cada array
        c1[i] <- categories["1"] / num.gb.land * 100
        c2[i] <- categories["2"] / num.gb.land * 100
        c3[i] <- categories["3"] / num.gb.land * 100
        c4[i] <- categories["4"] / num.gb.land * 100
        c5[i] <- categories["5"] / num.gb.land * 100
    }
    
    # Devolver los arrays en una lista para que sea más fácil acceder a ellos
    return(list(c1 = c1, c2 = c2, c3 = c3, c4 = c4, c5 = c5))
}

percentage.area <- fun.cat.percentage.area(severity, mask, years)
percentage.matrix <- do.call(cbind, percentage.area)

library(RColorBrewer)

install.packages("Hmisc")
library(Hmisc)

 


png("/oceano/gmeteo/users/fuentesm/ERA5/aec/percentage.area.categories.obs.png", width=1200, height=300)

matplot(
  years, 
  percentage.matrix, 
  type = "l",       # Tipo de gráfico (línea)
  lty = 1,          # Tipo de línea (sólida)
  col = brewer.pal(5, "YlOrRd"),  # Colores para las categorías
  lwd = 4,          # Grosor de las líneas
  xlab = "Years",   # Etiqueta del eje x
  ylab = "Percentage (%)",  # Etiqueta del eje y
  main = "Temporal categories of severity change",  # Título del gráfico
  axes = FALSE
)
# Añadir los ejes


axis(1, at = seq(min(years), max(years), by = 4), labels = seq(min(years), max(years), by = 4))
# Eje X: ticks adicionales cada año, sin etiquetas
axis(1, at = seq(min(years), max(years), by = 1), labels = FALSE, tck = -0.02) 

# Añadir el eje Y
axis(2, at = seq(0, 100, by = 10), labels = seq(0, 100, by = 10))  # Etiquetas del 0% al 100%
box()
# Añadir una leyenda para identificar las categorías
legend(
  "topleft",                             # Posición de la leyenda
  legend = c("Category 1", "Category 2", "Category 3", "Category 4", "Category 5"), 
  col = brewer.pal(5, "YlOrRd"),  # Colores correspondientes
  lty = 1,                                # Tipo de línea
  lwd = 2                                # Grosor de línea
)
dev.off()