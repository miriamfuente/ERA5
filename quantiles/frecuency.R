options(java.parameters = "-Xmx32g") 
library(loadeR)
library(transformeR)
library(visualizeR)
library(climate4R.UDG)
library(dplyr)
library(climate4R.value)
library(VALUE)
library(downscaleR) # needed for the biasCorrection (function biasCorrection)
library(climate4R.indices) # needed for calculate indices

# Variables Data
hottest.month <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/data/basic/hottest.month.pi.1986.2005.era5.land.rds")
tmax.bin <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/data/basic/tmax.bin.pi.1986.2005.era5.land.rds")
pr.bin <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/data/basic/pr.bin.pi.30mm.1986.2005.era5.land.rds")
mask <- readRDS("/oceano/gmeteo/users/fuentesm/ERA5/ERA5-LAND/data/basic/mask.pi.era5.land.rds")

meses <- 1:12
# years <- 1986:2005
list.years <- list()
list.lon <- list()
list.lat <- list()
list.total <- list()

for (i in 1:dim(tmax.bin$Data)[which(attr(tmax.bin$Data, "dimensions") == "lat")]) {
    print(paste("i= ", i))
    tmax.lat <- subsetDimension(tmax.bin, dimension="lat", indices=i)
    pr.lat <- subsetDimension(pr.bin, dimension="lat", indices=i)
    for(j in 1:dim(tmax.bin$Data)[which(attr(tmax.bin$Data, "dimensions") == "lon")]) {
        # print(paste("j= ", j))
        tmax.lon <- subsetDimension(tmax.lat, dimension="lon", indices=j)
        pr.lon <- subsetDimension(pr.lat, dimension="lon", indices=j)
        month <- hottest.month$Data[i, j]
        if (is.na(month)) {
            tmax.final.lon <- redim(tmax.lon, drop = TRUE)
            pr.final.lon <- redim(pr.lon, drop = TRUE)
        } else {
            pr.lon.hot <- subsetGrid(pr.lon, season=month)
            tmax.lon.hot <- subsetGrid(tmax.lon, season=month)
            tmax.lon.no.hot <- subsetGrid(tmax.lon, season=c(setdiff(meses, month)))    
            for (k in 1:dim(pr.lon.hot$Data)[which(attr(pr.lon.hot$Data, "dimensions") == "time")]) {
                # print(paste("k= ", k))
                if (pr.lon.hot$Data[k] == 1) {
                    date <- as.POSIXct(pr.lon.hot$Dates$start[k])
                    year <- as.numeric(format(date, "%Y"))
                    tmax.lon.hot.y <- subsetGrid(tmax.lon.hot, years=year)
                    attr(tmax.lon.hot.y$Data, "dimensions") <- c("time")
                    list.years[[k]] <- tmax.lon.hot.y
                } else{
                    date <- as.POSIXct(pr.lon.hot$Dates$start[k])
                    year <- as.numeric(format(date, "%Y"))
                    tmax.lon.hot.y <- subsetGrid(tmax.lon.hot, years=year)
                    tmax.lon.hot.y$Data[] <- NA
                    attr(tmax.lon.hot.y$Data, "dimensions") <- c("time")
                    list.years[[k]] <- tmax.lon.hot.y
                }
                
            }
            tmax.lon.hot <- do.call(bindGrid, c(list.years, list(dimension = "time", skip.temporal.check = TRUE)))
            tmax.final.lon <- bindGrid(tmax.lon.no.hot, tmax.lon.hot, dimension = "time")

        }
        list.lon[[j]] <- tmax.final.lon
    }
    list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)

}
list.final <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop = TRUE)

saveRDS(list.final, "ERA5-LAND/frecuency/ce.pi.30mm.1986.2005.rds")



suma <- climatology(list.final, clim.fun=list("sum", na.rm = TRUE))
suma.mask <- gridArithmetics(suma, mask, operator = "*")


# Plot the results
png("ERA5-LAND/frecuency/ce.event.30mm.png", width=1200, height=800)
spatialPlot(suma.mask, backdrop.theme="coastline", color.theme="YlOrRd", set.min=0, set.max=66, at=seq(0, 66, 2),
            main="Number of compound events Pr <= P50 & Pr(<30) > P50 and Tmax => T90", xlab="Longitude", ylab="Latitude", 
            colorkey = list(space = "right",
                            title = list("Nº Events", cex = 1))
            )
dev.off()

#----------------------------------------------------------------
# Comprobation of the results in a gribox
lat =8
lon= 24
tmax.bin.lat <- subsetDimension(tmax.bin, dimension="lat", indices=lat)
tmax.bin.lon <- subsetDimension(tmax.bin.lat, dimension="lon", indices=lon)
serie.temporal <- aggregateGrid(tmax.bin.lon, aggr.m = list(FUN = "sum", na.rm = TRUE))

pr.bin.lat <- subsetDimension(pr.bin, dimension="lat", indices=lat)
pr.bin.lon <- subsetDimension(pr.bin.lat, dimension="lon", indices=lon)

hottest.month.1 <- hottest.month$Data[,lat, lon]
serie.temporal.1 <- subsetGrid(serie.temporal, season=hottest.month.1)
pr.bin.lon.1 <- subsetGrid(pr.bin.lon, season=hottest.month.1)

list.final.lat <- subsetDimension(list.final, dimension="lat", indices=lat)
list.final.lon <- subsetDimension(list.final.lat, dimension="lon", indices=lon)
list.final.lon.1 <- aggregateGrid(list.final.lon, aggr.m = list(FUN = "sum", na.rm = TRUE))
list.final.lon.2 <- subsetGrid(list.final.lon.1, season=hottest.month.1)

t90 <- readRDS("quantiles/data.quantiles.tmax/tmax.q90.monthly.pi.1986.2005.rds")
t90.lat <- subsetDimension(t90, dimension="lat", indices=lat)
t90.lon <- subsetDimension(t90.lat, dimension="lon", indices=lon)
t90.lon.1 <- subsetGrid(t90.lon, season=hottest.month.1)

t.daily <- readRDS("quantiles/data.basic.pi/tmax.daily.hottest.month.pi.1986.2005.rds")
t.daily.lat <- subsetDimension(t.daily, dimension="lat", indices=lat)
t.daily.lon <- subsetDimension(t.daily.lat, dimension="lon", indices=lon)
t.daily.lon.1 <- subsetGrid(t.daily.lon, season=hottest.month.1)
tmax.bin.lon.1 <- subsetGrid(tmax.bin.lon, season=hottest.month.1)

years <- 1986:2005



png("prueba.serie.temporal.plot.png", width=1200, height=800)
# Dividir el área gráfica en 3 filas y 1 columna
par(mfrow=c(3,1))
# Añado el titulo
# Primer gráfico: serie.temporal.1
plot(years, serie.temporal.1$Data, type="l", col="blue", xlab="Year", ylab="Nº Events", 
     main="Events of Tmax >= T90 (37.8, -3.75)", lwd=2, ylim=c(min(serie.temporal.1$Data), max(serie.temporal.1$Data)))
axis(1, at=years)
axis(2, at=seq(min(serie.temporal.1$Data), max(serie.temporal.1$Data), by=1))
box()
# Segundo gráfico: pr.bin.lon.1
plot(years, pr.bin.lon.1$Data, type="l", col="red", xlab="Year", ylab="Nº Events", 
     main="Events of Pr <= P50 (37.8, -3.75)", lwd=2, ylim=c(min(pr.bin.lon.1$Data), max(pr.bin.lon.1$Data)))
axis(1, at=years)
axis(2, at=seq(min(pr.bin.lon.1$Data), max(pr.bin.lon.1$Data), by=1))
box()
# Tercer gráfico: list.final.lon.2
plot(years, list.final.lon.2$Data, type="l", col="green", xlab="Year", ylab="Nº Events", 
     main="Compound Events Pr <= P50 & Tmax >= T90 (37.8, -3.75)", lwd=2, ylim=c(min(list.final.lon.2$Data), max(list.final.lon.2$Data)))
axis(1, at=years)
axis(2, at=seq(min(list.final.lon.2$Data), max(list.final.lon.2$Data), by=1))
box()

dev.off()



png("prueba.serie.temporal.tmax.png", width=1200, height=800)
par(mfrow=c(2,1))

# Graficar la serie temporal de t.daily.lon.1$Data
plot(t.daily.lon.1$Data, type="l", col="black", xlab="Year", ylab="Nº Events", 
     main="Events of Tmax >= T90 (37.8, -3.75)", lwd=2, xaxt="n")  # Eliminar eje X por defecto

# Crear las etiquetas de los años para el eje X (31 días por año)
years <- seq(1986, 2005, by=1)  # Los años de 1986 a 2005
labels <- seq(0, 620, by=31)  # Los índices que corresponden a los 31 días por año (empezando desde el 0)

# Agregar el primer año en la posición 1
years_labels <- c(1986, years)  # Añadir el primer año (1986) al principio de los años

# Establecer las etiquetas del eje X (colocando los años en las posiciones adecuadas)
axis(1, at=labels, labels=years_labels, las=2)  # 'las=2' para rotar las etiquetas del eje X

# Agregar una línea horizontal representando el valor de t90.lon.1$Data
abline(h=t90.lon.1$Data, col="red", lwd=2, lty=2)  # Línea horizontal en el valor de t90.lon.1$Data

# Graficar la serie temporal de tmax.bin.lon.1$Data
plot(tmax.bin.lon.1$Data, type="l", col="black", xlab="Year", ylab="Nº Events", 
     main="Events of Tmax >= T90 (37.8, -3.75)", lwd=2, xaxt="n")  # Eliminar eje X por defecto

# Establecer las etiquetas del eje X para el segundo gráfico
axis(1, at=labels, labels=years_labels, las=2)  # 'las=2' para rotar las etiquetas del eje X

dev.off()




png("prueba.serie.temporal.plot.png", width=1600, height=1200)
# Dividir el área gráfica en 3 filas y 1 columna
par(mfrow=c(3,1))

# Primer gráfico: serie.temporal.1
plot(years, serie.temporal.1$Data, type="l", col="blue", xlab="Year", ylab="Nº Events", 
     main="Events of Tmax >= T90 (37.8, -3.75)", lwd=2, ylim=c(min(serie.temporal.1$Data), max(serie.temporal.1$Data)))
axis(1, at=years)
axis(2, at=seq(min(serie.temporal.1$Data), max(serie.temporal.1$Data), by=1))
box()

# Añadir valores en los puntos del primer gráfico
for (i in 1:length(serie.temporal.1$Data)) {
  text(years[i], serie.temporal.1$Data[i], labels=serie.temporal.1$Data[i], pos=3, cex=1, col="blue")
}

# Segundo gráfico: pr.bin.lon.1
plot(years, pr.bin.lon.1$Data, type="l", col="red", xlab="Year", ylab="Nº Events", 
     main="Events of Pr <= P50 (37.8, -3.75)", lwd=2, ylim=c(min(pr.bin.lon.1$Data), max(pr.bin.lon.1$Data)))
axis(1, at=years)
axis(2, at=seq(min(pr.bin.lon.1$Data), max(pr.bin.lon.1$Data), by=1))
box()

# Añadir valores en los puntos del segundo gráfico
for (i in 1:length(pr.bin.lon.1$Data)) {
  text(years[i], pr.bin.lon.1$Data[i], labels=pr.bin.lon.1$Data[i], pos=3, cex=1, col="red")
}

# Tercer gráfico: list.final.lon.2
plot(years, list.final.lon.2$Data, type="l", col="darkgreen", xlab="Year", ylab="Nº Events", 
     main="Compound Events Pr <= P50 & Tmax >= T90 (37.8, -3.75)", lwd=2, ylim=c(min(list.final.lon.2$Data), max(list.final.lon.2$Data)))
axis(1, at=years)
axis(2, at=seq(min(list.final.lon.2$Data), max(list.final.lon.2$Data), by=1))
box()

# Añadir valores en los puntos del tercer gráfico
for (i in 1:length(list.final.lon.2$Data)) {
  text(years[i], list.final.lon.2$Data[i], labels=list.final.lon.2$Data[i], pos=3, cex=1, col="darkgreen")
}

dev.off()
