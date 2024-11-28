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
hottest.month <- readRDS("quantiles/hottest.month.pi.rds")
tmax.daily <- readRDS("quantiles/tmax.daily.hottest.month.pi.1986.2005.rds")
tmax.q90 <- readRDS("quantiles/tmax.q90.monthly.pi.1986.2005.rds")
pr.bin <- readRDS("quantiles/pr.events.pi.1986.2005.rds")
mask <- readRDS("quantiles/mask.pi.rds")

meses <- 1:12
lista.years <- list()
list.lon <- list()
list.lat <- list()
list.total <- list()


# Primer bucle para i y j
for (i in 1:dim(tmax.daily$Data)[which(attr(tmax.daily$Data, "dimensions") == "lat")]) {
    print(paste("i= ", i))
    tmax.daily.lat <- subsetDimension(tmax.daily, dimension="lat", indices=i)
    tmax.q90.lat <- subsetDimension(tmax.q90, dimension="lat", indices=i)
    pr.bin.lat <- subsetDimension(pr.bin, dimension="lat", indices=i)
    
    for (j in 1:dim(tmax.daily$Data)[which(attr(tmax.daily$Data, "dimensions") == "lon")]) {
        print(paste("j= ", j))
        tmax.daily.lon <- subsetDimension(tmax.daily.lat, dimension="lon", indices=j)
        tmax.q90.lon <- subsetDimension(tmax.q90.lat, dimension="lon", indices=j)
        pr.bin.lon <- subsetDimension(pr.bin.lat, dimension="lon", indices=j)
        month <- hottest.month$Data[,i, j]
        # print(paste("month= ", month))
        if (is.na(month)) {
            tmax.final.lon <- redim(tmax.daily.lon, drop = TRUE)
        } else {
            pr.bin.lon.hot <- subsetGrid(pr.bin.lon, season=month)
            tmax.daily.lon.no.hot <- subsetGrid(tmax.daily.lon, season=c(setdiff(meses, month)))
            tmax.daily.lon.hot <- subsetGrid(tmax.daily.lon, season=month)
            tmax.q90.lon.hot <- subsetGrid(tmax.q90.lon, season=month)

            for (k in 1:dim(pr.bin.lon.hot$Data)[which(attr(pr.bin.lon.hot$Data, "dimensions") == "time")]) {
                # print(paste("k= ", k))
                date <- as.Date(pr.bin.lon.hot$Dates$start[k], format="%Y-%m-%d %H:%M:%S GMT")
                year <- as.numeric(format(date, "%Y"))
                # print(paste("year= ", year))
                tmax.daily.lon.hot.year <- subsetGrid(tmax.daily.lon.hot, years=year)
                if (pr.bin.lon.hot$Data[k] == 0) {
                    tmax.daily.lon.hot.year$Data[] <- NA
                } else {
                    for (t in 1:dim(tmax.daily.lon.hot.year$Data)[which(attr(tmax.daily.lon.hot.year$Data, "dimensions") == "time")]) {
                        # print(paste("t= ", t))
                        if (tmax.daily.lon.hot.year$Data[t] < as.numeric(tmax.q90.lon.hot$Data)) {
                            tmax.daily.lon.hot.year$Data[t] <- 0
                        } else {
                            tmax.daily.lon.hot.year$Data[t] <- 1
                        }
                    }
                }
                lista.years[[k]] <- tmax.daily.lon.hot.year
            }
            tmax.daily.lon.hottest <- do.call(bindGrid, c(lista.years, list(dimension = "time", skip.temporal.check = TRUE)))
            tmax.final.lon <- bindGrid(tmax.daily.lon.no.hot, tmax.daily.lon.hottest, dimension = "time")
        }
        
        # Guardar en list.lon
        list.lon[[j]] <- tmax.final.lon
    }
    
    # Combinar los resultados para latitudes
    list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
}

# Combinar los resultados para todas las latitudes
list.total <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

saveRDS(list.total, "quantiles/tmax.events.pi.1986.2005.rds", compress="xz")

sum.events <- climatology(list.total, clim.fun = list(FUN = "sum", na.rm = TRUE))
sum.events.mask <- gridArithmetics(sum.events, mask, operator="*")

# Plot the results
png("quantiles/ce.events.png", width=1200, height=800)
spatialPlot(sum.events.mask, backdrop.theme="coastline", color.theme="YlOrRd", set.min=0.5, set.max=65.5, at=seq(0.5, 65.5, 5),
            main="Number of compound events Pr <= P50 and Tmax => T90", xlab="Longitude", ylab="Latitude", 
            colorkey = list(space = "right",
                            title = list("Nº Events", cex = 1))
            )
dev.off()

# Plot the results for a gridbox
gridbox <- subsetDimension(list.total, dimension="lat", indices=25)
gridbox <- subsetDimension(gridbox, dimension="lon", indices=47)
gridbox.acc.m <- aggregateGrid(gridbox, aggr.y = list(FUN = "sum", na.rm = TRUE))
serietemporal <- gridbox.acc.m$Data
years <- 1986:2005

png("quantiles/ce.events.gridbox2.png", width=1200, height=800)
plot(years, serietemporal, type="l", col="blue", lwd=2, axes =FALSE, xlab="Time / Years", ylab="Frecuency / Days", main="Number of compound events Pr <= P50 and Tmax => T90. (42, 2)")
axis(1, at=seq(1986, 2005, 1))
axis(2, at=seq(min(serietemporal), max(serietemporal), 1))
dev.off()
