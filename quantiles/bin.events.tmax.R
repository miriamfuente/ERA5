options(java.parameters = "-Xmx32g") 
# Libraries needed for the script
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

# Plot the hottest month in the region of interest
png("quantiles/hottest.month.pi.png", width=800, height=800)
spatialPlot(hottest.month, backdrop.theme="coastline", color.theme="YlOrRd", set.min=0.5, set.max=12.5, at=seq(0.5, 12.5, 1),
            main="Hottest month", xlab="Longitude", ylab="Latitude", 
            colorkey = list(space = "right",
                            title = list("Month", cex = 1))
            )
dev.off()


# Define the months
meses <- 1:12
# Create lists to store the datasets
list.lon <- list()
list.lat <- list()
list.total <- list()


# Loop over the latitude and longitude to select the hottest month. Then compare the daily maximum temperature with the 90th percentile 
# of the daily tmax for the hottest month. If the daily tmax is greater than the 50th percentile, assign 0,
# otherwise assign 1. Finally, combine the results for the region of interest and keep the same structure of the original dataset
for (i in 1:dim(tmax.daily$Data)[which(attr(tmax.daily$Data, "dimensions") == "lat")]) {
    print(paste("i= ", i))
    tmax.daily.lat <- subsetDimension(tmax.daily, dimension="lat", indices=i)
    tmax.q90.lat <- subsetDimension(tmax.q90, dimension="lat", indices=i)
    
    for (j in 1:dim(tmax.daily$Data)[which(attr(tmax.daily$Data, "dimensions") == "lon")]) {
        print(paste("j= ", j))
        tmax.daily.lon <- subsetDimension(tmax.daily.lat, dimension="lon", indices=j)
        tmax.q90.lon <- subsetDimension(tmax.q90.lat, dimension="lon", indices=j)
        month <- hottest.month$Data[,i, j]
        # print(paste("month= ", month))
        if (is.na(month)) {
            tmax.final.lon <- redim(tmax.daily.lon, drop = TRUE)
        } else {
            tmax.daily.lon.no.hot <- subsetGrid(tmax.daily.lon, season=c(setdiff(meses, month)))
            tmax.daily.lon.hot <- subsetGrid(tmax.daily.lon, season=month)
            tmax.q90.lon.hot <- subsetGrid(tmax.q90.lon, season=month)

            for (k in 1:dim(tmax.daily.lon.hot$Data)[which(attr(tmax.daily.lon.hot$Data, "dimensions") == "time")]) {
                if (tmax.daily.lon.hot$Data[k] < tmax.q90.lon.hot$Data) {
                    tmax.daily.lon.hot$Data[k] <- 0
                } else {
                    tmax.daily.lon.hot$Data[k] <- 1
                }
            }
            tmax.final.lon <- bindGrid(tmax.daily.lon.no.hot, tmax.daily.lon.hot, dimension = "time")
            }
        
        # Store the results
        list.lon[[j]] <- tmax.final.lon
    }
    
    # Combine the results for longitudes
    list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
}

# Combine the results for latitudes
list.total <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

# Save the results
saveRDS(list.total, "quantiles/tmax.events.pi.1986.2005.rds", compress="xz")

sum.events <- climatology(list.total, clim.fun = list(FUN = "sum", na.rm = TRUE))
sum.events.mask <- gridArithmetics(sum.events, mask, operator="*")

# Plot the results
png("quantiles/tmax.events.png", width=1200, height=800)
spatialPlot(sum.events.mask, backdrop.theme="coastline", color.theme="YlOrRd", set.min=58.5, set.max=63.5, at=seq(58.5, 63.5, 1),
            main="Number of events Tmax => T90", xlab="Longitude", ylab="Latitude", 
            colorkey = list(space = "right",
                            title = list("Nº Events", cex = 1))
            )
dev.off()


