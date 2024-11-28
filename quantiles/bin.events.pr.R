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
pr.daily <- readRDS("quantiles/pr.daily.hottest.month.pi.1986.2005.rds")
pr.q50 <- readRDS("quantiles/pr.q50.monthly.pi.1986.2005.rds")
pr.monthly <- readRDS("quantiles/pr.monthly.pi.1986.2005.rds")
mask <- readRDS("quantiles/mask.pi.rds")

# Define the months
meses <- 1:12
# Create lists to store the datasets
list.lon <- list()
list.lat <- list()
list.total <- list()
start_date <- NULL
end_date <- NULL

# Loop over the latitude and longitude to select the hottest month. Then compare the monthly precipitation with the 50th percentile 
# of the monthly precipitation for the hottest month. If the monthly precipitation is greater than the 50th percentile, assign 0,
# otherwise assign 1. Finally, combine the results for the region of interest and keep the same structure of the original dataset
for (i in 1:dim(pr.monthly$Data)[which(attr(pr.monthly$Data, "dimensions") == "lat")]) {
    pr.monthly.lat <- subsetDimension(pr.monthly, dimension="lat", indices=i)
    pr.q50.lat <- subsetDimension(pr.q50, dimension="lat", indices=i)
    
    for (j in 1:dim(pr.monthly$Data)[which(attr(pr.monthly$Data, "dimensions") == "lon")]) {
        pr.monthly.lon <- subsetDimension(pr.monthly.lat, dimension="lon", indices=j)
        pr.q50.lon <- subsetDimension(pr.q50.lat, dimension="lon", indices=j)
        month <- hottest.month$Data[,i, j]
        if (is.na(month)) {
            pr.final.lon <- redim(pr.monthly.lon, drop = TRUE)
        } else {
            pr.monthly.lon.no.hot <- subsetGrid(pr.monthly.lon, season=c(setdiff(meses, month)))
            pr.monthly.lon.hot <- subsetGrid(pr.monthly.lon, season=month)
            pr.q50.lon.hot <- subsetGrid(pr.q50.lon, season=month)
            for (k in 1:dim(pr.monthly.lon.hot$Data)[which(attr(pr.monthly.lon.hot$Data, "dimensions") == "time")]) {
                if (pr.monthly.lon.hot$Data[k] > pr.q50.lon.hot$Data) {
                    pr.monthly.lon.hot$Data[k] <- 0
                } else {
                    pr.monthly.lon.hot$Data[k] <- 1
                }
            }
            pr.final.lon <- bindGrid(pr.monthly.lon.no.hot, pr.monthly.lon.hot, dimension = "time")
        }
        
        # Needed to assign the start and end dates to the final dataset
        if (is.null(start_date) && is.null(end_date)) {
            start_date <- pr.monthly.lon$Dates$start
            end_date <- pr.monthly.lon$Dates$end
        }
        
        # Assign the start and end dates to the final dataset
        pr.final.lon$Dates$start <- start_date
        pr.final.lon$Dates$end <- end_date
        
        # Store the results
        list.lon[[j]] <- pr.final.lon
    }
    
    # Combine the results for longitudes
    list.lat[[i]] <- redim(do.call(bindGrid, c(list.lon, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
}

# Combine the results for latitudes
list.total <- redim(do.call(bindGrid, c(list.lat, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

# Save the results
saveRDS(list.total, "quantiles/pr.events.pi.1986.2005.rds", compress="xz")

sum.events <- climatology(list.total, clim.fun = list(FUN = "sum", na.rm = TRUE))
sum.events.mask <- gridArithmetics(sum.events, mask, operator = "*")

# Plot the results
png("quantiles/pr.events.png", width=1200, height=800)
spatialPlot(sum.events.mask, backdrop.theme="coastline", color.theme="YlGnBu", set.min=0.5, set.max=20.5, at=seq(0.5, 20.5, 1),
            main="Number of events Pr <= P50", xlab="Longitude", ylab="Latitude", 
            colorkey = list(space = "right",
                            title = list("Nº Events", cex = 1))
            )
dev.off()

