fun.trends.frecuency <- function(ce, hottest.month, mask) {
    # Output: list of the trend and each significance
    source("/oceano/gmeteo/users/fuentesm/ERA5/fun.aux.trends.R")
    
    lon.y <- list()
    lat.y <- list()
    last.y <- list()
    start_date <- NULL
    end_date <- NULL

    for (i in 1:dim(ce$Data)[which(attr(ce$Data, "dimensions") == "lat")]) {
        ce.lat <- subsetDimension(ce, dimension="lat", indices=i)
        ce.lat.15 <- subsetDimension(ce, dimension="lat", indices=15)
        for (j in 1:dim(ce$Data)[which(attr(ce$Data, "dimensions") == "lon")]) {
            ce.lon.15 <- subsetDimension(ce.lat.15, dimension="lon", indices=15)
            ce.lon <- subsetDimension(ce.lat, dimension="lon", indices=j)
            month <- hottest.month$Data[i,j]
            if(is.na(month)){
                ce.lon$Data <- array(NA, dim = c(620, 1, 1))
                attr(ce.lon$Data, "dimensions") <- c("time", "lat", "lon")

            } else{
            ce.lon <- subsetGrid(ce.lon, season=month)
            ce.lon$Data <- array(ce.lon$Data, dim = c(620, 1, 1))
            attr(ce.lon$Data, "dimensions") <- c("time", "lat", "lon")
            }
            # Needed to assign the start and end dates to the final dataset
            if (is.null(start_date) && is.null(end_date)) {
                ce.lat.15 <- subsetDimension(ce, dimension="lat", indices=15)
                ce.lon.15 <- subsetDimension(ce.lat.15, dimension="lon", indices=15)
                month.15 <- hottest.month$Data[15,15]
                ce.lon.15 <- subsetGrid(ce.lon.15, season=month.15)
                start_date <- ce.lon.15$Dates$start
                end_date <- ce.lon.15$Dates$end
            }
            
            # Assign the start and end dates to the final dataset
            ce.lon$Dates$start <- start_date
            ce.lon$Dates$end <- end_date

            lon.y[[j]] <- ce.lon
        }
        lat.y[[i]] <- redim(do.call(bindGrid, c(lon.y, list(dimension = "lon", skip.temporal.check = TRUE))), drop = TRUE)
    }
    last.y <-  redim(do.call(bindGrid, c(lat.y, list(dimension = "lat", skip.temporal.check = TRUE))), drop=TRUE)

    
    ce.hot <- last.y
    ce.hot.mm <- aggregateGrid(ce.hot, aggr.m = list(FUN = "sum", na.rm = TRUE))


    trend <- climatology(ce.hot.mm, clim.fun = list(FUN = "computeTrend"))
    trend <- gridArithmetics(trend, mask, operator = "*")

    sig <- map.stippling(clim = climatology(ce.hot.mm, clim.fun = list(FUN = "computeSigTrend")), 
                        threshold = 0.05, condition = "LT", 
                        pch = 19, cex = .3, col = "black")  # points exhibiting significant trends (at a 95% confidence level)

    return(list(trend = trend, sig = sig))
}